{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards #-}

module LDSC
    ( createPlinkFiles
    , makeAnnotation
    , computePartitionedLDScore
    , ldRegress
    , outputResult
    , LDSCConfig(..)
    , VariantFile(..)
    , Annotation(..)
    , SumStats(..)
    , LDSCResult(..)
    ) where

import Shelly
import           Control.Monad.Reader              (asks, ReaderT)
import           Data.Aeson
import           Data.Binary (Binary(..))
import           GHC.Generics           (Generic)
import Conduit
import Data.Conduit.Zlib (gzip)
import Bio.Data.Bed
import Data.Maybe
import Control.Monad
import Bio.Utils.Misc (readInt, readDouble)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Control.Workflow
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MSem
import Text.Printf (printf)
import System.IO.Temp (withSystemTempDirectory)

forPool :: Int -> [a] -> (a -> IO b) -> IO [b]
forPool max xs f = do
    sem <- new max
    mapConcurrently (with sem . f) xs

data LDSCConfig = LDSCConfig
    { ldsc_python_path :: FilePath
    , variant_files :: Maybe [VariantFile]
    , plink_files :: Maybe [PlinkFile]
    , baseline_ldscore :: FilePath
    , weights :: FilePath
    , snp_files :: [SNPSet]
    , output_dir :: FilePath
    , annotations :: [Annotation]
    , summary_statistics :: [SumStats]
    } deriving (Generic)

instance Binary LDSCConfig
instance ToJSON LDSCConfig 
instance FromJSON LDSCConfig 

data SNPSet = SNPSet
    { snp_chromosome :: String
    , snp_file :: FilePath
    } deriving (Generic, Show)

instance Binary SNPSet
instance ToJSON SNPSet
instance FromJSON SNPSet

data SumStats = SumStats
    { trait :: String
    , trait_file :: FilePath
    } deriving (Generic, Show)

instance Binary SumStats
instance ToJSON SumStats
instance FromJSON SumStats

data VariantFile = VariantFile
    { label :: String
    , vcf_file :: FilePath
    , cm_map :: FilePath
    } deriving (Generic, Show)

instance Binary VariantFile
instance ToJSON VariantFile
instance FromJSON VariantFile

data Annotation = Annotation
    { annotation_name :: String
    , annotation_file :: FilePath
    } deriving (Generic, Show)

instance Binary Annotation
instance ToJSON Annotation
instance FromJSON Annotation

data PlinkFile = PlinkFile
    { chromosome :: String
    , prefix :: FilePath 
    } deriving (Generic, Show)

instance Binary PlinkFile
instance ToJSON PlinkFile
instance FromJSON PlinkFile

data LDSCResult = LDSCResult
    { trait_name :: String
    , annot_name :: String
    , coefficient :: Double
    , coefficient_std_error :: Double
    , coefficient_p_value :: Double
    } deriving (Generic, Show)

instance Binary LDSCResult
instance ToJSON LDSCResult
instance FromJSON LDSCResult

createPlinkFiles :: Either VariantFile PlinkFile
                 -> Maybe [String]  -- entries to use
                 -> ReaderT LDSCConfig IO PlinkFile
createPlinkFiles (Right fl) _ = return fl
createPlinkFiles (Left fl) keep = do
    dir <- asks $ (<> "/plink/") . output_dir
    liftIO $ do
        let l = label fl
        shelly $ withTmpDir $ \tmp -> do
            mkdir_p dir
            extra <- case keep of
                Nothing -> return []
                Just k -> do
                    liftIO $ writeFile (tmp <> "/tmp") $ unlines k
                    return ["--keep", T.pack $ tmp <> "/tmp"]
            run_ "plink" $
                [ "--vcf", T.pack $ vcf_file fl
                , "--cm-map", T.pack $ cm_map fl, T.pack l
                , "--make-bed"
                , "--out", T.pack $ dir <> l] <> extra
        return $ PlinkFile l (dir <> l)


-- TODO: remove this step
makeAnnotation :: (String, [PlinkFile], [Annotation]) -> ReaderT LDSCConfig IO (String, [(PlinkFile, FilePath)])
makeAnnotation (outPrefix, plinks, annoFls) = do
    dir <- (<> ("/ld_scores/" <> outPrefix)) <$> asks output_dir
    liftIO $ shelly $ mkdir_p dir
    res <- liftIO $ forPool 8 plinks $ \pl@PlinkFile{..} -> do
        let output = printf "%s/%s.annot.gz" dir chromosome
            chr = B.pack $ "chr" <> chromosome
        regions <- forM annoFls $ \Annotation{..} -> do
            peaks <- runResourceT $ runConduit $ streamBedGzip annotation_file .|
                filterC (\x -> _bed3_chrom x == chr) .| sinkList
            return $ bedToTree const $ zip peaks $ repeat ()
        let findIntersect xs = B.intercalate "\t" $ flip map regions $ \b ->
                if isIntersected b bed then "1" else "0"
              where
                bed = let i = readInt $ xs!!3 in BED3 ("chr" <> (xs!!0)) (i - 1) i
            header = B.intercalate "\t" $ map (B.pack . annotation_name) annoFls
        runResourceT $ runConduit $ sourceFile (prefix <> ".bim") .|
            linesUnboundedAsciiC .|
            (yield header >> mapC (findIntersect . B.words)) .|
            unlinesAsciiC .| gzip .| sinkFile output
        return (pl, output)
    return (outPrefix, res)

{-
computeLDScore :: PlinkFile -> ReaderT LDSCConfig IO (String, FilePath)
computeLDScore plink = do
-}

computePartitionedLDScore :: (String, [(PlinkFile, FilePath)]) -> ReaderT LDSCConfig IO (String, [(String, FilePath)])
computePartitionedLDScore (outPrefix, inputs) = do
    dir <- (<> ("/ld_scores/" <> outPrefix)) <$> asks output_dir
    liftIO $ shelly $ mkdir_p dir
    exe <- asks ldsc_python_path
    snp <- map (\x -> (snp_chromosome x, snp_file x)) <$> asks snp_files
    res <- liftIO $ forPool 8 inputs $ \(PlinkFile{..}, anno) -> do
        let snpFl = fromMaybe (error "No snp file") $ lookup chromosome snp
            prefix' = printf "%s/%s" dir chromosome
        shelly $ run_ "python" $ map T.pack
            [ exe <> "/ldsc.py"
            , "--l2", "--bfile", prefix, "--ld-wind-cm", "1"
            , "--annot", anno, "--thin-annot"
            , "--print-snps", snpFl
            , "--out", prefix' ]
        return (chromosome, prefix')
    return (outPrefix, res)

ldRegress :: (SumStats, FilePath, [(String, FilePath)]) -> ReaderT LDSCConfig IO [LDSCResult]
ldRegress (SumStats{..}, control, targets) = do
    exe <- asks ldsc_python_path
    baseline <- asks baseline_ldscore
    ws <- asks weights
    liftIO $ forPool 8 targets $ \(t, p) -> withSystemTempDirectory "tmp.ldsc" $ \tmpDir -> do
        let ldcts = tmpDir <> "/tmp.ldcts"
            output = tmpDir <> "/result"
        writeFile ldcts $ concat [t, "\t", p, ",", control]
        shelly $ run_ "python" $ map T.pack
            [ exe <> "/ldsc.py"
            , "--h2-cts", trait_file
            , "--ref-ld-chr", baseline
            , "--w-ld-chr", ws
            , "--ref-ld-chr-cts", ldcts
            , "--out", output ]
        let f [a,b,c,d] = LDSCResult trait (B.unpack a) (readDouble b)
                (readDouble c) (readDouble d)
        f . B.split '\t' . last . B.lines <$>
            B.readFile (output <> ".cell_type_results.txt")

outputResult :: [[LDSCResult]] -> ReaderT LDSCConfig IO ()
outputResult inputs = do
    dir <- (<> "/Result/") <$> asks output_dir
    let output1 = dir <> "coefficients.tsv"
        output2 = dir <> "pvalues.tsv"
    liftIO $ do
        shelly $ mkdir_p dir
        let rownames = map (trait_name . head) inputs
            colnames = map annot_name $ head inputs
            (dat1, dat2) = unzip $ flip map inputs $ \input -> unzip $
                flip map input $ \LDSCResult{..} -> (coefficient, coefficient_p_value)
        writeData output1 rownames colnames dat1
        writeData output2 rownames colnames dat2
  where
    writeData out r c d = B.writeFile out $ B.unlines $
        map (B.intercalate "\t" . map B.pack) $ ("" : c) : zipWith f r d
      where
        f x y = x : map show y
        