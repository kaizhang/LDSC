{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where


import           Data.Version           (showVersion)
import Data.Yaml (decodeFileThrow, Value(..))
import           Control.Monad.Reader              (asks, ReaderT)
import           Data.Default                         (def)
import qualified Data.HashMap.Strict as M
import Control.Workflow.Coordinator.Remote (Remote, RemoteConfig(..), getDefaultRemoteConfig)
import           Control.Workflow
import Control.Workflow.Main
import Conduit
import Data.Proxy (Proxy(..))
import Bio.Data.Bed
import Data.Maybe
import qualified Data.Text as T
import Text.Printf (printf)
import Data.List
import Data.Ord
import Data.Function (on)
import Lens.Micro.Platform

import Paths_LDSC (version)
import LDSC
import Main.Functions

-- Construct workflow
build "wf" [t| SciFlow LDSCConfig |] $ do
    uNode "Create_Plink_Files_Prep" [| \() -> asks plink_files >>= \case
        Nothing -> map Left . fromMaybe (error "Either Plink files or VCF files need to be provided") <$>
            asks variant_files
        Just pl -> return $ map Right pl
        |]
    nodePar "Create_Plink_Files" [| \x -> createPlinkFiles x Nothing |] $ return ()
    uNode "Make_Annotation_Prep" [| \plinks -> do
        annoFls <- asks annotations
        return $ flip map annoFls $ \anno -> (annotation_name anno, plinks, [anno])
      |]
    nodePar "Make_Annotation" 'makeAnnotation $ nCore .= 8
    nodePar "Compute_LD_Score" 'computePartitionedLDScore $ nCore .= 8
    path ["Create_Plink_Files_Prep", "Create_Plink_Files", "Make_Annotation_Prep", "Make_Annotation", "Compute_LD_Score"]

    node "Merge_Annotation" [| \plinks -> do
        annoFls <- asks annotations
        dir <- asks output_dir
        let output = dir <> "/merged_annotation.bed.gz"
        liftIO $ do
            regions <- runResourceT $ runConduit $
                mapM_ (streamBedGzip . annotation_file) annoFls .| sinkList :: IO [BED3]
            runResourceT $ runConduit $ mergeBed regions .| sinkFileBedGzip output
            let anno = Annotation "Control" output
            return ("Control", plinks, [anno])
        |] $ return ()
    node "Make_Control_Annotation" 'makeAnnotation $ nCore .= 8
    node "Compute_Control_LD_Score" 'computePartitionedLDScore $ nCore .= 8
    path ["Create_Plink_Files", "Merge_Annotation", "Make_Control_Annotation", "Compute_Control_LD_Score"]

    uNode "Regression_Prep" [| \(control, targets) -> do
        sumstats <- asks summary_statistics
        let control' = getPath $ snd $ head $ snd control
            targets' = map (\x -> (fst x, getPath $ snd $ head $ snd x)) targets
            getPath = T.unpack . fst . T.breakOnEnd "/" . T.pack
        return $ flip map sumstats $ \s -> (s, control', targets')
        |]
    nodePar "Regression" 'ldRegress $ nCore .= 8
    node "Save_Result" 'outputResult $ return ()
    ["Compute_Control_LD_Score", "Compute_LD_Score"] ~> "Regression_Prep"
    path ["Regression_Prep", "Regression", "Save_Result"]

getCoordConfig :: String -> Int -> FilePath -> IO RemoteConfig
getCoordConfig ip port fl = do
    config <- getDefaultRemoteConfig ["remote", "--ip", ip, "--port", show port]
    settings <- decodeFileThrow fl :: IO (M.HashMap String Value)
    return config
        { _remote_parameters = str <$> M.lookup "submit_params" settings
        , _submission_cmd = str $ M.lookupDefault "sbatch" "submit_command" settings
        , _cpu_format = str $ M.lookupDefault "--ntasks-per-node=%d" "submit_cpu_format" settings
        , _memory_format = str $ M.lookupDefault "--mem=%d000" "submit_memory_format" settings
        }
  where
    str (String x) = T.unpack x
    str _ = error "Expecting string"


main :: IO ()
main = defaultMain header descr commands wf
  where
    header = printf "LDSC-v%s" $ showVersion version
    descr = "LDSC."
    commands =
        [ runParser getCoordConfig
        , deleteParser
        , showParser
        , viewParser
        , remoteParser (Proxy :: Proxy Remote) ]

