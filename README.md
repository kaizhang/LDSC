# LD Score Regression (LDSC)

This is a computational pipeline to perform the LD Score Regression (LDSC) analysis.
For more information, see here: https://github.com/bulik/ldsc.

If you use this pipeline in your paper, 
Please cit the original publications: https://github.com/bulik/ldsc#citation

## Installation

**Prerequisite: install the `ldsc` python program first by following [these instructions](https://github.com/bulik/ldsc#getting-started).**

To install this pipeline, download pre-built binaries for macOS or Linux systems:

- `ldsc-CentOS-x86_64`: for Red Hat Enterprise Linux derivatives.

- `ldsc-Ubuntu-x86_64`: for Debian linux derivatives.

- `ldsc-macOS-XX-XX`: for macOS.

Example:

```
curl -L https://github.com/kaizhang/LDSC/releases/latest/download/ldsc-CentOS-x86_64 -o ldsc
chmod +x ldsc
./ldsc --help
```

## Use the pipeline

To run this pipeline, you need to prepare the following files:

- Annotaton files containing genome regions in BED format.
- GWAS summary statistics.
- plink files
- A list of SNPs
- LD scores
- LD score weights

As an example, we will run the pipeline for GRCh38 genome and European acestry:

1. Download all files listed here: http://renlab.sdsc.edu/kai/LDSC_hg38/
2. Go to the example directory: `cd example`.
3. Modify the `ldsc_program_path` in the configuration file `config.dhall`. 
4. Run the pipeline: `ldsc run --config config.dhall`

The states are stored in a file called `sciflow.db`, which is used to resume the program at the last checkpoint.
The results are in the `output` directory.

The running time depends on the number of annotations and GWAS summary statistics.
You can use parallel computing to speed up the analysis.

To use parallelism in one computer: `ldsc run --config config.dhall -n 10 +RTS -N10`.
The number 10 here is the number of parallel jobs.

To use the pipeline to in a grid computing system, adjust the relevant settings in `config.dhall`,
and run it using: `ldsc run --config config.dhall -n 10 --cloud`.
The default setting is configured for the TSCC environment at UCSD.