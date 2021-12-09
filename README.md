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
2. `cd example && ldsc run --config config.dhall`