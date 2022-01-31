#!/bin/bash

#install miniconda
# https://docs.conda.io/projects/conda/en/latest/user-guide/install/linux.html#install-linux-silent

# add to path
# source path

# isntall vcflib with conda
# conda install -c bioconda vcflib
# /home/lawless/miniconda3/bin/conda install -c bioconda vcf
# conda install -c bioconda vcflib

data=/work/gr-fe/lawless/database/dbNSFP/toy/data
file=test_sample_vep
cd $data

# Rename samples 
vcf2tsv \
	$file\.vcf  > $file\.vcflib.tsv

