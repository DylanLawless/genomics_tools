#!/bin/bash

#install miniconda
# https://docs.conda.io/projects/conda/en/latest/user-guide/install/linux.html#install-linux-silent

# add to path
# source path

# isntall vcflib with conda
# conda install -c bioconda vcflib
# /home/lawless/miniconda3/bin/conda install -c bioconda vcf

# conda install -c bioconda vcflib
# Comment: this script will split multi allelic sites

# data=/work/gr-fe/progg/pathways/raw_data
# cd $data
file=pri.leeds.hiv.combined.genotype.ts99pt9.1pcdbsnp.1pcEVS.vep.rename

# Rename samples 
vcfbreakmulti \
	./$file.vcf  > ./$file.multi.vcf

