#!/bin/bash
#SBATCH --job-name=filter
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=28
#SBATCH --time=02:00:00
#SBATCH --mem=34gb
#SBATCH -o ./log/1.6.filter.%J.out
#SBATCH -e ./log/1.6.filter.%J.err

processed=/work/gr-fe/username/case_gwas/data/processed

#==============================================================================
# KING check for relatedness
#==============================================================================

# printf "Checking relatedness with KING"
# 
# cd $processed/geno/
# /work/gr-fe/username/tool/king \
# 	-b $processed/geno/CONTROL.QC.impute2_plink2.allfiles.bed \
# 	--kinship \
# 	--degree 2 \
# 	--cpus 12
# 
# printf "Finished checking relatedness with KING\n"

#==============================================================================
# Remove unwanted samples
# No relatives to be removed
#==============================================================================

#==============================================================================
# Filter
# check and remove missingness
# exclude individuals with too much missing genotype data.
# --mind 0.1 = exclude with more than 10% missing genotypes (default level)
# --maf 0.01 --geno 0.2 loose thresholds for first pass, running another 
# more strick QC after merging cohorts to the default level, geno 0.1
# Controls also applied with hwe 1x10^(-6) = 0.000001
#==============================================================================

# plink2 \
# 	--bfile $processed/geno/CONTROL.QC.impute2_plink2.allfiles \
# 	--mind 0.1 --maf 0.01 --geno 0.2 --hwe 0.000001 --make-bed --allow-no-sex \
# 	--out $processed/geno/CONTROL.QC.impute2_plink2.allfiles_mind1_maf01_geno2_hwe6

