#!/bin/bash
#SBATCH --job-name=filter
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=28
#SBATCH --time=01:00:00
#SBATCH --mem=60gb
#SBATCH -o ./log/2.1_filter.%J.out
#SBATCH -e ./log/2.1_filter.%J.err

echo started at `date`
processed=/work/gr-fe/username/case_gwas/data/processed

#==============================================================================
# Filter PLINK files for analysis (MAF, Geno, HWE)
#==============================================================================

plink \
	--bfile $processed/combined/geno/cohort \
	--maf 0.01 \
	--geno 0.1 \
	--make-bed \
	--allow-no-sex \
	--out $processed/combined/geno/cohort.filt \
	--thread-num 28

echo ended at `date`
