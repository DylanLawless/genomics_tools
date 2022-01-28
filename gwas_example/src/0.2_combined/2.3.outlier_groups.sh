#!/bin/bash
#SBATCH --job-name=outpier_groups
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=28
#SBATCH --time=01:00:00
#SBATCH --mem=60gb
#SBATCH -o ./log/2.3_outlier.%J.out
#SBATCH -e ./log/2.3_outlier.%J.err

echo started at `date`
processed=/work/gr-fe/username/case_gwas/data/processed

#==============================================================================
# Remove non_EU groups that we can't match up well between 2 cohorts
#==============================================================================

plink \
	--bfile $processed/combined/geno/cohort.filt \
	--remove $processed/combined/geno/merged_non_EU \
	--make-bed \
	--out $processed/combined/geno/cohort.filt.group \
	--thread-num 28

echo ended at `date`
