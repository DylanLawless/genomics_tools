#!/bin/bash
#SBATCH --job-name=plinkmerge
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=28
#SBATCH --time=01:00:00
#SBATCH --mem=24gb
#SBATCH -o ./log/2.0_plinkmerge.%J.out
#SBATCH -e ./log/2.0_plinkmerge.%J.err

echo started at `date`

#==============================================================================
# Megrge two cohorts
#==============================================================================

# list of prepared dataset of case/controls
processed=/work/gr-fe/username/case_gwas/data/processed
case=$processed/geno/CASE.QC.impute2_plink2.allfiles.king_mind1_maf01_geno2.info8
control=$processed/geno/CONTROL.QC.impute2_plink2.allfiles_mind1_maf01_geno2_hwe6.info8

printf "Merging case and control cohorts\n"

plink \
    --bfile $case \
    --bmerge $control.bed $control.bim $control.fam \
    --make-bed \
    --out $processed/combined/geno/cohort

echo ended at `date`

