#!/bin/bash
#SBATCH --job-name=gcta
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=28
#SBATCH --time=02:00:00
#SBATCH --mem=120gb
#SBATCH -o ./log/2.6_gcta.%J.out
#SBATCH -e ./log/2.6_gcta.%J.err

echo started at `date`

processed=/work/gr-fe/username/case_gwas/data/processed

#==============================================================================
#  GCTA
#==============================================================================

gcta64 \
    --bfile  $processed/combined/geno/cohort.filt.group \
    --mlma-loco \
    --qcovar $processed/combined/geno/cohort.filt.group.pca.eigenvec.8pc \
    --covar  $processed/combined/geno/cohort.filt.group.sex \
    --grm    $processed/combined/grm/cohort.filt.group.grm \
    --pheno  $processed/combined/geno/cohort.filt.group.pheno \
    --out    $processed/combined/geno/cohort.filt.group.case_control \
    --thread-num 28
 
# # # have a look at the QQ and Manhattan plots in manhat_plot

