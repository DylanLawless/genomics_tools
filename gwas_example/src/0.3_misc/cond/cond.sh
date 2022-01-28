#!/bin/bash
#SBATCH --job-name=kpneuomonia
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=12
#SBATCH --time=01:00:00
#SBATCH --mem=30gb
#SBATCH -o ./log/cond.%J.out
#SBATCH -e ./log/cond.%J.err


processed=/work/gr-fe/lawless/spss_gwas/data/processed

# format test.ma
# Chr     SNP     bp      A1      A2      Freq    b       se      p
# SNP A1 A2 freq b se p N 

# get columns
# 2 3 4 5 6 7 8 sample size

# cut -f 2,4-9 \
# 	/work/gr-fe/lawless/spss_gwas/data/processed/combined/geno/cohort.filt.group.case_control.loco.test.mlma \
# 	| sed -e 's/$/ 1504/' > test.ma

# Fix the header "N"

# gcta64  \
#     --bfile  $processed/combined/geno/cohort.filt.group \
# 	--cojo-file test.ma \
# 	--cojo-cond cond.snplist \
# 	--out cond


gcta64  \
    --bfile  $processed/combined/geno/cohort.filt.group \
	--cojo-file test.ma \
	--cojo-slct \
	--chr 9 \
	--cojo-cond cond.snplist \
	--out cond3

