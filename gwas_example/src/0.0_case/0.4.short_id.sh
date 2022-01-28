#!/bin/bash
#SBATCH --job-name=0.4.short
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=28
#SBATCH --time=02:00:00
#SBATCH --mem=64gb
#SBATCH -o ./log/0.4.short.%J.out
#SBATCH -e ./log/0.4.short.%J.err

#==============================================================================
# Long SNP IDs do not scale well in Plink1.9
# Therefore we must replace them with shorter ones
# If a snp id is >80, replace with a short temp id
# Keep a copy of original in case it is needed later 
#==============================================================================

scratch_raw=/scratch/username/case_gwas/data/raw
cd $scratch_raw/archive.CASE.data

for file in CASE*bim
	do
		mv $file $file\_original &&
		cat $file\_original | awk '{ if (length($2) >50) {$2 = $1"_"$4"_tmp"; print} else { print }; }' | sed 's/ /\t/g' > $file &
	done
wait
