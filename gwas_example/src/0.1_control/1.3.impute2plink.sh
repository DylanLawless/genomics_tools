#!/bin/bash
#SBATCH --job-name=1.3.makebed
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=22
#SBATCH --time=03:00:00
#SBATCH --mem=64gb
#SBATCH -o ./log/1.3.makebed.%J.out
#SBATCH -e ./log/1.3.makebed.%J.err

#==============================================================================
# This script will use Plink2 to convert impute2 gen file to plink format
# Cohort takes <1hr on 12 cores
# Run sequently to keep memory within limit, but parallel chr should be fine
#==============================================================================

raw=/work/gr-fe/username/case_gwas/data/raw/
scratch_raw=/scratch/username/case_gwas/data/raw

cd $scratch_raw/archive.CONTROL.data

# Convert gen to bfile in parallel
for i in {1..22}
do
	for file in S*chr$i.*impute2
	do
	    /work/gr-fe/username/tool/plink2 \
	    --gen $file ref-first \
	    --sample $raw/CONTROL_samples \
	    --make-bed \
	    --out $file\_plink2
	done
done &
wait
