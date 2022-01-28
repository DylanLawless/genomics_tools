#!/bin/bash
#SBATCH --job-name=0.unzip
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=28
#SBATCH --time=02:00:00
#SBATCH --mem=20gb
#SBATCH -o ./log/0.0.unzip.%J.out
#SBATCH -e ./log/0.0.unzip.%J.err

echo started at `date`

#==============================================================================
# This script will set up directories on scratch for intermediate work
# Then copy data from raw to scratch
# Unzip all files
#==============================================================================

# Define path to scratch data
scratch_raw=/scratch/username/case_gwas/data/raw
mkdir -p  $scratch_raw/archive.CASE.data

# Define path to raw data
raw=/work/gr-fe/username/case_gwas/data/raw/
cd $raw/archive.CASE.data

# Sync raw to scratch in parallel
for i in {1..22}
    do
		rsync -avz -P CASE.QC_chr$i.pos*impute2.gz \
		$scratch_raw/archive.CASE.data/ &
     done
wait

# Unzip all in parallel
cd $scratch_raw/archive.CASE.data/
for i in {1..22}
    do
        gunzip CASE.QC_chr$i*impute2.gz &
    done
wait

echo ended at `date`
