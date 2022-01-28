#!/bin/bash
#SBATCH --job-name=0.1.replace_gen_chr
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=22
#SBATCH --time=01:00:00
#SBATCH --mem=30gb
#SBATCH -o ./log/0.1.replace_gen_chr.%J.out
#SBATCH -e ./log/0.1.replace_gen_chr.%J.err

#==============================================================================
# The chromosome colums have no value set.
# This script will set the chromosome number accordingly based on file name.
#==============================================================================

echo started at `date`

scratch_raw=/scratch/username/case_gwas/data/raw
cd $scratch_raw/archive.CASE.data/

# Replace lines that start with "---" with the correct chromosome number
# Run in parallel
for i in {1..22}
    do
        sed -i "s/^---/$i/g" CASE.QC_chr$i.pos*impute2 &&
        echo finished $i >> finished_chr_columns &
done

wait

echo "all done" >> finished_chr_columns
