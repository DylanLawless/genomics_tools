#!/bin/bash
#SBATCH --job-name=0.2.info
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=22
#SBATCH --time=08:00:00
#SBATCH --mem=64gb
#SBATCH -o ./log/0.2.info.%J.out
#SBATCH -e ./log/0.2.info.%J.err

#==============================================================================
# This script will get SNPTEST information scores
# https://mathgen.stats.ox.ac.uk/genetics_software/snptest/snptest.html#info_measures
# Input files are 1-2GB so 64GB memory should be enough
# The input data is (1) the gen file and (2) the sample list
#==============================================================================

scratch_raw=/scratch/username/case_gwas/data/raw
raw=/work/gr-fe/username/case_gwas/data/raw
processed=/work/gr-fe/username/case_gwas/data/processed

# Make dir for longterm storage
# made in script 0.2: $processed/snptest
mkdir $processed/snptest/control_snptest/
snptest=$processed/snptest/control_snptest/

cd $scratch_raw/archive.CONTROL.data

# Get snptest summary with impute information score
for i in {1..22}
do
	for file in S*chr$i.*impute2
    do
		/work/gr-fe/username/tool/snptest_v2.5.4-beta3_linux_x86_64_dynamic/snptest_v2.5.4-beta3 \
		-data $file \
		$raw/CONTROL_samples \
		-filetype gen \
		-summary_stats_only \
		-o $file\_snptest
    done &
done
wait

printf "All done" > x.info.summary

#==============================================================================
# The SNPs failing the desired score threshold could be read from this file.
# However, to store a smaller version for longer term just
# print the columns of interest to new file
# 1,2,4,5,6,9: Chr SNP, Pos, A1, A2, score.
# Note that col 1 is "alternate_ids" but contains to chr number in this dataset
# File sizes >50% smaller. Runtime ~10 minutes with gzip.
#==============================================================================

# Skip 9 header lines
# Print columns
for i in {1..22}
do
	for file in S*chr$i.*impute2
	do
		tail -n +10 $file\_snptest \
		|awk '{print $1, $2, $4, $5, $6, $9}' \
		> $file\_snptest_score &&
		gzip $file\_snptest_score
	done &
done
wait

cp ./*snptest_score.gz $snptest/

#==============================================================================
# Wait until a later stage to prune out snps based on the info score
# The next few stages consist of QC based on geno, maf, mind and therefore.
# Wait until cohort a near-end stage to prune prevent having to re-run other 
# steps if any changes are desired.
#==============================================================================
