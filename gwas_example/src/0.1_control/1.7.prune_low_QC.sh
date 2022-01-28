#!/bin/bash
#SBATCH --job-name=1.7.prun
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=22
#SBATCH --time=01:00:00
#SBATCH --mem=64gb
#SBATCH -o ./log/1.7.prune.%J.out
#SBATCH -e ./log/1.7.prune.%J.err

#==============================================================================
# This script will use SNPTEST information scores prepared previously
# https://mathgen.stats.ox.ac.uk/genetics_software/snptest/snptest.html#info_measures
#==============================================================================

processed=/work/gr-fe/username/case_gwas/data/processed
cohort=$processed/geno/CONTROL.QC.impute2_plink2.allfiles_mind1_maf01_geno2_hwe6
snptest=$processed/snptest/control_snptest
cd $snptest

printf "Print SNP IDs for info less than 0.8\n"
printf "Input for these scores was the original raw Impute2 data\n"
printf "and was from before filtering\n"

for i in {1..22}
do
	for file in S*chr$i.*score
	do
		cat $file \
		| awk '$6 < 0.8' \
		| awk '{print $2}' \
		> $file\_below_0.8
	done
done &
wait
printf "All files printed\n"

printf "Cat into one file...\n"
for i in {1..22}
do
	cat S*chr$i.*score_below_0.8 >> exclude_info_below_0.8
done
printf "Printed to exlude_info_below_0.8\n"


printf "Now using this list of any SNP below QC\n"
printf "To be removed if still remaining in the filtered plink dataset\n"
printf "Expecting < 10% removal\n"

# Remove these SNPs
plink \
	--bfile $cohort \
	--exclude exclude_info_below_0.8 \
	--make-bed \
	--out $cohort.info8
