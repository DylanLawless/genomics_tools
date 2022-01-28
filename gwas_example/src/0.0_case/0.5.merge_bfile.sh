#!/bin/bash
#SBATCH --job-name=0.5.merge
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=28
#SBATCH --time=02:00:00
#SBATCH --mem=84gb
#SBATCH -o ./log/0.5.merge.%J.out
#SBATCH -e ./log/0.5.merge.%J.err

#==============================================================================
# This script will use Plink1.9 to merge files in order to make it easier to 
# work with a single full genome cohort
#==============================================================================

processed=/work/gr-fe/username/case_gwas/data/processed
scratch_raw=/scratch/username/case_gwas/data/raw
cd $scratch_raw/archive.CASE.data

#==============================================================================
# Make merge list of all files in genomic order.
# Removed topline: CASE.QC_chr1.pos13417-5012355.impute2_plink2 
# I didn't make a script for this small job and forgot to record the command
# but essentially - list all files then sort based on chr number and then on
# position. Note that unix sort will not correctly order the position as a
# string. 
# Output: $processed/mergelist/case_filelist_bed.bim.fam 557 lines
# Output: $processed/mergelist/control_filelist_bed.bim.fam 552 lines
#==============================================================================

# filter miss snps
for i in {1..22}
do
	for file in S*chr$i.*impute2
	do
	/work/gr-fe/username/tool/plink_1.90/plink \
	--bfile $file\_plink2 \
	--exclude CASE.QC.impute2_plink2.allfiles-merge.missnp \
	--make-bed --out $file\_plink2_filt
	done
done &
wait

# Merge all in order
/work/gr-fe/username/tool/plink_1.90/plink \
	--bfile CASE.QC_chr1.pos13417-5012355.impute2_plink2_filt \
	--merge-list $processed/mergelist/case_filelist_bed.bim.fam \
	--make-bed --out CASE.QC.impute2_plink2.allfiles \
	--threads 22 --memory 60000

# Sync to main backed up are for long-term storage and downstream analysis
mkdir $processed/geno
rsync -avz CASE.QC.impute2_plink2.allfiles.* $processed/geno

