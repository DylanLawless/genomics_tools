#!/bin/bash
#SBATCH --job-name=LDlocus
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=28
#SBATCH --time=01:00:00
#SBATCH --mem=60gb
#SBATCH -o ./log/3.1_LDlocus.%J.out
#SBATCH -e ./log/3.1_LDlocus.%J.err

echo started at `date`
processed=/work/gr-fe/lawless/spss_gwas/data/processed

#	#==============================================================================
#	# Filter PLINK files for locus of interest
#	# to see how many match our list of coding variants in LD
#	#==============================================================================
#	# One option is to extract a signle region in one step
#	# SNPs in LD region
#	# 9:111463878-112074733
#	# 9 111463-112074 kb
#	
#	# The alternative done here is to list the known coding variants in LD with lead
#	# is listed in our file: LDrange_coding_variants.txt
#	# We will check the number of both coding and all SNPs
#	
#	
#	# checking first the cases only
#	plink \
#		--bfile $processed/combined/geno/cohort.filt.group \
#		--extract range LDrange_coding_variants.txt \
#		--make-bed \
#		--out $processed/misc/chr9.locus.coding.casecontrol
#	
#	plink \
#		--bfile $processed/combined/geno/cohort.filt.group \
#		--extract range LDrange_all_variants.txt \
#		--make-bed \
#		--out $processed/misc/chr9.locus.all.casecontrol
#	
#	# then check case/control cohort after QC
#	plink \
#		--bfile $processed/geno/SPSS.QC.impute2_plink2.allfiles.king_mind1_maf01_geno2.info8 \
#		--extract range LDrange_coding_variants.txt \
#		--make-bed \
#		--out $processed/misc/chr9.locus.coding.cases
#	
#	plink \
#		--bfile $processed/geno/SPSS.QC.impute2_plink2.allfiles.king_mind1_maf01_geno2.info8 \
#		--extract range LDrange_all_variants.txt \
#		--make-bed \
#		--out $processed/misc/chr9.locus.all.cases
#	
#	
#	# compare numbers
#	wc -l LDrange_coding_variants.txt > LD_extract_count.txt
#	wc -l LDrange_all_variants.txt >> LD_extract_count.txt
#	wc -l $processed/misc/chr9.locus.*bim >> LD_extract_count.txt
#	
#	
#	# Next we want the summary stats for these SNPs
#	# The coding variants are sufficienct for our interest
#	# 5 variants from
#	# /work/gr-fe/lawless/spss_gwas/data/processed/misc/chr9.locus.coding.casecontrol.bim
#	
#	# 9	9:111659483	0	111659483	G	T
#	# 9	9:111660851	0	111660851	T	C
#	# 9	rs1051474:111881856:T:C	0	111881856	C	T
#	# 9	rs12001627:111882167:G:C	0	111882167	C	G
#	# 9	9:111945049	0	111945049	T	G
#	
#	# for a large list use $grep -wFf list target
#	# we will just grep the 5 individually since it is so few
#	output=/work/gr-fe/lawless/spss_gwas/data/processed/combined/geno
#	
#	grep "^9" $output/cohort.filt.group.case_control.loco.mlma | grep "9:111659483" > LD_extract_mlma.txt
#	grep "^9" $output/cohort.filt.group.case_control.loco.mlma | grep "9:111660851" >> LD_extract_mlma.txt
#	grep "^9" $output/cohort.filt.group.case_control.loco.mlma | grep "rs1051474:111881856:T:C" >> LD_extract_mlma.txt
#	grep "^9" $output/cohort.filt.group.case_control.loco.mlma | grep "rs12001627:111882167:G:C" >> LD_extract_mlma.txt
#	grep "^9" $output/cohort.filt.group.case_control.loco.mlma | grep "9:111945049" >> LD_extract_mlma.txt
#	
#	# to get the full list, extract col2 to make a grep list
#	cut -f2 /work/gr-fe/lawless/spss_gwas/data/processed/misc/chr9.locus.all.casecontrol.bim > all_grep_list.txt
#	grep "^9" $output/cohort.filt.group.case_control.loco.mlma \
#		| grep -wFf all_grep_list.txt $output/cohort.filt.group.case_control.loco.mlma \
#		> LD_extract_mlma_all.txt
#	
#	# Note that we include any transcript; i.e. 15 candidate coding variants 
#	# represents only 8 unique positions/canonical variants. If one of this 
#	# was significantly assocciated then we would look as all potention transcripts
#	# for coding changes. But since none of strongly assocciated then we will just
#	# report 8 canonical coding variants in LD but not assocciated. 
#	echo ended at `date`



output=/work/gr-fe/lawless/spss_gwas/data/processed/combined/geno
# 111600000 - 112000000
grep "^9" $output/cohort.filt.group.case_control.loco.mlma \
	| awk '$3>111600000' \
	| awk '$3<112000000' > $output/cohort.filt.group.case_control.loco.mlma.111-112

# results followed further in "LD_vep" locally. 
