#!/bin/bash
#SBATCH --job-name=grm
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=28
#SBATCH --time=02:00:00
#SBATCH --mem=34gb
#SBATCH -o ./log/0.6.filter.%J.out
#SBATCH -e ./log/0.6.filter.%J.err

processed=/work/gr-fe/username/case_gwas/data/processed
file=CASE.QC.impute2_plink2.allfiles.king_mind1_maf01_geno2.info8

#==============================================================================
# Make GRM and PCA
# Prune known and cohort-specific LD regions for the grm
#==============================================================================

# Removing long-range LD regions for PCA
plink2 \
	--bfile $processed/geno/$file \
	--exclude range $processed/exclusion_regions_hg19.txt \
	--make-bed \
	--out $processed/grm/$file.no_lrldr

# Produce a pruned subset of markers that are in approximate 
# linkage equilibrium with each other
# Writes the IDs to plink.prune.in
# (and the IDs of all excluded variants to plink.prune.out)
plink2 \
--bfile $processed/grm/$file.no_lrldr \
--indep-pairwise 50 5 0.5 \
--out $processed/grm/$file.no_lrldr

# Extract the pruned subset of markers
plink2 \
--bfile $processed/grm/$file.no_lrldr \
--extract $processed/grm/$file.no_lrldr.prune.in \
--make-bed --out $processed/grm/$file.no_lrldr.pruned

# PCA
# this takes apporx 2 minutes on 12 threads
/work/gr-fe/username/tool/gcta64 \
--bfile $processed/grm/$file.no_lrldr.pruned \
--autosome --make-grm \
--out $processed/grm/$file.grm \
--thread-num 12

/work/gr-fe/username/tool/gcta64 \
--grm $processed/grm/$file.grm \
--pca 20 \
--out $processed/grm/$file.pca \
--thread-num 12

# # Have a look at the PCA in gwas_2/grm/plots_pca.pdf
# module load intel/18.0.5 &&\
# module load intel-mkl/2018.5.274 &&\
# module load r/3.6.0 &&\
# 
# cd $processed/grm/
#      Rscript $processed/grm/pca.R

