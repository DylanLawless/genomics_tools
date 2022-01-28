#!/bin/bash
#SBATCH --job-name=filter
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=12
#SBATCH --time=03:00:00
#SBATCH --mem=60gb
#SBATCH -o ./log/2.2_grm.%J.out
#SBATCH -e ./log/2.2_grm.%J.err

processed=/work/gr-fe/username/case_gwas/data/processed
wdir=$processed/combined

#==============================================================================
# Make GRM and PCA
# Prune known and cohort-specific LD regions for the grm
#==============================================================================

# Removing long-range LD regions for PCA
plink2 \
	--bfile $wdir/geno/cohort.filt \
	--exclude range $processed/exclusion_regions_hg19.txt \
	--make-bed \
	--out $wdir/grm/cohort.filt.no_lrldr

# Produce a pruned subset of markers that are in approximate 
# linkage equilibrium with each other
# Writes the IDs to plink.prune.in
# (and the IDs of all excluded variants to plink.prune.out)
plink2 \
	--bfile $wdir/grm/cohort.filt.no_lrldr \
	--indep-pairwise 50 5 0.5 \
	--out $wdir/grm/cohort.filt.no_lrldr

# Extract the pruned subset of markers
plink2 \
	--bfile $wdir/grm/cohort.filt.no_lrldr \
	--extract $wdir/grm/cohort.filt.no_lrldr.prune.in \
	--make-bed --out $wdir/grm/cohort.filt.no_lrldr.pruned

# PCA
# this takes apporx 10 minutes on 32 threads, 70 minutes on this dataset on ~18 thread

/work/gr-fe/username/tool/gcta64 \
	--bfile $wdir/grm/cohort.filt.no_lrldr.pruned \
	--autosome --make-grm \
	--out $wdir/grm/cohort.filt.grm \
	--thread-num 24

/work/gr-fe/username/tool/gcta64 \
	--grm $wdir/grm/cohort.filt.grm \
	--pca 20 \
	--out $wdir/grm/cohort.filt.pca \
	--thread-num 24

# # have a look at the PCA in gwas_2/grm/plots_pca.pdf
# module load intel/18.0.5 &&\
# module load intel-mkl/2018.5.274 &&\
# module load r/3.6.0 &&\
# 
# cd $wdir/grm/
#      Rscript $wdir/grm/pca.R


# # exclude 2 outliers
# plink \
#     --bfile $wdir/geno/cohort.filt \
#     --remove $wdir/grm/outliers \
#     --make-bed \
#     --out $wdir/geno/cohort.filtered \
#     --threads 12
# 
