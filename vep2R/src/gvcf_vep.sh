#!/bin/sh
#SBATCH --job-name=vep
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --time=03:00:00
#SBATCH --mem=30gb
#SBATCH --out=../log/vep.out.%J
#SBATCH --error=../log/vep.err.%J

echo started at `date`
 
#//////////////////////////////////////////////////////////////////////////////
# Load all modules required
#//////////////////////////////////////////////////////////////////////////////

module load intel/19.0.5
module load samtools/1.10
module load htslib/1.10.2

#//////////////////////////////////////////////////////////////////////////////
# Directories
#//////////////////////////////////////////////////////////////////////////////
# LOFFILES=/work/gr-fe/progg/temp_pri/loftee_files/GRCh37
# VEPdb=/work/gr-fe/databases/
# base=/scratch/lawless
VEP_dir=/work/gr-fe/lawless/tool/ensembl-vep
REFGENOME=/work/gr-fe/lawless/database/GRCh38_reference_genome/GRCh38_full_analysis_set_plus_decoy_hla.fa
dbNSFP=/work/gr-fe/lawless/database/dbNSFP/data/dbNSFP4.2a_grch38.gz
data=/work/gr-fe/lawless/database/dbNSFP/toy/data

# copy loftee plugin files /work/gr-fe/databases/vep/VEP_plugins/
# LoF.pl, SpliceConsensus.pm, utr_splice.pl, gerp_dist.pl, loftee_splice_utils.pl, loftee_splice_utils.pl, -r maxEntScan, svm.pl
# to the new plugins dir

$VEP_dir/vep \
--fasta $REFGENOME \
--use_given_ref \
--everything --offline \
--cache \
--dir_cache $VEP_dir/CACHE \
 --plugin dbNSFP,$dbNSFP,ALL \
--dir_plugins $VEP_dir/Plugins \
-i $data/test_sample.vcf \
--tab \
-o $data/test_sample_vep.tsv \
--fork 12 \
--force_overwrite
# --vcf # vcf output

# You may include all columns with ALL; this fetches a large amount of data per
# variant!:
# --plugin dbNSFP,$dbNSFP,LRT_score,GERP++_RS \

# Set up
# download ref genome
# source: http://ftp.1000genomes.ebi.ac.uk/vol1/ftp/technical/reference/GRCh38_reference_genome/
# method: /work/gr-fe/lawless/database/GRCh38_reference_genome/download.sh
# date: 20220131
# author: dylan


# previously used
# $base/ensembl-vep/vep \
# --offline --vcf --everything \
# --cache \
# --dir_cache $base/VEP_cache/ \
# --fasta $VEPdb/cache_b37/homo_sapiens/92_GRCh37/Homo_sapiens.GRCh37.75.dna.primary_assembly.fa.gz \
# --dir_plugins $base/VEP_plugins \
# --plugin gnomADc,$base/gnomAD_files/gnomADg.gz \
# --custom $base/gnomAD_files/gnomad.genomes.r2.1.1.sites.vcf.bgz,gnomADg,vcf,exact,0,AF \
# --plugin Condel,$VEPdb/VEP_plugins/config/Condel/config/ \
# --plugin SpliceConsensus \
# -i $data/test_sample_vep.vcf \
# -o $data/test_sample.vcf \
# --use_given_ref \
# --fork 12 \
# --force_overwrite
exit
