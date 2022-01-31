#!/bin/sh
#SBATCH --job-name=vcftosimple
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --time=02:00:00
#SBATCH --mem=30gb
#SBATCH --out=./vcftosimple.out.%J
#SBATCH --error=./vcftosimple.err.%J

echo started at `date`

# Load all modules required
module load intel/19.0.5

# Tools
vcfhacks=/work/gr-fe/lawless/tool/vcfhacks

# Directories
# GENO=/work/gr-fe/progg/pathways/raw_data

# Convert vcf to simple tab-delim file

# version that Pualine used
# perl $vcfhacks/annovcfToSimple.pl \
# --vep --info_fields AF gnomADe_AF gnomADg_AF --gene_anno -u --contains_variant --text_output --functional \
# -i ./pri.leeds.hiv.combined.genotype.ts99pt9.1pcdbsnp.1pcEVS.vep.rename.multi.vcf
# -o ./pri.leeds.hiv.combined.genotype.ts99pt9.1pcdbsnp.1pcEVS.vep.rename.multi.tsv

# # version for Manon
# perl $vcfhacks/annovcfToSimple.pl \
# --vep --all \
# --do_not_simplify \
# --summarise --contains_variant \
# --text_output \
# --functional \
# -i ./pri.leeds.hiv.combined.genotype.ts99pt9.1pcdbsnp.1pcEVS.vep.rename.multi.vcf \
# -o ./pri.leeds.hiv.combined.genotype.ts99pt9.1pcdbsnp.1pcEVS.vep.rename.multi.tsv

# shorter version for Manon
perl $vcfhacks/annovcfToSimple.pl \
--vep --all --canonical_only -u --contains_variant --text_output --functional \
-i ../data/test_sample_vep.vcf \
-o ../data/test_sample_vep.tsv

# # filter the result to remove non-functional variants and duplicate transcripts 
#  head -1 ./test_samples_vep.tsv.txt \
#  >       ./test_samples_vep.canon_functional.tsv # add header line first

# grep -v "No valid functional variant" \
#  ./pri.leeds.hiv.combined.genotype.ts99pt9.1pcdbsnp.1pcEVS.vep.rename.multi.gnomad.tsv.txt | \
#  grep "ENSG" \
#  >> ./pri.leeds.hiv.combined.genotype.ts99pt9.1pcdbsnp.1pcEVS.vep.rename.multi.gnomad.canon_functional.tsv

exit
