#!/bin/sh
#SBATCH --job-name=virt_panel
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=2
#SBATCH --time=04:00:00
#SBATCH --mem=30gb
#SBATCH --out=./log/virt_panel.%J.out
#SBATCH --error=./log/virt_panel.%J.err

set -e
echo started at `date`

# Load all modules required
module load intel/18.0.5

# Tools
vcfhacks=./tool/vcfhacks

# Directories
gePID_list=./tool/gene_lists/genomics_england_panel_app/ge_panel_app_Primary_immunodeficiency_v2.1_20200224_coordinate_grch38p13.bed
dir=./data
file=$dir/sample_vcf.vep

# location format -r "X:1-2000", or -b for a bed file or list file with 1 per line.
perl $vcfhacks/filterVcfOnLocation.pl \
	-i $file\.vcf \
	-b $gePID_list \
	-o $file\.gePIDv2.1.vcf

echo ended at `date`
