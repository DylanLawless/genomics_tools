#!/bin/bash

#==============================================================================
# This script will convert the msa file to a text file 
# with column 1 = sample name and column 2 = sequence
# Then split into two files
#==============================================================================

cd ../data

# This script will but the ID in column 1 and sequence in column 2 (tab delimiter introced on line 15) (line 21 to 32 are hacks for my dataset with different protein names).
for file in *.msa
do
	cat $file \
		| sed 's/ \[Human respiratory syncytial virus\]/	/g' \
		| sed 's/ /_/g' \
		| tr -d '\n' \
		| tr '>' '\n' \
		| sed 's/lcl|//g' \
		| tail -n +2 \
	| sed 's/\.8_fusion_glycoprotein//g' \
	| sed 's/\.7_attachment_glycoprotein//g' \
	| sed 's/\.11_RNA-directed_RNA_polymerase_L//g' \
	| sed 's/\.9_transcription_elongation_factor_M2-1//g' \
	| sed 's/\.10_transcription-replication_factor_M2-2//g' \
	| sed 's/\.5_matrix_protein//g' \
	| sed 's/\.1_non-structural_protein_1//g' \
	| sed 's/\.2_non-structural_protein_2//g' \
	| sed 's/\.3_nucleoprotein//g' \
	| sed 's/\.4_phosphoprotein//g' \
	| sed 's/\.6_small_hydrophobic_protein//g' \
	| sed 's/NC_.*_Bovine_respiratory_syncytial_virus\,_complete_genome\./NC	/g' \
		> $file.tabbed
done

# Get file with simple sample labels
for file in *.msa.tabbed
do
	cat $file \
	| cut -f1 -d '	' \
	> $file.labels
done
# 
# Split the sequence into single tabs
for file in *.msa.tabbed
do
	cat $file \
	| cut -f2 -d'	' \
	| sed 's/\(.\)/\1 /g' \
	> $file.split
done

# Output files
# Load in R with
# 
# # Multiple sequence alignment file amino acids
# msa.1 <- 
#   read.table(file="./data/file.split", 
#              header = FALSE, 
#              sep = " ", 
#              stringsAsFactors = FALSE,
#              colClasses = c("character"))
# 
# # Sample lables
# msa.2 <- 
#   read.table(file="./data/file.labels", 
#              header = FALSE, 
#              sep = "\t", 
#              stringsAsFactors = FALSE)

