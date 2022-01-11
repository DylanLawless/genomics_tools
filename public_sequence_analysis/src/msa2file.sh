#!/bin/bash

#==============================================================================
# This script will convert the msa file to a text file 
# with column 1 = sample name and column 2 = sequence
# Then split into two files
#==============================================================================

cd ../data

# This script will put the ID in column 1 and sequence in column 2 (tab delimiter introced on line 15) (line 21 to 32 are hacks for my dataset with different protein names).
for file in *.msa
do
	cat $file \
		| cut -f1 -d"|" \
		| tr -d '\n' \
		| tr '>' '\n' \
		| tail -n +2 \
		> $file.tabbed
done

# Get file with simple sample labels
for file in *.msa.tabbed
do
	cat $file \
	| cut -f1 -d ' ' \
	> $file.labels
done
# 
# Split the sequence into single tabs
for file in *.msa.tabbed
do
	cat $file \
	| cut -f2 -d' ' \
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

