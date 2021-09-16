#!/bin/bash

file=ge_panel_app_Primary_immunodeficiency_v2.1_20200224_coordinate_grch38p13

	# print
	# keep columns: chr start stop 
	# remove headers
	# sort by chr then by position 
cat $file.tsv | \
	awk '{print $4,$2,$3}' | \
	tail -n +2 | \
	sort -nk1,1 -nk2,2 \
	> $file.bed
	
# convert to GRCh38 notation "chr"

cat $file.bed | grep -v "^CHR" > main
cat main | grep -v "^X" > numeric
cat main | grep "^X" > chrx

cat numeric chrx > ordered 

cat ordered | \
	sed 's/^/chr/' | \
	awk '{print $1":"$2"-"$3}' \
	> $file.bed


rm main numeric chrx ordered




