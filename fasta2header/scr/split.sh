#!/bin/bash

cd ../data

split -l 1 fasta_header 

mv x* split/

cd split
for file in x* 
do
	cat $file | sed -e 's/.\{159\}/&\n/g' > cut_$file
done

# cleanup
rm x*

# merge line-by-line
# remove overhand 12 lines
zsh -c 'paste ./*(.om)' | sed 's/	/&\n/g' |\
	tail -r | tail -n +12 | tail -r \
	> fasta_header_split.txt

# cleanup 
rm cut_*
