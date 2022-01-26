#!/bin/bash
set -e

# # v1
# nextalign \
#  --sequences ../data/HRSV_virus11250_attachment_glycoprotein_host9605_20180101_20220108.fasta \
#  --reference ../data/NC_001781.1:4675-5600_HRSVB.fasta \
#  --output-dir ../data/ \
#  --output-basename nextalign_B_NC_001781

# nextalign \
#  --sequences ../data/HRSV_virus11250_attachment_glycoprotein_host9605_20180101_20220108.fasta \
#  --reference ../data/NC_038235.1:4673-5595_HRSVA.fasta \
#  --output-dir ../data/ \
#  --output-basename nextalign_A_NC_038235

nextalign \
 --sequences ../data/HRSV_virus11250_attachment_glycoprotein_host9605_19891231_20220101.fasta \
 --reference ../data/NC_001781.1:4675-5600_HRSVB.fasta \
 --output-dir ../data/ \
 --output-basename nextalign_B_NC_001781

nextalign \
 --sequences ../data/HRSV_virus11250_attachment_glycoprotein_host9605_19891231_20220101.fasta \
 --reference ../data/NC_038235.1:4673-5595_HRSVA.fasta \
 --output-dir ../data/ \
 --output-basename nextalign_A_NC_038235

# set up msa2file.sh
mv ../data/nextalign_A_NC_038235.aligned.fasta \
../data/nextalign_A_NC_038235.aligned.fasta.msa

mv ../data/nextalign_B_NC_001781.aligned.fasta \
../data/nextalign_B_NC_001781.aligned.fasta.msa


# Dylan it might be checking whether nextstrain aligner uses optimised and iterative alignment process (used by MAFFT and some of the other popular tools) when it is building the final output. Maybe this is why you are getting something a bit strange: https://en.wikipedia.org/wiki/Multiple_sequence_alignment#Progressive_alignment_construction

# Nextstrain say they use a "variation of Smith–Waterman" https://docs.nextstrain.org/projects/nextclade/en/latest/user/algorithm/01-sequence-alignment.html

# So compared to MAFFT, which starts with Smith-Waterman for pairwise alignment but then refines it progressively, there might be differences in scoring/penalizing of indels and substitutions between the two
* between the two MSA tools
