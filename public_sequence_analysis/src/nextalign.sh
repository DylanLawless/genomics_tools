#!/bin/bash
set -e

nextalign \
 --sequences ../data/HRSV_virus11250_attachment_glycoprotein_host9605_20180101_20220108.fasta \
 --reference ../data/NC_001781.1:4675-5600_HRSVB.fasta \
 --output-dir ../data/ \
 --output-basename nextalign_B_NC_001781

nextalign \
 --sequences ../data/HRSV_virus11250_attachment_glycoprotein_host9605_20180101_20220108.fasta \
 --reference ../data/NC_038235.1:4673-5595_HRSVA.fasta \
 --output-dir ../data/ \
 --output-basename nextalign_A_NC_038235

