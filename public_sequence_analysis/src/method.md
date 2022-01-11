# Reference sequences

* Human orthopneumovirus Subgroup A, complete cds
* NCBI Reference Sequence: NC_038235.1:4673-5595
* <https://www.ncbi.nlm.nih.gov/nuccore/NC_038235.1?report=fasta&from=4673&to=5595>
* File: data/NC_038235.1:4673-5595_HRSVA.fasta
 
* Human orthopneumovirus Subgroup B, complete genome
* NCBI Reference Sequence: NC_001781.1:4675-5600
* <https://www.ncbi.nlm.nih.gov/nuccore/NC_001781.1?report=fasta&from=4675&to=5600>
* File: data/NC_001781.1:4675-5600_HRSVB.fasta



# Download public sequence data

Next we download viral sequence data from NCBI. 

<https://www.ncbi.nlm.nih.gov/labs/virus/vssi/#/virus?SeqType%2F_s=Nucleotide&VirusLineage%2F_ss=Human%20orthopneumovirus,%20taxid:11250&SeqType_s=Nucleotide&HostLineage_ss=Homo%20(humans),%20taxid:9605&VirusLineage_ss=Human%20orthopneumovirus%20(HRSV),%20taxid:11250&ProtNames_ss=attachment%20glycoprotein&CollectionDate_dr=2018-01-01T00:00:00.00Z%20TO%202022-01-07T23:59:59.00Z>

## Download settings
* Virus: Human orthopneumovirus (HRSV), taxid:11250
* Proteins: attachment glycoprotein
* Host: Homo (humans), taxid:9605
* Collection Date:  From Jan 1, 2018 To Jan 8, 2022

* Filename: data/HRSV_virus11250_attachment_glycoprotein_host9605_20180101_20220108.fasta
* Filename: data/HRSV_virus11250_attachment_glycoprotein_host9605_20180101_20220108.csv

## Results:

* Download 1: Sequence data (FASTA Format) Nucleotide [sequences.fasta]
* Download 2: Current table view result CSV format, all columns, meta data. [sequences.csv]

# Align 
* Using nextalign local install on mac
* <https://docs.nextstrain.org/projects/nextclade/en/stable/user/nextalign-cli.html#>
* File: src/nextalign.sh
