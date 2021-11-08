## msa2file
1. Script - [make_data](msa2file/src/make_data.sh) Create a toy dataset of viral multiple sequence alignment data.
2. Script - [msa2file](msa2file/src/msa2file.sh) Split it into 2 files: labels and genotype for importing with R data.table.
3. Output - [msa2file/data/](msa2file/data).

## fasta2header
Manipulate a MSA file to get line-by-line alignment.
This script converts a simple multiple sequence alignment to lines of set lenght _n_ and then prints each line-by-line in order.
It produces a text version of what most MSA viewers would do, such that I can use the output in html website background and control it using css. 
The name is becuase I used this to create a html header on a webpage.

1. Script - [split.sh](fasta2header/scr/split.sh)
2. Input - [MSA sequence only](fasta2header/data/fasta_header) 10 sample of protein length 1272aa. Line set to break at length 159
3. Output - [MSA line-by-line](fasta2header/data/split/fasta_header_split.txt) Prited out line-by-line.

## gene panel
1. Gene names source from reputable source <https://panelapp.genomicsengland.co.uk/panels/>
2. Gene names mapped to coordinates using http://www.ensembl.org/biomart for reference genome build.
3. The biomart output converted to bed with [make_bed.sh](gene_list/genomics_england_panel_app/make_bed.sh)
4. The bed can then be used as shown in [virtual_panel.sh](gene_list/genomics_england_panel_app/virtual_panel.sh)

## R vep consequence filter
1. Filter a VCF as a dataframe based on VEP conseqience annotation [vep_consequence_table.R](vep_consequence_filter/scr/vep_consequence_table.R)
* Script in: [vep_consequence_table/scr](https://github.com/DylanLawless/genomics_tools/tree/master/vep_consequence_filter)
* An example dataset is provided after vcf2tsv conversion in: 
[vep_consequence_table/data](https://github.com/DylanLawless/genomics_tools/tree/master/vep_consequence_filter)
* The vep_consequence_table.tsv file has been prepared for query matching your dataset: [vep_consequence_table/data](https://github.com/DylanLawless/genomics_tools/tree/master/vep_consequence_filter)

## glm shiny
Unfinish app for custom genotype glm models.

## Variant testing
[variant_testing](https://github.com/DylanLawless/genomics_tools/tree/master/variant_testing)
Example of case/control enrichment of variant
The toy_examlple:
* dataset - multiple alignment sequence of amino acids
* Logistic regression - on single variant or whole gene
* Fisher exact test
* ODDs ratio and confidence interval

Excell format files are shown as an example of what data may look like.

How do we test something like SKAT-O or linear regression such that we can see the effect of single variants or multiple variants in single samples, or variants shared in several samples?

## Somatic risk
Plots for somatic variant risk with individual events and cumulative risk.

## Other links
This samtools repo contains useful data including VCF
<https://github.com/samtools/hts-specs>
including specs for [VCFv4.2.pdf](https://github.com/samtools/hts-specs/blob/master/VCFv4.2.pdf)
and [CRAMv3.pdf](https://github.com/samtools/hts-specs/blob/master/CRAMv3.pdf).

