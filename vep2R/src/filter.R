library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

# Import data ----
# The tab output from VEP dbNSFP has the annotation data sep by "|".
system("gunzip ../data/test_sample_vep.tsv.gz")

file="../data/test_sample_vep.tsv"

headers = read.csv(file, skip = 0, header = F, nrows = 814, as.is = T)
# tail(headers, 1)

df = read.csv(file, skip = 814, header = F, sep = "\t", )

names <- strsplit( (headers[814, c(1)]), "\\t") %>% as.data.frame()
names(names) <- c("v1")
names(df) <- t(names)
system("gzip ../data/test_sample_vep.tsv")

# testing copy
d <- df

# There are duplicated columns. Keep only the last one
d <- d[, !duplicated(colnames(d), fromLast = TRUE)] 

# select high impact variants
d %>% select(IMPACT) %>% unique()



d_high <- d %>% filter(grepl("HIGH", IMPACT))
d_high <- d %>% filter(grepl("HIGH|MODERATE", IMPACT))
d_high <- d %>% filter(grepl("HIGH|MODERATE|LOW", IMPACT))

# re order for viewing
d_high <- d_high %>% select(SYMBOL, Consequence, HGVSc, HGVSp,
									 CANONICAL,
									 MAX_AF, MAX_AF_POPS, # Max in 1000 Genomes, ESP and ExAC/gnomAD¬
									 gnomAD_exomes_non_topmed_POPMAX_AF, 
									 gnomAD_genomes_POPMAX_AF,
									 gnomAD_genomes_POPMAX_AC,
									 gnomAD_genomes_POPMAX_AN,
									 clinvar_OMIM_id,
									 clinvar_clnsig,
									 clinvar_hgvs,
									 clinvar_review,
									 clinvar_trait,
									 everything())

d_filt <- d_high %>% filter(MAX_AF < 0.1) #%>% filter(CANONICAL == "YES")


# Patient-specific ----------------------------------------------------

# pheno: severe adverse effects to Mesantoin, an anti-convulsant drug
# CYP2C19
# https://www.pharmvar.org/gene/CYP2C19
d_high %>% filter(SYMBOL == "CYP2C19" & CANONICAL == "YES") %>% select(SYMBOL, Consequence, HGVSc, HGVSp,
																  CANONICAL, rs_dbSNP)

# rs4986893
# https://go.drugbank.com/pharmaco/genomics/DBSNPE000019

#Defining Change(s): A Allele, homozygote	rs4986893
#Allele Name: CYP2C19*3
#Patients with this genotype have reduced metabolism of mephenytoin.
#References: De Morais SM, Wilkinson GR, Blaisdell J, Meyer UA, Nakamura K, Goldstein JA: Identification of a new genetic defect responsible for the polymorphism of (S)-mephenytoin metabolism in Japanese. Mol Pharmacol. 1994 Oct;46(4):594-8. [Article]

# The other varirants include a stop-gained and therefore may be compound-het.

