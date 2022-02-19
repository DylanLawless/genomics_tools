library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

# test set ----
# For this test I pick two variants.

# rs750988335 GnomAD v2.1.1 9-32493805-T-C (GRCh37)
# rs750988335 GnomAD v3.1.2 9-32493807-T-C (GRCh38)

# rs141339092 GnomAD v2.1.1 9-32491328-T-C (GRCh37)
# rs141339092 GnomAD v3.1.2 9-32491330-T-C (GRCh38)

# Import data ----
# The tab output from VEP dbNSFP has the annotation data sep by "|".
system("gunzip ../data/ddx58_grch38.vep.tsv")

file="../data/ddx58_grch38.vep.tsv"

headers = read.csv(file, skip = 0, header = F, nrows = 814, 
						 as.is = T)
tail(headers, 1)

df = read.csv(file, #skip = 814, 
				  skip = 735, 
				  header = F, sep = "\t", )

names <- strsplit( (headers[814, c(1)]), "\\t") %>% as.data.frame()
names(names) <- c("v1")
names(df) <- t(names)
system("gzip ../data/ddx58_grch38.vep.tsv")
rm(headers)

# testing copy
d <- df

# There are duplicated columns. Keep only the last one
d <- d[, !duplicated(colnames(d), fromLast = TRUE)] 

# Replace the NA value
d[ d == "-" ] <- NA

# select high impact variants
d %>% select(IMPACT) %>% unique()

d_high <- d %>% filter(grepl("HIGH", IMPACT))
d_high <- d %>% filter(grepl("HIGH|MODERATE", IMPACT))
d_high <- d %>% filter(grepl("HIGH|MODERATE|LOW", IMPACT))

# re order for viewing
d_high <- d_high %>% select(SYMBOL, Consequence, IMPACT, HGVSc, HGVSp,
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

d_filt <- d_high %>% filter(MAX_AF < 0.1) %>% filter(CANONICAL == "YES") %>% filter(!SYMBOL == "-")
df <- d_filt
rm(d_filt)
rm(d_high)
# Printing clean ----
# Remove columns where all rows are NA
# The function works, but the more complex DT lapply will scale to large datasets.
# Filter(function(x)!all(is.na(x)), df)

library(data.table)
DT <- as.data.table(df)
d_print <- DT[,which(unlist(lapply(DT, function(x)!all(is.na(x))))),with=F]

# Output table html ----
# reactable ----
library(reactable)
options(reactable.theme = reactableTheme(
	borderColor = "#dfe2e5",
	stripedColor = "#E5E5E5",
	highlightColor = "#fcf0e6",
	cellPadding = "8px 12px",
	style = list(fontFamily = "-apple-system, Arial, BlinkMacSystemFont, Segoe UI, Helvetica,  sans-serif",
					 fontSize = "0.8rem"),
	searchInputStyle = list(width = "50%")
))

df_t <- 
	reactable( d_print,
				  compact = TRUE,
				  searchable = TRUE,
				  #elementId = "download-table",
				  defaultPageSize = 10,
				  defaultColDef = colDef(minWidth = 90),
				  columns = list(
				  	"DOMAINS" = colDef(minWidth = 1000),  # overrides the default
				  	"Consequence" = colDef(minWidth = 120),  # overrides the default
				  	"gnomAD_exomes_non_topmed_POPMAX_AF" = colDef(minWidth = 120), 
				  	"gnomAD_genomes_POPMAX_AF" = colDef(minWidth = 120), 
				  	'IMPACT' = colDef( minWidth = 130,
				  												  style = function(value) {
				  												  	if (value == "HIGH") {color <- "#99cc33"
				  												  	} else if (value == "MODERATE") {color <- "#fa7e1e"
				  												  	} else if (value == "LOW") {color <- "#339900"
				  												  	} else if (value == "NA") {color <- "grey"
				  												  	} else { color <- "black"}
				  												  	list(color = color) })
				  	
				  ),
				  filterable = TRUE,
				  showSortable = TRUE,
				  showPageSizeOptions = TRUE,
				  striped = TRUE,
				  highlight = TRUE
	)

df_t
library(reactablefmtr)
save_reactable(df_t, "../output/ddx58_grch38.vep.html")


