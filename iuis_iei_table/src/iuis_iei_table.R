library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
#library(ggplot2)
#library(plotly)

# Eventual, merge with /web/tools/genomic_tools/gene_evidence/gene_evidence.R for better links

df <-  readxl::read_xlsx("../data/10875_2022_1289_MOESM2_ESM.xlsx")
# checks since this is xlsx format
# rows 506 (incl. header) - yes
# cols 16 - yes
# hyperlinks - lost

# clean the data ----
df1 <- df$Inheritance 
df1 <- str_replace_all(df1," ","") # stupid white spaces
df1 <- str_replace_all(df1,"\\?","NA") # question mark is not appropriate, if not sufficient evidence keep as NA
df1 <- str_replace_all(df1,"AR or AD" ,"AD or AR" ) # make consistent
df1 <- str_replace_all(df1,"AD or AR" ,"AD/AR" ) # make consistent
df1 <- str_replace_all(df1,"AD \\(1 AR\\)" ,"AD/AR" ) # make consistent 
df1 <- str_replace_all(df1,"AD \\(1 AR\\)" ,"AD/AR" )
df1 <- str_replace_all(df1,"AD halploinsufficiency" ,"AD haploinsufficiency" ) 
df1 <- str_replace_all(df1,"XL \\(females may be affected\\)" ,"XL females_affected" )
df1 <- str_replace_all(df1,"XL females can be affected" ,"XL females_affected" )
df1 <- str_replace_all(df1," " ,":" ) # make 2 columns
df1 <- df1 %>% replace_na('NA') # I want the character string "NA"

df$Inheritance <- df1
rm(df1)
df <- separate(df, Inheritance, into = c("Inheritance", "Inheritance detail"), sep = ":")
df$Inheritance %>% unique()

df$OMIM_ID <- df$OMIM
df <- df %>% select(-OMIM)

df$OMIM_ID <- str_replace_all(df$OMIM_ID," ","") # stupid white spaces
df$OMIM_ID <- str_replace_all(df$OMIM_ID,"Not yet attributed","NA") # (-_-) 
df$OMIM_ID <- str_replace_all(df$OMIM_ID,"614602 \r\n" ,"614602") # (-_-) 
df$OMIM_ID <- str_replace_all(df$OMIM_ID,"· 612411" ,"612411") # (-_-) 

# column names
colnames(df)[colnames(df) == 'Genetic defect'] <- 'Gene symbol'

# set up URLs ----
# These URLs are only to show the text, but do not get used for the actual hyperlink. I add those within
# When using % for url, we nee to escape it for sprintf. Use doulbe (%%).
# We also have to escape for the reactable, and therefore use quadruple (%%%%).
df$UniProt <- sprintf('https://www.uniprot.org/...%s', df$`Gene symbol`)
df$Ensembl <- sprintf('https://www.ensembl.org/...%s', df$`Gene symbol`)
df$OMIM <- sprintf('https://www.omim.org/...%s', df$`Gene symbol`)
df$omni <- sprintf('https://omni.institutimagine.org/...%s', df$`Gene symbol`)
df$'gnomAD r2 GRCh37' <- sprintf('https://gnomad.broadinstitute.org/r2_1...%s', df$`Gene symbol`)
df$'gnomAD r3 GRCh38' <- sprintf('https://gnomad.broadinstitute.org/r3...%s', df$`Gene symbol`)
df$pdb <- sprintf('https://www.rcsb.org/...%s', df$`Gene symbol`)
df$HGNC <- sprintf('https://www.genenames.org/...%s', df$`Gene symbol`)
df$ORPHA <- sprintf('https://hpo.jax.org/ORPHA/...%s', df$`Gene symbol`)
df$HPO <- sprintf('https://hpo.jax.org/...%s', df$`Gene symbol`)
df$DisProt <- sprintf('https://disprot.org/...%s', df$`Gene symbol`)
df$AmiGo <- sprintf('http://amigo.geneontology.org/...%s', df$`Gene symbol`)
df$'Alpha Fold' <-  sprintf('https://www.alphafold.ebi.ac.uk/...%s', df$`Gene symbol`)
df$Gemma <- sprintf('https://gemma.msl.ubc.ca/...%s', df$`Gene symbol`)
df$ClinGen <- sprintf('https://search.clinicalgenome.org/...%s', df$`Gene symbol`)

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

# for value coloring, there can be no NAs (but character string "NA")

df_t <- 
	reactable( df,
				  compact = TRUE,
				  searchable = TRUE,
				  #elementId = "download-table",
				  defaultPageSize = 10,
				  defaultColDef = colDef(minWidth = 100),
				  columns = list(
				  	"Disease" = colDef(minWidth = 260), 
				  	"Inheritance detail" = colDef(minWidth = 140), 
				  	"Major category" = colDef(minWidth = 200), 
				  	"Associated features" = colDef(minWidth = 200), 
				  	"Subcategory" = colDef(minWidth = 200), 
				  	 "OMIM_ID" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://www.omim.org/entry/%s", df[index, "OMIM_ID"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),
				  	 
				  	 "UniProt" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://www.uniprot.org/uniprot/?query=gene:%s&sort=score", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),

				  	 "Ensembl" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://www.ensembl.org/Human/Search/Results?q=%s;site=ensembl;facet_species=Human", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),

				  	 "OMIM" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://www.omim.org/search?index=entry&sort=score+desc%%%%2C+prefix_sort+desc&start=1&limit=10&search=%s", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),

				  	 "gnomAD r2 GRCh37" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://gnomad.broadinstitute.org/gene/%s?dataset=gnomad_r2_1", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),

				  	 "gnomAD r3 GRCh38" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://gnomad.broadinstitute.org/gene/%s?dataset=gnomad_r3", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),

				  	 "omni" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://omni.institutimagine.org/search=%s/page=1", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),
				  	 "pdb" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://www.rcsb.org/search?request=%%%%7B%%%%22query%%%%22%%%%3A%%%%7B%%%%22type%%%%22%%%%3A%%%%22group%%%%22%%%%2C%%%%22nodes%%%%22%%%%3A%%%%5B%%%%7B%%%%22type%%%%22%%%%3A%%%%22group%%%%22%%%%2C%%%%22nodes%%%%22%%%%3A%%%%5B%%%%7B%%%%22type%%%%22%%%%3A%%%%22group%%%%22%%%%2C%%%%22nodes%%%%22%%%%3A%%%%5B%%%%7B%%%%22type%%%%22%%%%3A%%%%22terminal%%%%22%%%%2C%%%%22service%%%%22%%%%3A%%%%22full_text%%%%22%%%%2C%%%%22parameters%%%%22%%%%3A%%%%7B%%%%22value%%%%22%%%%3A%%%%22%s%%%%22%%%%7D%%%%7D%%%%5D%%%%2C%%%%22logical_operator%%%%22%%%%3A%%%%22and%%%%22%%%%7D%%%%5D%%%%2C%%%%22logical_operator%%%%22%%%%3A%%%%22and%%%%22%%%%2C%%%%22label%%%%22%%%%3A%%%%22full_text%%%%22%%%%7D%%%%5D%%%%2C%%%%22logical_operator%%%%22%%%%3A%%%%22and%%%%22%%%%7D%%%%2C%%%%22return_type%%%%22%%%%3A%%%%22entry%%%%22%%%%2C%%%%22request_options%%%%22%%%%3A%%%%7B%%%%22pager%%%%22%%%%3A%%%%7B%%%%22start%%%%22%%%%3A0%%%%2C%%%%22rows%%%%22%%%%3A25%%%%7D%%%%2C%%%%22scoring_strategy%%%%22%%%%3A%%%%22combined%%%%22%%%%2C%%%%22sort%%%%22%%%%3A%%%%5B%%%%7B%%%%22sort_by%%%%22%%%%3A%%%%22score%%%%22%%%%2C%%%%22direction%%%%22%%%%3A%%%%22desc%%%%22%%%%7D%%%%5D%%%%7D%%%%2C%%%%22request_info%%%%22%%%%3A%%%%7B%%%%22query_id%%%%22%%%%3A%%%%228fb30ed1650bdcc2f59067e304229b28%%%%22%%%%7D%%%%7D", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),
				  	 "ORPHA" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://hpo.jax.org/app/browse/disease/ORPHA:%s", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),
				  	 "HPO" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://hpo.jax.org/app/browse/search?q=%s&navFilter=all", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),
				  	 "HGNC" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://www.genenames.org/tools/search/#!/?query=HGNC:%s", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),
				  	 "DisProt" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://disprot.org/browse?sort_field=disprot_id&sort_value=asc&page_size=20&page=0&release=current&show_ambiguous=true&show_obsolete=false&free_text=%s", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),
				  	 "AmiGo" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("http://amigo.geneontology.org/amigo/search/bioentity?q=%s&searchtype=geneproduct", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),
				  	 "Alpha Fold" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://www.alphafold.ebi.ac.uk/search/text/%s", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),

				  	 "ClinGen" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://search.clinicalgenome.org/kb/genes?page=1&size=50&search=%s", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),

				  	 "Gemma" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://gemma.msl.ubc.ca/searcher.html?query=%s&scope=G", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),
				  	

				  	 
				  	"Inheritance" = colDef( minWidth = 100,
				  												  style = function(value) {
				  												  	if (value == "AD/AR") {color <- "#962fbf" # purple
				  												  	} else if (value == "Variable") {color <- "#962fbf" # purple
				  												  	} else if (value == "AR") {color <- "#4f5bd5" # blue
				  												  	} else if (value == "AD") {color <- "#d62976" # pink
				  												  	} else if (value == "Sporadic/toxin") {color <- "#e5ab00" # yellow
				  												  	} else if (value == "XL") {color <- "#fa7e1e" # orange
				  												  	} else if (value == "XLR") {color <- "#fa7e1e" # orange
				  												  	} else if (value == "NA") {color <- "#339900" # green
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


# library(reactablefmtr)
# save_reactable(df_t, "../output/iusis_iei_table_2022.html")

