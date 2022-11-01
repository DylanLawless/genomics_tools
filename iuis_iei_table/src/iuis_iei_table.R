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
df1 <- str_replace_all(df1,"\\(","")
df1 <- str_replace_all(df1,"\\)","")
df1 <- str_replace_all(df1," ","") # white spaces
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

# df$OMIM_ID <- df$OMIM
# df <- df %>% select(-OMIM)
colnames(df)[colnames(df) == 'OMIM'] <- 'OMIM_ID'

# Fix typos etc
df$OMIM_ID <- str_replace_all(df$OMIM_ID," ","") # white spaces
df$OMIM_ID <- str_replace_all(df$OMIM_ID,"Not yet attributed","NA") # (-_-) 
df$OMIM_ID <- str_replace_all(df$OMIM_ID,"614602 \r\n" ,"614602") # (-_-) 
df$OMIM_ID <- str_replace_all(df$OMIM_ID,"· 612411" ,"612411") # (-_-) 
df$Disease <- str_replace_all(df$Disease,"C8bdeficiency" ,"C8b deficiency") # (-_-) 
# column names
colnames(df)[colnames(df) == 'Genetic defect'] <- 'Gene symbol'


# Fix gene symbols
df$`Gene symbol` <- str_replace_all(df$`Gene symbol`,"IKBKG c. 671+3 G > C" ,"IKBKG") # (-_-) 
df$`Gene symbol` <- str_replace_all(df$`Gene symbol`,"CFHR1 CFHR2. CFHR3 CFHR4 CFHR5" ,"CFHR1 CFHR2 CFHR3 CFHR4 CFHR5") # split this (-_-) 
#df$`Gene symbol` <- str_replace_all(df$`Gene symbol`,"x" ," ") # (-_-) 

df <- tidyr::separate(df, `Gene symbol`, c("Gene symbol", "Gene symbol note"), " ")
df <- df %>% select(-  "Gene symbol note")

# set up URLs ----
# These URLs are only to show the text, but do not get used for the actual hyperlink. I add those within
# When using % for url, we nee to escape it for sprintf. Use doulbe (%%).
# We also have to escape for the reactable, and therefore use quadruple (%%%%).
df$UniProt <- sprintf('uniprot.org/%s', df$`Gene symbol`)
df$Ensembl <- sprintf('ensembl.org/%s', df$`Gene symbol`)
df$OMIM <- sprintf('omim.org/%s', df$`Gene symbol`)
df$GWAS <- sprintf('GWAS/%s', df$`Gene symbol`)
df$omni <- sprintf('omni/%s', df$`Gene symbol`)
df$'gnomAD r2 GRCh37' <- sprintf('gnomad.org/r2%s', df$`Gene symbol`)
df$'gnomAD r3 GRCh38' <- sprintf('gnomad.org/r3%s', df$`Gene symbol`)
df$pdb <- sprintf('rcsb.org/%s', df$`Gene symbol`)
df$HGNC <- sprintf('genenames.org/%s', df$`Gene symbol`)
df$ORPHA <- sprintf('hpo/ORPHA/%s', df$`Gene symbol`)
df$HPO <- sprintf('hpo.jax.org/%s', df$`Gene symbol`)
# df$DisProt <- sprintf('disprot.org/%s', df$`Gene symbol`)
df$AmiGo <- sprintf('amigo.org/%s', df$`Gene symbol`)
df$'Alpha Fold' <-  sprintf('alphafold/%s', df$`Gene symbol`)
df$Gemma <- sprintf('gemma/%s', df$`Gene symbol`)
df$ClinGen <- sprintf('clinicalgenome.org/%s', df$`Gene symbol`)

dft <- df %>%
	mutate( across(
		.cols = everything(),
		~str_replace( ., "\\(", "" )
	) ) %>%
	
	mutate( across(
		.cols = everything(),
		~str_replace( ., "\\)", "" )
	) )

df <- dft 

write.table((df), file='../data/10875_2022_1289_MOESM2_ESM_DLcleaned.tsv', sep="\t",  quote=FALSE, row.names=FALSE)

# reactable ----
library(reactable)
options(reactable.theme = reactableTheme(
	borderColor = "#dfe2e5",
	stripedColor = "#E5E5E5",
	highlightColor = "#fcf0e6",
	cellPadding = "8px 12px",
	style = list(fontFamily = "-apple-system, Arial, BlinkMacSystemFont, Segoe UI, Helvetica,  sans-serif",
					 fontSize = "1.0rem"),
	searchInputStyle = list(width = "50%")
))

# for value coloring, there can be no NAs (but character string "NA")

df_t <- 
	reactable( df,
				  compact = TRUE,
				  searchable = TRUE,
				  resizable = TRUE, 
				  # wrap = FALSE,
				  #elementId = "download-table",
				  defaultPageSize = 10,
				  filterable = TRUE,
				  showSortable = TRUE,
				  showPageSizeOptions = TRUE,
				  striped = TRUE,
				  highlight = TRUE,
				  defaultColDef = colDef(minWidth = 200),
				  columns = list(
				  	"Disease" = colDef(minWidth = 200), 
				  	"Inheritance" = colDef(minWidth = 140), 
				  	"Inheritance detail" = colDef(minWidth = 140), 
				  	"Major category" = colDef(minWidth = 300), 
				  	"Associated features" = colDef(minWidth = 200), 
				  	"T cell count" = colDef(minWidth = 200), 
				  	"B cell count" = colDef(minWidth = 200), 
				  	"Immunoglobulin levels" = colDef(minWidth = 200), 
				  	"Subcategory" = colDef(minWidth = 200), 
				  	"HPO table" = colDef(minWidth = 140), 				  	
				  	"HPO subtable" = colDef(minWidth = 140), 
				  	 "OMIM_ID" = colDef(minWidth = 140,
				  	                    cell = function(value, index) {
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

				  	"GWAS" = colDef(cell = function(value, index) {
				  		url <- sprintf("https://www.ebi.ac.uk/gwas/genes/%s", df[index, "Gene symbol"], value)
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
				  	   url <- sprintf("https://www.rcsb.org/search?request=%%7B%%22query%%22%%3A%%7B%%22type%%22%%3A%%22group%%22%%2C%%22nodes%%22%%3A%%5B%%7B%%22type%%22%%3A%%22group%%22%%2C%%22nodes%%22%%3A%%5B%%7B%%22type%%22%%3A%%22group%%22%%2C%%22nodes%%22%%3A%%5B%%7B%%22type%%22%%3A%%22terminal%%22%%2C%%22service%%22%%3A%%22full_text%%22%%2C%%22parameters%%22%%3A%%7B%%22value%%22%%3A%%22%s%%22%%7D%%7D%%5D%%2C%%22logical_operator%%22%%3A%%22and%%22%%7D%%5D%%2C%%22logical_operator%%22%%3A%%22and%%22%%2C%%22label%%22%%3A%%22full_text%%22%%7D%%5D%%2C%%22logical_operator%%22%%3A%%22and%%22%%7D%%2C%%22return_type%%22%%3A%%22entry%%22%%2C%%22request_options%%22%%3A%%7B%%22paginate%%22%%3A%%7B%%22start%%22%%3A0%%2C%%22rows%%22%%3A25%%7D%%2C%%22scoring_strategy%%22%%3A%%22combined%%22%%2C%%22sort%%22%%3A%%5B%%7B%%22sort_by%%22%%3A%%22score%%22%%2C%%22direction%%22%%3A%%22desc%%22%%7D%%5D%%7D%%2C%%22request_info%%22%%3A%%7B%%22query_id%%22%%3A%%2203e25d3f3eddbc329e8a3c568558431b%%22%%7D%%7D", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),
				  	 "ORPHA" = colDef(cell = function(value, index) {
				  	 	url <- sprintf(#"https://hpo.jax.org/app/browse/disease/ORPHA:%s"
				  	 	               "https://hpo.jax.org/app/browse/search?q=%s&navFilter=all", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),
				  	 "HPO" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://hpo.jax.org/app/browse/search?q=%s&navFilter=gene", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),
				  	 "HGNC" = colDef(cell = function(value, index) {
				  	 	url <- sprintf("https://www.genenames.org/tools/search/#!/?query=%s", df[index, "Gene symbol"], value)
				  	 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 }),
				  	# disprot does not recognise gene symbols
				  	 # "DisProt" = colDef(cell = function(value, index) {
				  	 # 	url <- sprintf("https://disprot.org/browse?sort_field=disprot_id&sort_value=asc&page_size=20&page=0&release=current&show_ambiguous=true&show_obsolete=false&free_text=%s", df[index, "Gene symbol"], value)
				  	 # 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 # }),
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
				  )
	)

df_t


install.packages("remotes")
remotes::install_github("kcuilla/reactablefmtr")


library(reactablefmtr)
save_reactable(df_t, "../output/iusis_iei_table_2022.html")


# Table of tables ----

#df2 <- df %>% select(- GWAS)

df_sub_t <- 
	reactable( df,
				  groupBy = "Major category",
				  #defaultExpanded = TRUE,
				  compact = TRUE,
				  searchable = TRUE,
				  resizable = TRUE, 
				  # wrap = FALSE,
				  #elementId = "download-table",
				  defaultPageSize = 50,
				  filterable = TRUE,
				  showSortable = TRUE,
				  showPageSizeOptions = TRUE,
				  striped = TRUE,
				  highlight = TRUE,
				  defaultColDef = colDef(minWidth = 640),
				  # columns = list(
				  # 	"Major category" = colDef( 
				  # 									style = function(value) {
				  # 											if (value == "Table 1 Immunodeficiencies affecting cellular and humoral immunity" ) {color <-  "#67074e"
				  # 										} else if (value == "Table 2 Combined immunodeficiencies with associated or syndromic features") {color <- "#69241b"
				  # 										} else if (value == "Table 3 Predominantly Antibody Deficiencies") {color <- "#1b3d49"
				  # 										} else if (value == "Table 4 Diseases of Immune Dysregulation") {color <- "#00567b"
				  # 										} else if (value == "Table 5 Congenital defects of phagocyte number or function" ) {color <- "#05878a"
				  # 										} else if (value == "Table 6 Defects in intrinsic and innate immunity" ) {color <- "#074e67"
				  # 										} else if (value == "Table 7 Autoinflammatory Disorders") {color <- "#5a175d"
				  # 										} else if (value == "Table 8 Complement Deficiencies") {color <- "#dd9933"
				  # 										} else if (value == "Table 9 Bone marrow failure") {color <- "#812b2f"
				  # 										} else { color <- "black"}
				  # 										list(color = color) })
				  # )
	)


df_sub_t 



df_sub_t <- 
	reactable( (df
				  %>% select(- GWAS)),
				  groupBy = "Major category",
				  compact = TRUE,
				  searchable = TRUE,
				  resizable = TRUE, 
				  # wrap = FALSE,
				  #elementId = "download-table",
				  defaultPageSize = 10,
				  filterable = TRUE,
				  showSortable = TRUE,
				  showPageSizeOptions = TRUE,
				  striped = TRUE,
				  highlight = TRUE,
				  # defaultColDef = colDef(minWidth = 640),
				  defaultColDef = colDef(minWidth = 200),
				  columns = list(
				  	"Disease" = colDef(minWidth = 200), 
				  	"Inheritance" = colDef(minWidth = 140), 
				  	"Inheritance detail" = colDef(minWidth = 140), 
				  	"Major category" = colDef(minWidth = 640), 
				  	"Associated features" = colDef(minWidth = 200), 
				  	"T cell count" = colDef(minWidth = 200), 
				  	"B cell count" = colDef(minWidth = 200), 
				  	"Immunoglobulin levels" = colDef(minWidth = 200), 
				  	"Subcategory" = colDef(minWidth = 200), 
				  	"HPO table" = colDef(minWidth = 140), 				  	
				  	"HPO subtable" = colDef(minWidth = 140), 
				  	"OMIM_ID" = colDef(minWidth = 140,
				  							 cell = function(value, index) {
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
				  	
				  	# "GWAS" = colDef(cell = function(value, index) {
				  	# 	url <- sprintf("https://www.ebi.ac.uk/gwas/genes/%s", df[index, "Gene symbol"], value)
				  	# 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	# }),
				  	
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
				  		url <- sprintf("https://www.rcsb.org/search?request=%%7B%%22query%%22%%3A%%7B%%22type%%22%%3A%%22group%%22%%2C%%22nodes%%22%%3A%%5B%%7B%%22type%%22%%3A%%22group%%22%%2C%%22nodes%%22%%3A%%5B%%7B%%22type%%22%%3A%%22group%%22%%2C%%22nodes%%22%%3A%%5B%%7B%%22type%%22%%3A%%22terminal%%22%%2C%%22service%%22%%3A%%22full_text%%22%%2C%%22parameters%%22%%3A%%7B%%22value%%22%%3A%%22%s%%22%%7D%%7D%%5D%%2C%%22logical_operator%%22%%3A%%22and%%22%%7D%%5D%%2C%%22logical_operator%%22%%3A%%22and%%22%%2C%%22label%%22%%3A%%22full_text%%22%%7D%%5D%%2C%%22logical_operator%%22%%3A%%22and%%22%%7D%%2C%%22return_type%%22%%3A%%22entry%%22%%2C%%22request_options%%22%%3A%%7B%%22paginate%%22%%3A%%7B%%22start%%22%%3A0%%2C%%22rows%%22%%3A25%%7D%%2C%%22scoring_strategy%%22%%3A%%22combined%%22%%2C%%22sort%%22%%3A%%5B%%7B%%22sort_by%%22%%3A%%22score%%22%%2C%%22direction%%22%%3A%%22desc%%22%%7D%%5D%%7D%%2C%%22request_info%%22%%3A%%7B%%22query_id%%22%%3A%%2203e25d3f3eddbc329e8a3c568558431b%%22%%7D%%7D", df[index, "Gene symbol"], value)
				  		htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	}),
				  	"ORPHA" = colDef(cell = function(value, index) {
				  		url <- sprintf(#"https://hpo.jax.org/app/browse/disease/ORPHA:%s"
				  			"https://hpo.jax.org/app/browse/search?q=%s&navFilter=all", df[index, "Gene symbol"], value)
				  		htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	}),
				  	"HPO" = colDef(cell = function(value, index) {
				  		url <- sprintf("https://hpo.jax.org/app/browse/search?q=%s&navFilter=gene", df[index, "Gene symbol"], value)
				  		htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	}),
				  	"HGNC" = colDef(cell = function(value, index) {
				  		url <- sprintf("https://www.genenames.org/tools/search/#!/?query=%s", df[index, "Gene symbol"], value)
				  		htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	}),
				  	# disprot does not recognise gene symbols
				  	# "DisProt" = colDef(cell = function(value, index) {
				  	# 	url <- sprintf("https://disprot.org/browse?sort_field=disprot_id&sort_value=asc&page_size=20&page=0&release=current&show_ambiguous=true&show_obsolete=false&free_text=%s", df[index, "Gene symbol"], value)
				  	# 	htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	# }),
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
				  )
	)

df_sub_t

# Color pallets are arbitrary, "Deep Autumn" and "Jewel Tones" from color-hex.com


