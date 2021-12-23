library(lubridate)
library(dplyr)
library(stringr)
#library(ggplot2)
#library(plotly)

# get gene list ----
# Which source should we use to define "all genes"?
# https://www.genecards.org/List/Statistics#ProteinCoding

# genes from String db
#protein.info.v11.5.txt.gz (1.4 Gb)	list of STRING proteins incl. their display names and descriptions
# grep "^9606" protein.info.v11.5.txt > string.protein.info.v11.5.9606.txt

# genes from BioMart Ensembl GRCh37 release 105 
#https://grch37.ensembl.org/biomart/martview/7d9bf7937836726d33d926806f92a978

#Dataset 63737 / 63737 Genes
#Human genes (GRCh37.p13)
#UniProtKB Gene Name symbol
#HGNC ID

df_genes <- 
  read.table(file="../data/BioMartEnsemblGRCh37release105_UniProtKBGeneNameSymbol_HGNCid.csv", 
             header = TRUE,
             sep = ",", 
             stringsAsFactors = FALSE)

# replace empty
df_genes[df_genes==""]<-NA
df_genes <- na.omit(df_genes)
colnames(df_genes)[colnames(df_genes) == 'HGNC.ID'] <- 'HGNC'
colnames(df_genes)[colnames(df_genes) == 'UniProtKB.Gene.Name.symbol'] <- 'Gene symbol'
df_genes <- df_genes %>% unique()

# import ----
df <- 
  read.table(file="../data/Gene-Disease Validity-2021-11-24.csv", 
             header = TRUE,
             skip = 4, # skip the info head
             sep = ",", 
             stringsAsFactors = FALSE) %>% 
  slice(-c(1)) # remove the row of symbols

df2 <- 
  read.table(file="../data/ClinGen Curation Activity Summary Report-2021-11-24.csv", 
             header = TRUE,
             skip = 3, # skip the info head
             sep = ",", 
             stringsAsFactors = FALSE)

# tidy ----
# Use lubridate for the Complete ISO-8601 date
df$CLASSIFICATION.YEAR <- 
  year(lubridate::as_datetime(df$CLASSIFICATION.DATE))

# clean names
df <- df %>% select(-CLASSIFICATION.DATE)

df$GENE.ID..HGNC. <- str_replace(df$GENE.ID..HGNC., "HGNC:", "") 
df$DISEASE.ID..MONDO. <- str_replace(df$DISEASE.ID..MONDO., "MONDO:", "") 
df$SOP <- str_replace(df$SOP, "SOP", "")

names(df) <- str_to_sentence(names(df), locale = "en")
df$Classification <- str_replace(df$Classification, "No Known Disease Relationship", "No Known") 


names(df) <- str_replace_all(names(df),"\\.\\."," ")
names(df) <- str_replace_all(names(df),"\\."," ")

colnames(df)[colnames(df) == 'Gcep'] <- 'GCEP'
colnames(df)[colnames(df) == 'Gene id Hgnc '] <- 'HGNC'
colnames(df)[colnames(df) == 'Sop'] <- 'SOP'
colnames(df)[colnames(df) == 'Moi'] <- 'MOI'
colnames(df)[colnames(df) == 'Disease id Mondo '] <- 'MONDO'
colnames(df)[colnames(df) == 'Online report'] <- 'ClinGen'
colnames(df)[colnames(df) == 'Classification'] <- 'ClinGen classification'


colnames(df2)[colnames(df2) == 'disease_label'] <- 'Disease label'
colnames(df2)[colnames(df2) == 'gene_symbol'] <- 'Gene symbol'
colnames(df2)[colnames(df2) == 'hgnc_id'] <- 'HGNC'
df2$HGNC <- str_replace(df2$HGNC, "HGNC:", "") 
colnames(df2)[colnames(df2) == 'mondo_id'] <- 'MONDO'
df2$MONDO <- str_replace(df2$MONDO, "MONDO:", "") 
colnames(df2)[colnames(df2) == 'mode_of_inheritance'] <- 'MOI'
colnames(df2)[colnames(df2) == 'dosage_haploinsufficiency_assertion'] <- 'Haploinsufficiency classification'


df2$MOI <- str_replace_all(df2$MOI,"Autosomal recessive inheritance","AR")
df2$MOI <- str_replace_all(df2$MOI,"Autosomal dominant inheritance","AD")
df2$MOI <- str_replace_all(df2$MOI,"X-linked inheritance","XL")
df2$MOI <- str_replace_all(df2$MOI,"Semidominant mode of inheritance","Semi-AD")
df2$MOI <- str_replace_all(df2$MOI,"N/A",'NA')
df2$MOI <- str_replace_all(df2$MOI,"Mode of inheritance",'NA') # note to do this last - probably an error from clingen

# split date from classification
df2[c('Haploinsufficiency classification', 'Haploinsufficiency classification date')] <- str_split_fixed(df2$`Haploinsufficiency classification`, "[()]", 2)

df2$`Haploinsufficiency classification` <- str_replace_all(df2$`Haploinsufficiency classification`,"0 - No Evidence for Haploinsufficiency ",'No evidence') 
df2$`Haploinsufficiency classification` <- str_replace_all(df2$`Haploinsufficiency classification`,"1 - Little Evidence for Haploinsufficiency ",'Little evidence') 
df2$`Haploinsufficiency classification` <- str_replace_all(df2$`Haploinsufficiency classification`,"2 - Emerging Evidence for Haploinsufficiency ",'Emerging Evidence') 
df2$`Haploinsufficiency classification` <- str_replace_all(df2$`Haploinsufficiency classification`,"3 - Sufficient Evidence for Haploinsufficiency ",'Sufficient Evidence') 
df2$`Haploinsufficiency classification` <- str_replace_all(df2$`Haploinsufficiency classification`,"30 - Gene Associated with Autosomal Recessive Phenotype ",'Assoc with AR') 
df2$`Haploinsufficiency classification` <- str_replace_all(df2$`Haploinsufficiency classification`,"40 - Dosage Sensitivity Unlikely ",'Unlikely') 
df2$`Haploinsufficiency classification` %>% unique()


df2 <- df2 %>% select(`Gene symbol`, HGNC, MOI, MONDO, `Haploinsufficiency classification`, `Disease label`)

df3 <- merge(df, df2, all=TRUE)
df4 <- merge(df_genes, df3, all=TRUE)
df <- df4
# Note this is where I use df4 in place of the origin df. 



# lnk to uniprot ----
#https://www.uniprot.org/uniprot/?query=gene:rag1&fil=organism%3A%22Homo+sapiens+%28Human%29+%5B9606%5D%22&sort=score

# When using % for url, we nee to escape it for sprintf. Use doulbe (%%).
# We also have to escape for the reactable, and therefore use quadruple (%%%%).
df$UniProt <- sprintf('https://www.uniprot.org/uniprot/?query=gene:%s&fil=organism%%%%3A%%%%22Homo+sapiens+%%%%28Human%%%%29+%%%%5B9606%%%%5D%%%%22&sort=score', df$`Gene symbol`)

df$Ensembl <- sprintf('https://www.ensembl.org/Human/Search/Results?q=%s;site=ensembl;facet_species=Human', df$`Gene symbol`)

df$OMIM <- sprintf('https://www.omim.org/search?index=entry&sort=score+desc%%%%2C+prefix_sort+desc&start=1&limit=10&search=%s', df$`Gene symbol`)

df$omni <- sprintf('https://omni.institutimagine.org/search=%s/page=1', df$`Gene symbol`)
df$'gnomAD r2 GRCh37' <- sprintf('https://gnomad.broadinstitute.org/gene/%s?dataset=gnomad_r2_1', df$`Gene symbol`)
df$'gnomAD r3 GRCh38' <- sprintf('https://gnomad.broadinstitute.org/gene/%s?dataset=gnomad_r3', df$`Gene symbol`)
df$pdb <- sprintf('https://www.rcsb.org/search?request=%%%%7B%%%%22query%%%%22%%%%3A%%%%7B%%%%22type%%%%22%%%%3A%%%%22group%%%%22%%%%2C%%%%22nodes%%%%22%%%%3A%%%%5B%%%%7B%%%%22type%%%%22%%%%3A%%%%22group%%%%22%%%%2C%%%%22nodes%%%%22%%%%3A%%%%5B%%%%7B%%%%22type%%%%22%%%%3A%%%%22group%%%%22%%%%2C%%%%22nodes%%%%22%%%%3A%%%%5B%%%%7B%%%%22type%%%%22%%%%3A%%%%22terminal%%%%22%%%%2C%%%%22service%%%%22%%%%3A%%%%22full_text%%%%22%%%%2C%%%%22parameters%%%%22%%%%3A%%%%7B%%%%22value%%%%22%%%%3A%%%%22%s%%%%22%%%%7D%%%%7D%%%%5D%%%%2C%%%%22logical_operator%%%%22%%%%3A%%%%22and%%%%22%%%%7D%%%%5D%%%%2C%%%%22logical_operator%%%%22%%%%3A%%%%22and%%%%22%%%%2C%%%%22label%%%%22%%%%3A%%%%22full_text%%%%22%%%%7D%%%%5D%%%%2C%%%%22logical_operator%%%%22%%%%3A%%%%22and%%%%22%%%%7D%%%%2C%%%%22return_type%%%%22%%%%3A%%%%22entry%%%%22%%%%2C%%%%22request_options%%%%22%%%%3A%%%%7B%%%%22pager%%%%22%%%%3A%%%%7B%%%%22start%%%%22%%%%3A0%%%%2C%%%%22rows%%%%22%%%%3A25%%%%7D%%%%2C%%%%22scoring_strategy%%%%22%%%%3A%%%%22combined%%%%22%%%%2C%%%%22sort%%%%22%%%%3A%%%%5B%%%%7B%%%%22sort_by%%%%22%%%%3A%%%%22score%%%%22%%%%2C%%%%22direction%%%%22%%%%3A%%%%22desc%%%%22%%%%7D%%%%5D%%%%7D%%%%2C%%%%22request_info%%%%22%%%%3A%%%%7B%%%%22query_id%%%%22%%%%3A%%%%228fb30ed1650bdcc2f59067e304229b28%%%%22%%%%7D%%%%7D', df$`Gene symbol`)

df$HGNC <- sprintf('https://www.genenames.org/tools/search/#!/?query=HGNC:%s', df$`HGNC`)

df$ORPHA <- sprintf('https://hpo.jax.org/app/browse/disease/ORPHA:%s', df$`Gene symbol`)

df$HPO <- sprintf('https://hpo.jax.org/app/browse/search?q=%s&navFilter=all', df$`Gene symbol`)

df$DisProt <- sprintf('https://disprot.org/browse?sort_field=disprot_id&sort_value=asc&page_size=20&page=0&release=current&show_ambiguous=true&show_obsolete=false&free_text=%s', df$`Gene symbol`)

df$AmiGo <- sprintf('http://amigo.geneontology.org/amigo/search/bioentity?q=%s&searchtype=geneproduct', df$`Gene symbol`)

df$'Alpha Fold' <-  sprintf('https://www.alphafold.ebi.ac.uk/search/text/%s', df$`Gene symbol`)

df$Gemma <- sprintf('https://gemma.msl.ubc.ca/searcher.html?query=%s&scope=G', df$`Gene symbol`)

# for genes we no validity yet, return a quiry for clingen: first make a search for every gene row
df$ClinGen_search <- sprintf('https://search.clinicalgenome.org/kb/genes?page=1&size=50&search=%s', df$`Gene symbol`)

# then coalesce (dplyr) for any NA in the url ClinGen column: result only the validated genes has a "gene-validity" url which everything else returns a surch. 
df <- df%>% mutate(ClinGen = coalesce(ClinGen,ClinGen_search)) 

# reorder
df <- df %>% select(
  "Gene symbol",
  "MOI",
  "ClinGen classification",
  "Disease label",
  "GCEP", 
  "ClinGen", "gnomAD r2 GRCh37", "gnomAD r3 GRCh38", "UniProt", "Ensembl", "OMIM", "omni", "pdb", "DisProt", "AmiGo", "Alpha Fold", "ORPHA", "HPO", "HGNC", "Gemma",
  "MONDO",
  "SOP",
  "Classification year")

library(tidyr)
df$`ClinGen classification` <- replace_na(df$`ClinGen classification`, "") 


# color theme ----
# Disputed #962fbf insta purple
# LIMITED	1-6 gold 	#ffbf00 gold
# MODERATE	7-11 orange #fa7e1e insta orange
# No Known Disease Relationship #d62976 inst pink
# DEFINITIVE	12-18 green #339900 dark
# STRONG	12-18 green 	#99cc33 light
# Refuted #4f5bd5 insta blue

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

df_t <- 
  reactable(df,
            compact = TRUE,
            searchable = TRUE,
            #elementId = "download-table",
            defaultPageSize = 10,
            defaultColDef = colDef(minWidth = 93),
            columns = list(
              "Disease label" = colDef(minWidth = 200),  # overrides the default
              "GCEP" = colDef(minWidth = 200), 
              "SOP" = colDef(minWidth = 70), 
              "ClinGen" = colDef(cell = function(value, index) {
                url <- sprintf(df[index, "ClinGen"], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              "UniProt" = colDef(cell = function(value, index) {
                url <- sprintf(df[index, 'UniProt'], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              "Ensembl" = colDef(cell = function(value, index) {
                url <- sprintf(df[index, 'Ensembl'], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              "OMIM" = colDef(cell = function(value, index) {
                url <- sprintf(df[index, 'OMIM'], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              "gnomAD r2 GRCh37" = colDef(cell = function(value, index) {
                url <- sprintf(df[index, 'gnomAD r2 GRCh37'], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              "gnomAD r3 GRCh38" = colDef(cell = function(value, index) {
                url <- sprintf(df[index, 'gnomAD r3 GRCh38'], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              "omni" = colDef(cell = function(value, index) {
                url <- sprintf(df[index, 'omni'], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              "pdb" = colDef(cell = function(value, index) {
                url <- sprintf(df[index, 'pdb'], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              "ORPHA" = colDef(cell = function(value, index) {
                url <- sprintf(df[index, 'ORPHA'], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              "HPO" = colDef(cell = function(value, index) {
                url <- sprintf(df[index, 'HPO'], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              "HGNC" = colDef(cell = function(value, index) {
                url <- sprintf(df[index, 'HGNC'], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              "DisProt" = colDef(cell = function(value, index) {
                url <- sprintf(df[index, 'DisProt'], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              "AmiGo" = colDef(cell = function(value, index) {
                url <- sprintf(df[index, 'AmiGo'], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              "Alpha Fold" = colDef(cell = function(value, index) {
                url <- sprintf(df[index, 'Alpha Fold'], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              "Gemma" = colDef(cell = function(value, index) {
                url <- sprintf(df[index, 'Gemma'], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              
              'ClinGen classification' = colDef( minWidth = 130,
                                       style = function(value) {
                                         if (value == "Disputed") {color <- "#962fbf"
                                         } else if (value == "Limited") {color <- "#e5ab00"
                                         } else if (value == "Moderate") {color <- "#fa7e1e"
                                         } else if (value == "No Known") {color <- "#d62976"
                                         } else if (value == "Definitive") {color <- "#339900"
                                         } else if (value == "Strong") {color <- "#99cc33"
                                         } else if (value == "Refuted") {color <- "#4f5bd5"
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
# clingen-gene_disease_validity_table_all_genes.html



# I would like a sparkiline footer for MOI
df_head <- df %>% head(100)
tbl <- with(df_head, table(MOI ))
tbl
barplot(tbl, legend = FALSE)
tbl <- as.data.frame(tbl)
library(reactablefmtr)
library(tidyverse)
library(sparkline)
library(dataui)

reactable(
  tbl,
  pagination = FALSE,
  defaultColDef = colDef(cell = data_bars(tbl),
                        footer = function(values) {
    if (!is.numeric(values)) return()
    sparkline(values, type = "bar", width = 100, height = 30)
  })
)

library(reactR)
reactable(
  tbl,
  pagination = FALSE,
  defaultColDef = colDef(cell = data_bars(tbl),
                         footer = function(values) {
                           if (!is.numeric(values)) return()
                           sparkline(values, type = "box", width = 100, height = 30)
                         })
)


reactable(
  tbl,
  columns = list(
    MOI = colDef(maxWidth = 85),
    MOI = colDef(
      cell = react_sparkline(tbl)
    )
  )
)



reactable(
  tbl,
  columns = list(
    MOI = colDef(maxWidth = 85),
    Freq = colDef(
      cell = react_sparkbar(
        tbl
      )
    )
  )
)


library(palmerpenguins)
df <- penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%
  summarize(flipper_length = list(flipper_length_mm))

reactable(
  df,
  columns = list(
    species = colDef(maxWidth = 85),
    sex = colDef(maxWidth = 85),
    flipper_length = colDef(
      cell = react_sparkbar(df)
    )
  )
)
# crosstalk ----
#Note: bsCols() Seems to completely override the flexdashboard CSS and can't be used on website
# make the data crosstalk for a checklist filter
library(crosstalk)
data <- SharedData$new(df)

df_b <- bscols( widths = c(2, 9),
  ( filter_checkbox("ClinGen classification", "ClinGen classification", data, ~'ClinGen classification')),
 df_t,
 device = c( "sm"))

checkbox <- filter_checkbox("ClinGen classification", "ClinGen classification", data, ~'ClinGen classification')
htmltools::browsable(df_t, checkbox)

# download button
library(htmltools)
library(fontawesome)
htmltools::browsable(
  tagList(
    tags$button(
      tagList(fontawesome::fa("download"), "Download as CSV"),
      onclick = "Reactable.downloadDataCSV('download-table', 'cars.csv')"
    ),
    
    df_b
  )
)



