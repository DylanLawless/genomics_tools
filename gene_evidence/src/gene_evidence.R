library(lubridate)
library(dplyr)
library(stringr)
#library(ggplot2)
#library(plotly)

# import ----
df <- 
  read.table(file="../data/Gene-Disease Validity-2021-11-24.csv", 
             header = TRUE,
             skip = 4, # skip the info head
             sep = ",", 
             stringsAsFactors = FALSE) %>% 
  slice(-c(1)) # remove the row of symbols

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
colnames(df)[colnames(df) == 'Online report'] <- 'CliGen'
colnames(df)[colnames(df) == 'Classification'] <- 'ClinGen classification'


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

# reorder
df <- df %>% select(
  "Gene symbol",
  "MOI",
  "ClinGen classification",
  "Disease label",
  "GCEP",
  "gnomAD r2 GRCh37", "gnomAD r3 GRCh38", "UniProt", "Ensembl", "OMIM", "omni", "pdb", "DisProt", "AmiGo", "Alpha Fold", "ORPHA", "HPO", "HGNC", "Gemma",
  "MONDO",
  "SOP",
  "Classification year")


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
            defaultColDef = colDef(minWidth = 93 ),
            columns = list(
              "Disease label" = colDef(minWidth = 200),  # overrides the default
              "GCEP" = colDef(minWidth = 200), 
              "SOP" = colDef(minWidth = 70), 
              "CliGen" = colDef(cell = function(value, index) {
                # Render as a link
                url <- sprintf(df[index, "CliGen"], value)
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



