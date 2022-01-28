library(lubridate)
library(dplyr)
library(stringr)
#library(ggplot2)
#library(plotly)

df <- read.csv(file="../data/acgm_criteria_table.txt", sep= "\t")
df2 <- read.csv(file="../data/acgm_criteria_table_caveats.txt", sep= "\t")

colnames(df)[colnames(df) == 'Evidence_type'] <- 'Evidence type'
colnames(df)[colnames(df) == 'ACGM_label'] <- 'ACGM label'
colnames(df)[colnames(df) == 'Caveat_checks'] <- 'Caveat checks'
colnames(df)[colnames(df) == 'Manual_adjustment'] <- 'Manual adjustment'

colnames(df2)[colnames(df2) == 'Evidence_type'] <- 'Evidence type'
colnames(df2)[colnames(df2) == 'ACGM_label'] <- 'ACGM label'
colnames(df2)[colnames(df2) == 'Caveat_number'] <- 'Caveat number'
colnames(df2)[colnames(df2) == 'Manual_adjustment'] <- 'Manual adjustment'

names(df)
names(df2)
#colnames(df)[colnames(df) == 'Classification'] <- 'ClinGen classification'
#f2$MOI <- str_replace_all(df2$MOI,"Autosomal recessive inheritance","AR")
#df3 <- merge(df, df2, all=TRUE)

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
  reactable( df,
             compact = TRUE,
             searchable = TRUE,
             #elementId = "download-table",
             defaultPageSize = 10,
             defaultColDef = colDef(minWidth = 40),
             columns = list(
               "Manual adjustment" = colDef(minWidth = 60),  # overrides the default
               "Criteria" = colDef(minWidth = 180),  # overrides the default
               "Evidence type" = colDef(minWidth = 60,
                                        style = function(value) {
                                          if (value == "pathogenicity") {color <- "#cc0000"
                                          } else if (value == "benign") {color <- "#674ea7"
                                          } else { color <- "black"}
                                          list(color = color) }
                                        ), 
               'Evidence' = colDef( minWidth = 60,
                                                  style = function(value) {
                                                    if (value == "Disputed") {color <- "#962fbf"


                                                    } else if (value == "very_strong") {color <- "#309e00"
                                                    } else if (value == "strong") {color <- "#47be02"
                                                    } else if (value == "moderate") {color <- "#006fc7"
                                                    } else if (value == "supporting") {color <- "#0098b9"
                                                    } else if (value == "other") {color <- "#e97d0d"
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

df_t2 <- 
  reactable( df2,
             compact = TRUE,
             searchable = TRUE,
             #elementId = "download-table",
             defaultPageSize = 10,
             defaultColDef = colDef(minWidth = 40),
             columns = list(
               "Manual adjustment" = colDef(minWidth = 60),  # overrides the default
               "Caveat" = colDef(minWidth = 180),  # overrides the default
               "Evidence type" = colDef(minWidth = 60,
                                        style = function(value) {
                                          if (value == "pathogenicity") {color <- "#cc0000"
                                          } else if (value == "benign") {color <- "#674ea7"
                                          } else { color <- "black"}
                                          list(color = color) }
               ), 
               'Evidence' = colDef( minWidth = 60,
                                    style = function(value) {
                                      if (value == "Disputed") {color <- "#962fbf"
                                      
                                      
                                      } else if (value == "very_strong") {color <- "#309e00"
                                      } else if (value == "strong") {color <- "#47be02"
                                      } else if (value == "moderate") {color <- "#006fc7"
                                      } else if (value == "supporting") {color <- "#0098b9"
                                      } else if (value == "other") {color <- "#e97d0d"
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

df_t2



x
library(reactablefmtr)
save_reactable(df_t, "../output/acgm_criteria_table.html")
save_reactable(df_t2, "../output/acgm_criteria_table_caveat.html")
x



























library(reactablefmtr)
save_reactable(df_t, "../output/gene_disease_validity_table_all_genes.html")
# clingen-gene_disease_validity_table_all_genes.html

tmp <- df$`Gene symbol` %>% unique()
rm(tmp)
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



