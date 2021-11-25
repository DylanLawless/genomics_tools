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

# reorder
df <- df %>% select(
"Gene symbol",
"MOI",
"Classification",
"Online report",
"Disease label",
"GCEP",
"HGNC",
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

#cols_class <- c("#962fbf", "#ffbf00", "#fa7e1e", "#d62976", "#339900", "#99cc33", "#4f5bd5")

# cols_instagram <- c("#feda75", "#fa7e1e", "#d62976", "#962fbf", "#4f5bd5")
# warning, 2 red, yellow, 2 green
# cols_warn <- c("#cc3300", "#ff9966", "#ffcc00", "#99cc33", "#339900")

# classifications
# cols_names <- c(unique(df$Classification))
#ColourScale_groups <- data.frame(cols_names, cols_class)
#ColourScale_groups <- t(ColourScale_groups) %>% janitor::row_to_names(1)
#rownames(ColourScale_groups)<-NULL

# order the click filter 
#Classification <- c("No Known",  "Limited", "Refuted", "Disputed",  "Moderate",  "Strong", "Definitive")
#Classification_level <- as.numeric(c(1,2,3,4,5,6,7))
#Classification_levels <- data.frame(Classification, Classification_level)
# library(tidyr)
# df <- merge(df, Classification_levels)
# df <- unite(df, Classification_level, Classification, col = "Classification", sep = ". ")

# data table ---- 
# does not render the href
# DT: An R interface to the DataTables library
# library(DT)
# df_d <- datatable(head(df), 
#          class = 'compact stripe',
#          filter = 'top', options = list(
#            pageLength = 25, autoWidth = TRUE,
#            escape = FALSE))
# df_d

# reactable ----
library(reactable)
options(reactable.theme = reactableTheme(
  borderColor = "#dfe2e5",
  stripedColor = "#fcf0e6",
  highlightColor = "#f9e2cf",
  cellPadding = "8px 12px",
  style = list(fontFamily = "-apple-system, Arial, BlinkMacSystemFont, Segoe UI, Helvetica,  sans-serif",
               fontSize = "1.0rem"),
  searchInputStyle = list(width = "50%")
))

df_t <- 
  reactable(data,
            compact = TRUE,
            searchable = TRUE,
            #elementId = "download-table",
            defaultPageSize = 10,
            defaultColDef = colDef(minWidth = 90 ),
            columns = list(
              "Disease label" = colDef(minWidth = 200),  # overrides the default
              "GCEP" = colDef(minWidth = 200), 
              "SOP" = colDef(minWidth = 70), 
              "Online report" = colDef(cell = function(value, index) {
                # Render as a link
                url <- sprintf(df[index, "Online report"], value)
                htmltools::tags$a(href = url, target = "_blank", "link")
              }),
              
              Classification = colDef( minWidth = 130,
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
  ( filter_checkbox("Classification", "Classification", data, ~Classification)),
 df_t,
 device = c( "sm"))

checkbox <- filter_checkbox("Classification", "Classification", data, ~Classification)
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



