library(lubridate)
library(dplyr)
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

df <- df %>% select(-CLASSIFICATION.DATE)

df$GENE.ID..HGNC. <- str_replace(df$GENE.ID..HGNC., "HGNC:", "") 
df$DISEASE.ID..MONDO. <- str_replace(df$DISEASE.ID..MONDO., "MONDO:", "") 

library(stringr)
names(df) <- str_to_sentence(names(df), locale = "en")
df$Classification <- str_replace(df$Classification, "No Known Disease Relationship", "No Known") 

# reorder
df <- df %>% select(-"Disease.id..Mondo.", -"Gene.id..Hgnc.", -"Sop", "Disease.id..Mondo.", "Gene.id..Hgnc.", "Sop")

# clean names
names(df) <- str_replace_all(names(df),"\\.\\."," ")
names(df) <- str_replace_all(names(df),"\\."," ")


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
cols_names <- c(unique(df$Classification))
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
  stripedColor = "#fefcf5",
  highlightColor = "#fefafc",
  cellPadding = "8px 12px",
  style = list(fontFamily = "-apple-system, Arial, BlinkMacSystemFont, Segoe UI, Helvetica,  sans-serif",
               fontSize = "0.8rem"),
  searchInputStyle = list(width = "100%")
))

# turn the Online.report column into href links, which DT can escape
df_r <- reactable(df, columns = list(
  "Online report" = colDef(cell = function(value, index) {
    # Render as a link
    url <- sprintf(df[index, "Online report"], value)
    #htmltools::tags$a(href = url, target = "_blank", as.character(value))
    htmltools::tags$a(href = url, target = "_blank", "link")
  }),
  
  Classification = colDef(
    style = function(value) {
      if (value == "Disputed") {color <- "#962fbf"
      } else if (value == "Limited") {color <- "#ffbf00"
      } else if (value == "Moderate") {color <- "#fa7e1e"
      } else if (value == "No Known") {color <- "#d62976"
      } else if (value == "Definitive") {color <- "#339900"
      } else if (value == "Strong") {color <- "#99cc33"
      } else if (value == "Refuted") {color <- "#4f5bd5"
} else { color <- "black"}
      list(color = color)
    }
  )
  
),
filterable = TRUE,
showPageSizeOptions = TRUE,
striped = TRUE,
highlight = TRUE,
#details = function(index) paste("Details for row", index)
defaultPageSize = 25)

df_r

htmltools::browsable(
  tagList(
    tags$button(
      tagList(fontawesome::fa("download"), "Download as CSV"),
      onclick = "Reactable.downloadDataCSV('cars-download-table', 'cars.csv')"), df_r))

# crosstalk ----
# make the data crosstalk for a checklist filter
library(crosstalk)
data <- SharedData$new(df)

df_b <- bscols(
  widths = c(2, 9),
  list(
    filter_checkbox("Classification", "Classification", data, ~Classification)),
  
  reactable(data,
            compact = TRUE,
            searchable = TRUE,
            #elementId = "cars-download-table",
            defaultPageSize = 25,
            columns = list(
    "Online report" = colDef(cell = function(value, index) {
      # Render as a link
      url <- sprintf(df[index, "Online report"], value)
      htmltools::tags$a(href = url, target = "_blank", "link")
    }),
    
    Classification = colDef(
      style = function(value) {
        if (value == "Disputed") {color <- "#962fbf"
        } else if (value == "Limited") {color <- "#ffbf00"
        } else if (value == "Moderate") {color <- "#fa7e1e"
        } else if (value == "No Known") {color <- "#d62976"
        } else if (value == "Definitive") {color <- "#339900"
        } else if (value == "Strong") {color <- "#99cc33"
        } else if (value == "Refuted") {color <- "#4f5bd5"
        } else { color <- "black"}
        list(color = color)
      }
    )
   
  ),
  filterable = TRUE,
  showPageSizeOptions = TRUE,
  striped = TRUE,
  highlight = TRUE
  ),
  device = c( "sm")
)

df_b

# if this is not in use, remove from df_b "elementId = "cars-download-table""
htmltools::browsable(
  tagList(
    tags$button(
      tagList(fontawesome::fa("download"), "Download as CSV"),
      onclick = "Reactable.downloadDataCSV('cars-download-table', 'cars.csv')"), df_b))

# clingen-gene_disease_validity

