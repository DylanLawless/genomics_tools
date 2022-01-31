library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

# Import data ----

file="../data/datasources_compiled.tsv"
df = read.csv(file, sep = "\t", )

d <- df %>%
	group_by(Info) %>%
	tally() %>%
	filter(n >1) 

d <- d %>% arrange(desc(n)) 	
d$Info <- as.vector(d$Info) #get rid of factors
d$Info = factor(d$Info,d$Info) #add ordered factors back

d %>% 
	ggplot(aes(x = Info, y=n)) +
	geom_bar(stat="identity") +
	theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

ggsave("../output/annotation_datasets_category.pdf", width = 20, height = 10, units = "cm")

# number of annotation sources per category
# exuding uncategorized:
df %>%
	group_by(Info) %>%
	tally() %>%
	filter(n == 1) %>%
	tally()


names(df)
df <- df %>% select(-number, -Source)

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
	reactable(df,
				  compact = TRUE,
				  searchable = TRUE,
				  elementId = "download-table",
				  defaultPageSize = 300,
				  defaultColDef = colDef(minWidth = 93),
				  columns = list(
				  	"Info" = colDef(minWidth = 140), 
				  	"URL" = colDef(cell = function(value, index) {
				  		url <- sprintf(df[index, "URL"], value)
				  		htmltools::tags$a(href = url, target = "_blank", "link")
				  	}),
				  filterable = TRUE,
				  showSortable = TRUE,
				  showPageSizeOptions = TRUE,
				  striped = TRUE,
				  highlight = TRUE
	))

df_t
library(reactablefmtr)
save_reactable(df_t, "../output/annotation_datasets_table.html")


