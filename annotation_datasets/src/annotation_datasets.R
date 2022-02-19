library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

# Import data ----

file="../data/datasources_compiled.tsv"
df = read.csv(file, sep = "\t", )


d <- df %>%
	group_by(Database_category) %>%
	tally() %>%
	filter(n >1) 

d <- d %>% arrange(desc(n)) 	
d$Database_category <- as.vector(d$Database_category) #get rid of factors
d$Database_category = factor(d$Database_category,d$Database_category) #add ordered factors back
d$Database_category = factor(d$Database_category) #add ordered factors back

d %>% 
	ggplot(aes(x = Database_category, y=n)) +
	geom_bar(stat="identity", color="black") +
	xlab("Database category") +
	ylab("Number of databases") +
	theme_bw() +
	theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

ggsave("../output/annotation_datasets_category.pdf", width = 16, height = 8, units = "cm")
ggsave("../output/annotation_datasets_category.png", width = 16, height = 8, units = "cm")

# number of annotation sources per category
# exuding uncategorized:
df %>%
	group_by(Database_category) %>%
	tally() %>%
	filter(n == 1) %>%
	tally()


names(df)
df <- df %>% select(-Description, -Source)
df <- df %>% select(Name, Database_category, everything(), URL)
colnames(df)[colnames(df) == 'Database_category'] <- "Database category"
colnames(df)[colnames(df) == 'Usage'] <- "In use"
#library(stringr)
#df$Usage <- str_replace(df$Usage, "yes", "Yes")




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
				  defaultPageSize = 170,
				  defaultColDef = colDef(minWidth = 140),
				  columns = list(
				  	"In Use" = colDef(maxWidth = 40), 
				  	"URL" = colDef(minWidth = 300,
				  						cell = function(value, index) {
				  		url <- sprintf(df[index, "URL"], value)
				  		htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 # htmltools::tags$a(href = url, target = "_blank", "link")
				  	}),
				  	"In use" = colDef(cell = function(value) {
				  		# Render as an X mark or check mark
				  		if (value == "No") "\u274c No" else "\u2714\ufe0f Yes"
				  	})
				  	
				  ),
				 filterable = TRUE,
				 showSortable = TRUE,
				 showPageSizeOptions = TRUE,
				 striped = TRUE,
				 highlight = TRUE
	)

df_t
library(reactablefmtr)
save_reactable(df_t, "../output/annotation_datasets_table.html")
