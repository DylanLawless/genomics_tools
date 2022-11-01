library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(stringr)

# Import data ----

file="../data/resources_cheatsheet.tsv"
df = read.csv(file, sep = "\t", comment.char = "#", header = TRUE, na.strings = "" )

names(df)

# Fill epty rows
df <- tidyr::fill(df, Database_category, Function)

d <- df %>%
	select(Database_category) %>%
	group_by(Database_category) %>%
	na.omit() %>%
	tally() # %>%	filter(n >1) 

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
	theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))  +
	scale_x_discrete(labels = function(x) str_wrap(x, width = 20))

ggsave("../output/resources_cheatsheet_category.pdf", width = 16, height = 8, units = "cm")
ggsave("../output/resources_cheatsheet_category.png", width = 16, height = 8, units = "cm")

names(df)
df <- df %>% select(Database_category, Function, Name, URL, everything())
colnames(df)[colnames(df) == 'Database_category'] <- "Database category"

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
				  	"URL" = colDef(minWidth = 300,
				  						cell = function(value, index) {
				  		url <- sprintf(df[index, "URL"], value)
				  		htmltools::tags$a(href = url, target = "_blank", as.character(value))
				  	 # htmltools::tags$a(href = url, target = "_blank", "link")
				  	}),
				  	"Database category" 	= colDef(minWidth = 250),				  	
				  	"Function" 	= colDef(minWidth = 300),
				  	"Name" 	= colDef(minWidth = 300),
				  	"Notes" 	= colDef(minWidth = 500)
				  	
				  ),
				 filterable = TRUE,
				 showSortable = TRUE,
				 showPageSizeOptions = TRUE,
				 striped = TRUE,
				 highlight = TRUE
	)

df_t

library(reactablefmtr)
save_reactable(df_t, "../output/resources_cheatsheet.html")

# clean html ----
system("grep -v '<!DOCTYPE html' ../output/resources_cheatsheet.html > ../output/resources_cheatsheet_clean.html")
# For multiple tables on one html page, the <script>s used cause both tables to disappear. Therefore for table 2, edit the output to only contain only the <body> ... </body> content from the bottom of the documen    t. The scripts in table 1 will be applied on both. You must also comment out the <!DOCTYPE html> for table 1. grep with the flag “-A” to print number of lines “After” match.
# grep -A 20 "<body " resources_cheatsheet.html | grep -v "</html>" > resources_cheatsheet_clean.html
