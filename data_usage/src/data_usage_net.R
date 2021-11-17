library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)

# Data usage tracking
# caption: Long-term projects use data from many sources. Data may be reused multiple times, and some planned analysis is not required. Data tracking systems can show these analytics and improve multipurpose use.


# import ----
df <- 
  read.table(file="../data/data_usage.tsv", 
             header = TRUE, 
             sep = "\t", 
             stringsAsFactors = FALSE)

df_2 <- 
  read.table(file="../data/data_usage_2.tsv", 
             header = TRUE, 
             sep = "\t", 
             stringsAsFactors = FALSE)

# df 1 ----
df$Date <- ymd(df$date)
df$fileuse <- as.factor(df$fileuse)
df$fileuse <- factor(df$fileuse, levels = c("Yes","No"))
d <- highlight_key(df, ~datatype)

df_2$Date <- ymd(df_2$date)
df_2$fileuse <- as.factor(df_2$fileuse)
df_2$fileuse <- factor(df_2$fileuse, levels = c("Yes","No"))
d_2 <- highlight_key(df_2, ~datatype)

p <- d %>%
  ggplot( aes(x=Date, 
              y=log10(size_GB), 
              fill=datatype,
              shape=fileuse,
              label=data_purpose,
              label2=size_GB
              )) + 
  geom_point(aes( size=log10(size_GB) ),
             position = position_jitter(width = 10, height = 0.5)) +
  facet_grid(fileuse ~.) +
  theme_classic()


gg <- ggplotly(p, tooltip = c("label2", "label", "fill") ) 
cols <- (RColorBrewer::brewer.pal(8, "Dark2"))

s <- attrs_selected(
  showlegend = TRUE,
  mode = "lines+markers",
  marker = list(symbol = "x"),
  color = cols, 
  dynamic = TRUE
)

highlight(layout(gg, showlegend = TRUE), selected = s)

# df 2 ----
p_2 <- d_2 %>%
  ggplot( aes(x=Date, 
              y=log10(size_GB), 
              fill=datatype,
              shape=fileuse,
              label=data_purpose,
              label2=size_GB
  )) + 
  geom_point(aes( size=log10(size_GB) ),
             position = position_jitter(width = 10, height = 0.5)) +
  facet_grid(fileuse ~.) +
  theme_classic()


gg_2 <- ggplotly(p_2, tooltip = c("label2", "label", "fill") ) 
cols <- (RColorBrewer::brewer.pal(8, "Dark2"))

s <- attrs_selected(
  showlegend = TRUE,
  mode = "lines+markers",
  marker = list(symbol = "x"),
  color = cols, 
  dynamic = TRUE
)

highlight(layout(gg_2, showlegend = TRUE), selected = s)


# df merged ----
df_bind <- rbind(df, df_2)
dd <- highlight_key(df_bind, ~datatype)

p_merge <- dd %>%
  ggplot( aes(x=Date, 
              y=log10(size_GB), 
              fill=datatype,
              shape=fileuse,
              label=data_purpose,
              label2=size_GB
  )) + 
  geom_point(aes( size=log10(size_GB) ),
             position = position_jitter(width = 10, height = 0.5)) +
  facet_grid(fileuse ~.) +
  theme_classic()


gg_merge <- ggplotly(p_merge, tooltip = c("label2", "label", "fill") ) 
cols <- (RColorBrewer::brewer.pal(8, "Dark2"))

s <- attrs_selected(
  showlegend = TRUE,
  mode = "lines+markers",
  marker = list(symbol = "x"),
  color = cols, 
  dynamic = TRUE
)

highlight(layout(gg_merge, showlegend = TRUE), selected = s)



# network eg ----
# Libraries
library(igraph)
library(networkD3)

# create a dataset:
data <- data.frame(
  from=c("A", "A", "B", "D", "C", "D", "E", "B", "C", "D", "K", "A", "M"),
  to=c("B", "E", "F", "A", "C", "A", "B", "Z", "A", "C", "A", "B", "K"),
  value=c("1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1")
)

# Plot
p <- simpleNetwork(data, height="100px", width="100px",        
                   Source = 1,                 # column number of source
                   Target = 2,                 # column number of target
                   linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                   charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                   fontSize = 14,               # size of the node names
                   fontFamily = "serif",       # font og node names
                   linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
                   nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
                   opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                   zoom = T                    # Can you zoom on the figure?
)

p

# save the widget
library(htmlwidgets)
saveWidget(p, file=paste0( getwd(), "~/Desktop/HtmlWidget/networkInteractive2.html"))

saveWidget(p, file="~/Desktop/HtmlWidget/networkInteractive2.html")

# network ----
src <- df_bind$project
target <- df_bind$filename

networkData <- data.frame(src, target, stringsAsFactors = FALSE)

nodes <- data.frame(name = unique(c(src, target)), stringsAsFactors = FALSE)
nodes$id <- 0:(nrow(nodes) - 1)

# create a data frame of the edges that uses id 0:9 instead of their names
edges <- networkData %>%
  left_join(nodes, by = c("src" = "name")) %>%
  select(-src) %>%
  rename(source = id) %>%
  left_join(nodes, by = c("target" = "name")) %>%
  select(-target) %>%
  rename(target = id)

edges$width <- 1

# make a grouping variable that will match to colours
nodes$group <- ifelse(nodes$name %in% src, "dsc", "orion_cygnus")

# simple with default colours
forceNetwork(Links = edges, Nodes = nodes, 
             Source = "source",
             Target = "target",
             NodeID ="name",
             Group = "group",
             Value = "width",
             opacity = 0.9,
             zoom = TRUE)

# control colours with a JS ordinal scale
# edited 20 May 2017 with updated code from Renal Chesak's answer:
ColourScale <- 'd3.scaleOrdinal()
            .domain(["dsc", "orion_cygnus"])
           .range(["#FF6900", "#694489"]);'

forceNetwork(Links = edges, Nodes = nodes, 
             linkDistance = 10,
             Source = "source",
             Target = "target",
             NodeID ="name",
             Group = "group",
             Value = "width",
             opacity = 0.9,
             zoom = TRUE,
             opacityNoHover = 0,
             colourScale = JS(ColourScale),
             charge = -100,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
             fontSize = 14,               # size of the node names
             fontFamily = "serif",       # font og node names
             )




# other test
files <- df_bind %>% 
  select(project,filename)

files$value <- "1"

p <- simpleNetwork(files, #height="100px", width="100px",        
                   Source = 2,                 # column number of source
                   Target = 1,                 # column number of target
                   linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                   charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                   fontSize = 14,               # size of the node names
                   fontFamily = "serif",       # font og node names
                   linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
                   nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
                   opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                   zoom = T,                    # Can you zoom on the figure?
                   #colourScale = JS(ColourScale)
)

p

forceNetwork(Links = edges, Nodes = nodes, 
             Source = "source",
             Target = "target",
             NodeID ="name",
             Group = "group",
             Value = "width",
             opacity = 0.9,
             zoom = TRUE,
             colourScale = JS(ColourScale))





######################################

edges <- read.table(header = T, text = '
source target width
0      1     1
0      2     1
0      3     1
0      4     1
1      5     1
1      6     1
2      7     1
2      8     1
3      9     1
')

nodes <- read.table(header = T, text = '
name id  group
A    0   panther
B    1   leopard
C    2   tiger
D    3   lion
J    4   panthercub
E    5   leopardcub
F    6   leopardcub
G    7   tigercub
H    8   tigercub
I    9   lioncub
')

forceNetwork(Links = edges, Nodes = nodes, 
             Source = "source",
             Target = "target",
             NodeID ="name",
             Group = "group",
             Value = "width",
             opacity = 0.9,
             zoom = TRUE,
             fontSize = 20, 
             fontFamily = "sans-serif",
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"))









