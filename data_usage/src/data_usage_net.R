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



# network ----
# Libraries
library(igraph)
library(networkD3)




# save the widget
#library(htmlwidgets)
#saveWidget(p, file=paste0( getwd(), "~/Desktop/HtmlWidget/networkInteractive2.html"))
#saveWidget(p, file="~/Desktop/HtmlWidget/networkInteractive2.html")

project <- df_bind$project
filename <- df_bind$filename
datatype <- df_bind$datatype

# src = project
# target = filename

networkData <- data.frame(project, filename, stringsAsFactors = FALSE)

nodes <- data.frame(name = unique(c(project, filename)), stringsAsFactors = FALSE)
nodes$id <- 0:(nrow(nodes) - 1)

# make a grouping variable that will match to colours
#nodes$group <- ifelse(nodes$name %in% project, "dsc", "orion_cygnus")

# create a data frame of the edges that uses id 0:9 instead of their names
rm(edges)
edges <- networkData %>%
  left_join(nodes, by = c("project" = "name")) %>%
  select(-project) %>%
  rename(source = id) %>%
  left_join(nodes, by = c("filename" = "name")) %>%
  select(-filename) %>%
  rename(filename = id) 

edges$width <- 10

nodes <- nodes %>%
  left_join(
    df_bind %>% select(filename, datatype)
    , by = c("name" = "filename"),
    keep=FALSE) %>%
  unique()


# control colours with a JS ordinal scale
# edited 20 May 2017 with updated code from Renal Chesak's answer:
ColourScale <- 'd3.scaleOrdinal()
            .domain(["dsc", "orion_cygnus"])
           .range(["#FF6900", "#694489"]);'

ColourScale2 <- 'd3.scaleOrdinal()
            .domain(["genotype", "genome_sequence", "clinical", "serology", "somatic_genome", "somatic_panel", "immunology", "rna_expression", "orion_cygnus", "dsc" ])
           .range([ "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#2986cc", "#F2A21E"]);'



forceNetwork(Links = edges, Nodes = nodes, 
             linkDistance = 10,
             Source = "source",
             Target = "filename",
             NodeID ="name",
             Group = "datatype",
             Value = "width",
             opacity = 0.9,
             zoom = TRUE,
             opacityNoHover = 0,
             colourScale = JS(ColourScale2),
             charge = -100,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
             fontSize = 14,               # size of the node names
             fontFamily = "serif",       # font og node names
             )

# network datatypes ----
tmp1 <- df_bind %>% select(filename, datatype, project)
tmp2 <- tmp1 %>%
  group_by(datatype, project) %>%
  mutate(filename = as.character(filename)) %>%
  expand(filename, to = filename) %>% 
  filter(filename < to) %>% 
  select(from = filename, to, type = datatype)

tmp2$width <- 1
tmp2 <- as.data.frame(tmp2)

#nodes: name, id
#tmp2: from, to, type

edges2 <- tmp2 %>%
  left_join(nodes, by = c("from" = "name")) %>%
  select(-from) %>%
  rename(source = id) %>%
  left_join(nodes, by = c("to" = "name")) %>%
  select(-to) %>%
  rename(filename = id) %>%
  select(source, filename, width)

edges3 <- rbind(edges, edges2)

# to get node sizes, count how file usage
#library(tidyr)
count <- df_bind %>% 
  group_by(filename) %>% 
  summarize(number_filename = n())

colnames(count)[colnames(count) == 'filename'] <- 'name'

nodes <- nodes %>%
  left_join(
    count, by = c("name" = "name"),
    keep=FALSE) %>%
  unique()

# replace missing data
nodes <- nodes %>% replace_na(list(datatype = "project", number_filename = 1))
nodes$number_filename_10 <- nodes$number_filename*10

# to get link color, count how many projects a file is used in
count_project <- df_bind %>% 
  group_by(filename, project) %>% 
  summarize(count = n())%>% 
  group_by(filename) %>%
  summarize(project_count = n())

colnames(count_project)[colnames(count_project) == 'filename'] <- 'name'

nodes <- nodes %>%
  left_join(
    count_project, by = c("name" = "name"),
    keep=FALSE) %>%
  unique()

#colors
#"#edc951"
#"#eb6841"

link_cols <- c(
"#00a0b0",
"#cc2a36",
"#4f372d")

project_count <- as.numeric( c(1,2,'NA'))

node_link_cols <- data.frame(project_count, link_cols)
nodes <- merge(nodes, node_link_cols, by = "project_count" )

nodes <- nodes %>%  arrange(id)
# match filename to edge for color

link_cols_id <- nodes %>% select(link_cols, id)
link_project_count <- nodes %>% select(project_count, id)


edges4 <- edges3 %>% left_join(link_cols_id, by = c("filename" = "id"))
edges5 <- edges3 %>% left_join(link_project_count, by = c("filename" = "id"))



YourColors <- 'd3.scaleOrdinal()
                  .domain([0, 1, 2])
                  .range(["#7FFF00", "#A52A2A", "#E6E6FA"])'

# replace missing data
edges5 <- edges5 %>% replace_na(list(project_count = "0"))


forceNetwork(Links = edges5, Nodes = nodes, 
             linkDistance = 60,
             Nodesize = "number_filename",
             #linkColour = YourColors,
             linkColour = ifelse(edges5$project_count > 1, "#4f372d", "#00a0b0"),
        #    linkColour = ifelse(edges5$project_count > 1, "blue","black"),
             radiusCalculation = JS(" Math.sqrt(d.nodesize)*6"),
             Source = "source",
             Target = "filename",
             NodeID ="name",
             Group = "datatype",
             Value = "width",
             opacity = 0.9,
             zoom = TRUE,
             opacityNoHover = 0,
             colourScale = JS(ColourScale2),
             charge = -40,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
             fontSize = 14,               # size of the node names
             fontFamily = "serif",       # font og node names
)


