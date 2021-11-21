library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)

# Data usage tracking
# caption: Long-term projects use data from many sources. Data may be reused multiple times, and some planned analysis is not required. Data tracking systems can show these analytics and improve multipurpose use.

# df colors ----
# We require 11 color groups for these plots.
# The will be used in ggplot or d3; set up for each.

# for d3
ColourScale <- 'd3.scaleOrdinal()
.domain(["genotype", "genome_sequence", "clinical", "serology", "somatic_genome", "somatic_panel", "immunology", "rna_expression", "orion_cygnus", "dsc"])
.range([ "#f2825a", "#c2625a", "#92425a", "#62225a", "#32025a", "#de9d54", "#cf512b", "#36549d","#252381", "#0a0945"]);'

# for ggplot
ColourScale_label <- c("genotype", "genome_sequence", "clinical", "serology", "somatic_genome", "somatic_panel", "immunology", "rna_expression", "orion_cygnus", "dsc")

ColourScale_hex <- c("#f2825a", "#c2625a", "#92425a", "#62225a", "#32025a", "#de9d54", "#cf512b", "#36549d","#252381", "#0a0945")

ColourScale_groups <- data.frame(ColourScale_label,ColourScale_hex)
ColourScale_groups <- t(ColourScale_groups) %>% janitor::row_to_names(1)
rownames(ColourScale_groups)<-NULL

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

# df 1 and df2 ----
df$Date <- ymd(df$date)
df$fileuse <- as.factor(df$fileuse)
df$fileuse <- factor(df$fileuse, levels = c("Yes","No"))
d <- highlight_key(df, ~datatype)

df_2$Date <- ymd(df_2$date)
df_2$fileuse <- as.factor(df_2$fileuse)
df_2$fileuse <- factor(df_2$fileuse, levels = c("Yes","No"))
d_2 <- highlight_key(df_2, ~datatype)

# df merged + group colors ----
df_bind <- rbind(df, df_2)
dd <- highlight_key(df_bind, ~datatype)
rm(df, df_2, d, d_2)

facet.labs <- c("Usage: Yes ", "Usage: No")
names(facet.labs) <- c("Yes", "No")

p_merge <- dd %>% ggplot( aes(x=Date, y=log10(size_GB), 
                              fill=datatype,
                              shape=fileuse,
                              label=data_purpose,
                              label2=size_GB,
                              label3=project
)) + 
  geom_point(aes( size=log10(size_GB) ),
             alpha=.8,
             position = position_jitter(width = 10, height = 0.5)) +
  facet_grid(fileuse ~., labeller = labeller(fileuse = facet.labs)) +
  theme_classic() +
  scale_fill_manual(values=ColourScale_groups,
                    breaks = c("genotype", "genome_sequence", "clinical", "serology", "somatic_genome", "somatic_panel", "immunology", "rna_expression"))

#to set custom tooltips: tooltip = c("label", "fill")
plot_gg_merge <- ggplotly(p_merge) %>% 
  layout(#xaxis = list(showticklabels = FALSE),
    legend = list(orientation = "v",
                  y = 1, x = -200))

#rm(p_merge)

s <- attrs_selected(
  showlegend = TRUE,
  mode = "lines+markers",
  marker = list(symbol = "x"),
  #colors = colRGB, 
  dynamic = TRUE
)

# Fig: 1 main ----
highlight(plot_gg_merge, selected = s)

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

# make nodes
nodes <- data.frame(name = unique(c(project, filename)), stringsAsFactors = FALSE)
nodes$id <- 0:(nrow(nodes) - 1)

# make edges
networkData <- data.frame(project, filename, stringsAsFactors = FALSE)
# create a data frame of the edges that uses id 0:9 instead of their names
rm(edges)
edges <- networkData %>%
  left_join(nodes, by = c("project" = "name")) %>%
  select(-project) %>%
  rename(source = id) %>%
  left_join(nodes, by = c("filename" = "name")) %>%
  select(-filename) %>%
  rename(filename = id) 

rm(networkData)
edges$width <- 10

nodes <- nodes %>%
  left_join(
    df_bind %>% select(filename, datatype)
    , by = c("name" = "filename"),
    keep=FALSE) %>%
  unique()

# replace missing datatype with name
nodes$datatype[is.na(nodes$datatype)] <- nodes$name[is.na(nodes$datatype)]

# simple plot of project overlap
# control colours with a JS ordinal scale

plot_fn_s <- forceNetwork(Links = edges, Nodes = nodes, 
             linkDistance = 10,
             Source = "source",
             Target = "filename",
             NodeID ="name",
             Group = "datatype",
             Value = "width",
             opacity = 0.9,
             zoom = TRUE,
             opacityNoHover = 0,
             colourScale = JS(ColourScale),
             charge = -100,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
             fontSize = 14,               # size of the node names
             fontFamily = "sans-serif",       # font of node names
             legend = TRUE)

# Fig: 2 simple ----  
plot_fn_s

# network group datatypes ----
library(tidyr)
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

rm(tmp1, tmp2)
rm(edges, edges2)

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

rm(count)

# replace missing datatype with name
nodes$datatype[is.na(nodes$datatype)] <- nodes$name[is.na(nodes$datatype)]

# replace missing filenumber for the projects
nodes <- nodes %>% replace_na(list(number_filename = 1))

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

# replace missing data
edges5 <- edges5 %>% replace_na(list(project_count = "0"))

plot_fn <- forceNetwork(Links = edges5, Nodes = nodes, 
             linkDistance = 60,
             Nodesize = "number_filename",
             linkColour = ifelse(edges5$project_count > 1, "#4f372d", "#00a0b0"),
        #    linkColour = ifelse(edges5$project_count > 1, "blue","black"),
             radiusCalculation = JS(" Math.sqrt(d.nodesize)*6"),
        linkWidth = JS("function(d) { return Math.sqrt(d.value)/2; }"),
             Source = "source",
             Target = "filename",
             NodeID ="name",
             Group = "datatype",
             Value = "width",
             opacity = 0.9,
             zoom = TRUE,
             opacityNoHover = 0,
             colourScale = JS(ColourScale),
             charge = -40,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
             fontSize = 14,               # size of the node names
             fontFamily = "sans-serif",       # font og node names
              bounded = TRUE,
            arrows = FALSE,
            legend = TRUE,
)

# Fig: 2 main ----
plot_fn 

# other not in use ----
# clickJS 
# forceNetwork(..., clickAction = clickJS)


# Fig: all plots ----
highlight(plot_gg_merge, selected = s)
plot_fn
plot_fn_s
