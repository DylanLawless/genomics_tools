
# library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)


# import ----
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("rtracklayer")
library(rtracklayer)

# Identify canonical transcript ----
# GnomAD: Ensembl canonical transcript More information ENST00000379883.2
# Uniprot: ENST00000379868﻿; ENSP00000369197﻿; ENSG00000107201 [O95786-2]
# Uniprot:ENST00000379883﻿; ENSP00000369213﻿; ENSG00000107201 [O95786-1]

# Data sources ----
# Ensembl: https://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=ENSG00000107201;r=9:32455302-32526208
# Uniprot under format tab: https://www.uniprot.org/uniprot/O95786# 

# Data import ----
df <-readGFF("../data/ddx58_GRCh38_p13.gff")
df_uniprot <- readGFF("../data/uniprot_DDX58_HUMAN_O95786.gff")
df_uniprot <- data.frame(df_uniprot)

# Uniprot plot ----
# data tidy ----
# if column is all NA, drop. This DT method is more complex than filter, but will be fast on large data.
library(data.table)
dt <- as.data.table(df_uniprot)
dt_uniprot <- dt[,which(unlist(lapply(dt, function(x)!all(is.na(x))))),with=F]
rm(dt)

# add notes as shapes
dt_uniprot$Note <- as.character(df_uniprot$Note)

# get the mean start end to position center
dt_uniprot$position_label <- rowMeans(dt_uniprot[,c('start', 'end')], na.rm=TRUE)

# some labels have start=end (1 amino) and not shown as geom_point
# if start end = 0, add 1 to end
dt_uniprot$diff <- dt_uniprot$end - dt_uniprot$start
dt_uniprot$end_alt <- dt_uniprot$end
dt_uniprot$end_alt[dt_uniprot$diff==0] <-  1
dt_uniprot$end_alt[dt_uniprot$diff>0] <-  0
dt_uniprot$end <- dt_uniprot$end + dt_uniprot$end_alt

names(dt_uniprot)
dt_uniprot$type %>% unique()
# "Chain", "Domain", "Nucleotide binding", "Region", "Motif", "Metal binding",     "Modified residue", "Cross-link", "Alternative sequence", "Natural variant", "Mutagenesis", "Helix", "Turn", "Beta strand"


Domain <- c("Chain", "Domain", "Region", "Motif")
Features <- c("Nucleotide binding", "Metal binding", "Modified residue", "Cross-link", "Alternative sequence", "Natural variant", "Mutagenesis")
Structure <- c("Helix", "Turn", "Beta strand")
 
dt_uniprot_Domain <- filter(dt_uniprot, type %in% Domain)
dt_uniprot_Features <- filter(dt_uniprot, type %in% Features) 
dt_uniprot_Structure <- filter(dt_uniprot, type %in% Structure) 
dt_uniprot_Domain$label <- "Family & Domain"
dt_uniprot_Features$label <- "Features"
dt_uniprot_Structure$label <- "Structure"
dt_uniprot_Domain$type <- make.unique(as.character(dt_uniprot_Domain$type), sep = "_") # esp for domain, do not overlap
dt_uniprot <- rbind(dt_uniprot_Domain, dt_uniprot_Features, dt_uniprot_Structure)
dt_uniprot_sub <- dt_uniprot %>% filter(label=="Family & Domain")

library(wesanderson)
p <- dt_uniprot %>%
  group_by(type, Note) %>%
  ggplot(aes(x=start, y=type, label=type)) +
  geom_segment(size = 4, aes(x = start,  xend = end, y = type, yend = type, color = type), show.legend=FALSE) +
  facet_grid(vars(label), scales = "free", space = "free") +
  geom_text(data = dt_uniprot_sub, aes(label = Note, x = position_label,  y = type, ), hjust=0, vjust=0)  +
  geom_vline(xintercept=c(126,221), linetype="dotted", color="blue")+
  ylab("") +
  xlab("Protein position") + 
  theme_bw() +
  scale_x_continuous(limits = c(0, max(dt_uniprot$end)), breaks = seq(0, max(dt_uniprot$end), 50))+
  #scale_color_manual(values = wes_palette("Zissou1", 19, type = "continuous"))
  scale_color_manual(values = wes_palette("FantasticFox1", 19, type = "continuous"))

p 
ggp <- ggplotly(p)
hide_legend(ggp)


# possible to duplicate every row for region and use:  geom_raster(aes(x=start, fill = type), hjust=0.5, vjust=0.5, interpolate=FALSE)

# Ensembl plot ----
# Data tidy ----
df$length <-  df$end - df$start
#df2 <- df %>%
 # filter(transcript_id == "ENST00000379883")
  #filter(gene_type == "protein_coding")# %>%
  #filter(start <= 32475000) %>%
 # filter(length <= 100)

unique( df$gene_id )

#df %>%group_by("gene_id","transcript_id","exon_id")

#to get the correct y axis, 
#collapse "types", and transcript

library(tidyr)
df <- df %>% drop_na(transcript_id) 

# remove gaps to emulate CDNA
#df$start <- factor(df$start)
#df$end <- factor(df$end)

# mark the canonical transcript
df$canon[df$transcript_id=="ENST00000379883.3"] <-  1
canon <- df %>% filter(canon == 1) %>% filter(start == min(start))

enst <- df %>%
  ggplot(aes(x=start, y=transcript_id)) +
  geom_segment(size = 4,
               aes(x = start,  xend = end, 
                   y = transcript_id, yend = transcript_id, 
                   colour = gene_id)) +
  geom_point(data = canon, shape=23, fill="green", size =3) +
#  facet_grid(type ~., scales = "free_y" ) +
  geom_vline(xintercept=c(32493805, 32491328), linetype="dotted", color="blue")+
  theme(axis.text.x = element_text(angle = 90)) +
          ylab("") +  
          xlab("Protein position") +    theme_bw()

enst
ggest <- hide_legend(ggplotly(enst))



# fig main ---- 
subplot(ggp, ggest, nrows = 2, margin = 0.02, heights = c(0.5, 0.5), shareX = F, titleY=TRUE) 
# %>%
#layout(xaxis=list(autorange=F, range=c(23,39)), 
#       yaxis=list(autorange=F, range=c(54,56.5)))


#coord <- c(min(df$start), max(df$end), min(df_uniprot$start), max(df_uniprot$end))
coord <- c(0, 700, 0, 900)
# variant <- c(32466290, 32472975, 126, 221)
variant <- c(334, 360, 126, 221)
label <- c("gDNA", "gDNA", "cDNA", "cDNA")
position <- c("start", "end", "start", "end")
df_coord <- data.frame(label, coord, variant, position) 
  
df_coord
class(df_coord$variant)
class(df_coord$coord)

p3 <- df_coord %>%
  ggplot(aes(x=coord, y=label, group = position)) +
  geom_line(data = df_coord, aes(x=coord))  +
  geom_line(data = df_coord, aes(x=variant), linetype="dotted", color="blue") +
  #geom_path( aes(x=coord, y=label), arrow = arrow(length = unit(0.2, "cm"))) +
  #geom_path( aes(x=variant, y=label), arrow = arrow(length = unit(0.2, "cm"))) +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(limits = c(0, 900), breaks = seq(0, 900, 100))  +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("")
p3

# main figure ----
subplot(ggp, p3, ggest, nrows = 3, margin = 0.0, heights = c(0.4, 0.2, 0.4), shareX = F, titleY=F) 

# query gnomad for variants ----
SNP1 <- c(
"9-32493805-T-C",
"E / G",
"p.Asp126Gly",
"missense",
6,
281044,
2.13e-5,
0)

SNP2 <- c(
"9-32491328-T-C",
"E",
"p.Asn221Ser",
"missense",
4,
250734,
1.60e-5,
0)

head <- c("Variant ID", "Source", "HGVS Consq", "VEP", "AC", "AN", "Freq", "Hom")
gnomad <- data.frame( SNP1, SNP2)
rownames(gnomad) <- head
gnomad <- t(gnomad) %>% as.data.frame()
gnomad

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

gnomad_t <- 
  reactable(gnomad,
            compact = TRUE, 
            searchable = TRUE,
            #elementId = "download-table",
            defaultPageSize = 10,
            defaultColDef = colDef(minWidth = 93 ),
            columns = list(
            "Variant ID" = colDef(minWidth = 200),
            "HGVS Consq" = colDef(minWidth = 150)
            ))
            
gnomad_t


subplot(gnomad_t, ggp, p3, ggest, nrows = 4, margin = 0.0, heights = c(0.2, 0.2, 0.2, 0.4), shareX = F, titleY=F) 

df %>% drop_na(transcript_id) %>%
  ggplot(aes(x=start, y=type)) +
  geom_segment(size = 4,
               aes(x = start,  xend = end, 
                   y = type, yend = type, 
                   colour = gene_id)) +
  # facet_grid(gene_id ~., scales = "free_y" )
  facet_grid(transcript_id ~., scales = "free_y" )


ggplotly(p)

ggplot(df, 
       aes(x = start, xend = end, 
           y = seqid, yend = seqid, 
          # label = paste(lineend, linejoin)
           )) +
  geom_segment(size = 1, 
               #arrow = arrow(length = unit(0.3, "inches"))
               ) 
  #geom_text(hjust = 'outside', 
  #          nudge_x = -0.2) +
  #xlim(0.5, 2)


# Use lineend and linejoin to change the style of the segments
df2 <- expand.grid(
  lineend = c('round', 'butt', 'square'),
  linejoin = c('round', 'mitre', 'bevel'),
  stringsAsFactors = FALSE
)
df2 <- data.frame(df2, y = 1:9)

ggplot(df2, aes(x = 1, y = y, xend = 2, yend = y, label = paste(lineend, linejoin))) +
  geom_segment(
    lineend = df2$lineend, linejoin = df2$linejoin,
    size = 3, arrow = arrow(length = unit(0.3, "inches"))
  ) +
  geom_text(hjust = 'outside', nudge_x = -0.2) +
  xlim(0.5, 2)
          

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
