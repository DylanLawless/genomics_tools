
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

df_uniprot <- readGFF("../data/uniprot_HUMAN_Q9C0K0_BCL11B.gff")
df_uniprot <- data.frame(df_uniprot)
df_uniprot$Note<- as.character(df_uniprot$Note)

# The only domain on uniprot is SH2, so remove this and add all domains manyally according to paper
# df_uniprot <- df_uniprot |> filter(!Note=="SH2")

# Not on uniprot ----
# Note <- c("N-terminal", "coiled-coil", "DBD", "linker", "SH2", "Transactivation")
# start <- c(1,   138, 320, 562, 584, 675)
# end <-   c(122, 319, 561, 583, 674, 770)
# type <- c("Domain", "Domain", "Domain", "Domain", "Domain", "Domain")
# domains <- data.frame(type, Note, start, end)

# # Merge keeping only matches
# df_uniprot <- bind_rows(df_uniprot, 
# 					 domains
# 					 )

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
library(stringr)

dt_uniprot$Note <- str_replace_all(dt_uniprot$Note, "character\\(0\\)", "")
dt_uniprot$Dbxref <- str_replace_all(dt_uniprot$Dbxref, "character\\(0\\)", "")

dt_uniprot$Note = str_wrap(dt_uniprot$Note, width = 30)
dt_uniprot$Dbxref = str_wrap(dt_uniprot$Dbxref, width = 30)
dt_uniprot$evidence = str_wrap(dt_uniprot$evidence, width = 30)

wes_pal = c(
  "#00A08A", "#F2AD00", "#F98400", "#5BBCD6", # Darjeeling1 (removed "#FF0000" hard red)
  "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", # Darjeeling2 (removed #"#000000" black)
  "#DD8D29", "#E2D200", "#46ACC8", "#E58601", "#B40F20", #  FantasticFox1
  "#F1BB7B", "#FD6467", "#5B1A18", "#D67236", # GrandBudapest1
  "#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4" # GrandBudapest2
  # "#446455", "#FDD262", "#D3DDDC", "#C7B19C", #Chevalier1
)

# First plot ----
p <- dt_uniprot %>%
  group_by(type, Note) %>%
  ggplot(aes(y=type, x=start, label=end, label2=Note, label3=Dbxref, label4=evidence)) +
  geom_segment(size = 4, aes(x = start,  xend = end, y = type, yend = type, color = type), show.legend=FALSE) +
  facet_grid(vars(label), scales = "free", space = "free") +
  geom_text(data = dt_uniprot_sub, aes(label = Note, x = position_label,  y = type, ), hjust=0, vjust=0, text = paste0())  +
  ylab("") +
  xlab("Protein position") + 
  theme_bw() +
  scale_x_continuous(limits = c(0, max(dt_uniprot$end)), breaks = seq(0, max(dt_uniprot$end), 50))+
  scale_color_manual(values = wes_pal)

p

ggp <- ggplotly(p, tooltip = c("y", "x", "label", "label2", "label3", "label4" ))
hide_legend(ggp)



# Import variants ----
variants <- read.csv("../data/bcl11b_variants.csv", stringsAsFactors = FALSE, header = TRUE, sep=",")

variants <- variants %>% filter(symbol == "BCL11B") %>%
  select(symbol, #consequence, 
  		 protein_position, 
  		 #Pos
  		 )

variants$protein_position <- as.numeric(variants$protein_position)

p_protein <- p +  geom_vline(xintercept=c(variants$protein_position), linetype="dotted", color="blue")
p_protein

ggsave(plot=p_protein, file="./bcl11b_cohort_n548_summary_plot.pdf")
