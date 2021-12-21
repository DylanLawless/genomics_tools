
# library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)

# Most features are included here. Other possible features might be found via swissprot.
# https://www.ncbi.nlm.nih.gov/ipg/NP_056862.1
# SwissProt O36633.1 Human respiratory syncytial virus B1
# https://www.ncbi.nlm.nih.gov/projects/sviewer/?id=O36633.1

# import ----
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("rtracklayer")
library(rtracklayer)

# Data sources ----
# Strain data ----
# Uniprot under format tab: https://www.uniprot.org/uniprot/P03423#
# UniProtKB - P03423 (GLYC_HRSVA)
# Human respiratory syncytial virus A (strain A2)
# uniprot_RSV_GLYC_HRSVA_P03423.gff

# Uniprot under format tab: https://www.uniprot.org/uniprot/O36633
# UniProtKB - O36633 (GLYC_HRSVB)
# uniprot_RSV_GLYC_HRSVB_O36633.gff
# Human respiratory syncytial virus B (strain B1) 

# Data import ----
df_uniprot <- readGFF("../data/uniprot_RSV_GLYC_HRSVA_P03423.gff")
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

## LOCATION
#  Topological domain
#  Transmembrane
  
## PROCESSING and SEQUENCES
#  Chain
#  Glycosylation
#  Disulfide bond 
#  Site
#  Alternative sequence 
## Pathology & Biotech
#  Mutagenesis  

## FAMILY AND DOMAIN
#  Region
#  Compositional bias
#  Helix
#  Turn

Location <- c("Topological domain", "Transmembrane")
Domain <- c("Chain", "Disulfide bond", "Site")
Features <- c("Alternative sequence", "Glycosylation", "Mutagenesis")
Structure <- c( "Region", "Compositional bias", "Helix", "Turn")

dt_uniprot_Location <- filter(dt_uniprot, type %in% Location) 
dt_uniprot_Domain <- filter(dt_uniprot, type %in% Domain)
dt_uniprot_Features <- filter(dt_uniprot, type %in% Features) 
dt_uniprot_Structure <- filter(dt_uniprot, type %in% Structure) 
dt_uniprot_Location$label <- "Location"
dt_uniprot_Domain$label <- "Family & Domain"
dt_uniprot_Features$label <- "Features"
dt_uniprot_Structure$label <- "Structure"
dt_uniprot_Domain$type <- make.unique(as.character(dt_uniprot_Domain$type), sep = "_") # esp for domain, do not overlap
dt_uniprot_Structure1 <- dt_uniprot_Structure %>% filter(!type == "Region")
dt_uniprot_Structure2 <- dt_uniprot_Structure %>% filter(type == "Region")
dt_uniprot_Structure2$type <- make.unique(as.character(dt_uniprot_Structure2$type), sep = "_")
dt_uniprot_Structure <- rbind(dt_uniprot_Structure1, dt_uniprot_Structure2)
dt_uniprot <- rbind(dt_uniprot_Location, dt_uniprot_Domain, dt_uniprot_Features, dt_uniprot_Structure)

# remove label Notes - try to keep in hover
dt_uniprot$Note <- na_if(dt_uniprot$Note, "character(0)")
dt_uniprot$Note[grepl("-linked", dt_uniprot$Note)] <- "linked"
dt_uniprot$Note <- na_if(dt_uniprot$Note, "linked")
dt_uniprot$Note[grepl("loss of interaction", dt_uniprot$Note)] <- "loss of interaction"
dt_uniprot$Note <- na_if(dt_uniprot$Note, "loss of interaction")
library(stringr)
dt_uniprot$Note <- str_replace(dt_uniprot$Note, "In isoform Secreted glycoprotein G. Missing", "Missing in secreted isoform")

dt_uniprot_sub <- dt_uniprot #%>% filter(label=="Family & Domain")


# factor to character for alphabetic order
dt_uniprot$type <- as.character(dt_uniprot$type)

#"V126"  p.E123K/D and 
# "V221" p.P217T/S/L

# main figure ----
library(wesanderson)
p <- dt_uniprot %>%
  group_by(type, Note) %>%
  ggplot(aes(x=start, y=type, label=type)) +
  geom_segment(size = 4, aes(x = start,  xend = end, y = type, yend = type, color = type), show.legend=FALSE) +
  facet_grid(vars(label), scales = "free", space = "free") +
  geom_text(data = dt_uniprot_sub, aes(label = Note, x = position_label,  y = type, ), hjust=0, vjust=0)  +
  geom_vline(xintercept=c(123,217), linetype="dotted", color="blue")+
  ylab("") +
  xlab("Protein position") + 
  theme_bw() +
  scale_x_continuous(limits = c(0, max(dt_uniprot$end)), breaks = seq(0, max(dt_uniprot$end), 50))+
  #scale_color_manual(values = wes_palette("Zissou1", 19, type = "continuous"))
  scale_color_manual(values = wes_palette("FantasticFox1", 19, type = "continuous")) 

p 
ggp <- ggplotly(p)
hide_legend(ggp)

library(htmlwidgets)
saveWidget(hide_legend(ggp), file = "../output/gene_illustrate_rsv.html") 

# possible to duplicate every row for region and use:  geom_raster(aes(x=start, fill = type), hjust=0.5, vjust=0.5, interpolate=FALSE)




# duplicate rows for tiles
df_uniprot <- dt_uniprot
df_uniprot$freq <- df_uniprot$end - df_uniprot$start +1
tmp  <- df_uniprot # %>% head()

# note this next selection is critical so that the enough column sare used for correct "rep" command,
tmp <- tmp #%>% select(freq, seqid, type, position_label, start, end)
names(tmp)
tmp <- tmp %>% select(freq, seqid, Note, ID, type, position_label, start, end)
tmp
library(purrr)
tmp <- tmp %>% map_df(rep, tmp$freq)
tmp
tmp$res <- ave(tmp$freq, tmp$seqid, tmp$type, tmp$start, tmp$end, FUN = seq_along)
tmp$AA <- (tmp$start) + (tmp$res - 1)

tmp2 <- merge(tmp, df_uniprot)

library(data.table)
tmp2 <- as.data.table(tmp2)
tmp2 <- tmp2[,which(unlist(lapply(tmp2, function(x)!all(is.na(x))))),with=F]


# add notes as shapes
tmp2$Note <- as.character(tmp2$Note)

# detail figure ----
px <- tmp2 %>% 
  ungroup() %>%
  ggplot(aes(x=AA, y=type, 
             label=label, label2=start, label3=end, 
             )) +
  geom_tile(aes(fill=type)) + # geom_time = 6.3MB
  #geom_raster(aes()) # geom_raster = 6.3MB
  facet_grid(label~. , 
            scales = "free", space = "free"
             ) +
  geom_text(data = dt_uniprot_sub, aes(label = Note, x = position_label,  y = type, ), hjust=0, vjust=0) +
  geom_vline(xintercept=c(123,217), linetype="dotted", color="blue")+
  ylab("") +
  xlab("Protein position") + 
  theme_bw() +
  scale_x_continuous(limits = c(0, max(dt_uniprot$end)), breaks = seq(0, max(dt_uniprot$end), 50))+
  scale_fill_manual(values = wes_palette("FantasticFox1", 19, type = "continuous")) 
px

gpx <- hide_legend(ggplotly(px))
gpx
saveWidget(gpx, file = "../output/gene_illustrate_rsv_detail.html") 




