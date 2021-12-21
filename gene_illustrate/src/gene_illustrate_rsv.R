
# library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)

# https://www.ncbi.nlm.nih.gov/gene/1489824
# G attachment glycoprotein [ Human orthopneumovirus ]

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
  geom_vline(xintercept=c(123,217), linetype="dotted", color="red")+
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
# saveWidget(hide_legend(ggp), file = "../output/gene_illustrate_rsv.html") 

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
             label=label, label2=start, label3=end, label4=evidence, label5=Dbxref
             )) +
  geom_tile(aes(fill=type)) + # geom_time = 6.3MB
  #geom_raster(aes()) # geom_raster = 6.3MB
  facet_grid(label~. , 
            scales = "free", space = "free"
             ) +
  geom_text(data = dt_uniprot_sub, aes(label = Note, x = position_label,  y = type, ), hjust=0, vjust=0) +
  geom_vline(xintercept=c(123,217), linetype="dotted", color="red")+
  ylab("") +
  xlab("Protein position") + 
  theme_bw() +
  scale_x_continuous(limits = c(0, max(dt_uniprot$end)), breaks = seq(0, max(dt_uniprot$end), 50))+
  scale_fill_manual(values = wes_palette("FantasticFox1", 19, type = "continuous")) 
px

gpx <- hide_legend(ggplotly(px))
# gpx
# saveWidget(gpx, file = "../output/gene_illustrate_rsv_detail.html") 



# import P value and r sequred LD from analysis ----

# write.table(fit_long_cor, file='~/web/tools/genomics_tools/gene_illustrate/data/rsv_fit_long_cor.csv',sep=",", quote=FALSE, row.names=FALSE, col.names = FALSE)




fit_long_cor <- 
  read.table(file="../data/rsv_fit_long_cor.csv",
             header = TRUE, 
             sep = ",", 
             stringsAsFactors = FALSE,
            # colClasses = c("character"),
             )

fit_long_cor$p <- as.numeric(fit_long_cor$p)
class(fit_long_cor$position)

# main figure ----

p3_legend <- fit_long_cor %>%
  ggplot(aes( x= as.numeric( position ), y=( -log10(p) ) ))+
  geom_point( aes(color = groups ) ) +
  scale_color_manual(breaks = levels(fit_long_cor$groups),
                     values = c("blue", "green", "orange", "red"),
                     name="r² with proxy SNP") +
  labs(x = "Position", y = "-log10 (Pvalue) ") + 
  theme_bw()
p3_legend
#ggplotly(p3_legend)

p3 <- p3_legend + theme(panel.background = element_rect("#F7F7F7"),
                        legend.position="none")

p3
#ggplotly(p3)
# r2_with_proxy_SNP.pdf 3x7.5
# r2_with_proxy_SNP_no_legend.pdf 3x6

subplots <- subplot(ggplotly(p3), gpx, nrows = 2, margin = 0.02, heights = c(0.3, 0.7), shareX = TRUE, titleY=TRUE) 

# save ouput ----
# saveWidget(subplots, file = "../output/gene_illustrate_rsv_Pval.html") 


# merge with dataset

colnames(tmp2)[colnames(tmp2) == 'AA'] <- 'position'
tmp3 <- merge(fit_long_cor, tmp2, all=T, by='position')

# crosstalk ----

library(ggplot2)
library(plotly)
library(DT)

library(tidyverse)
library(plotly)

library(reactable)
library(crosstalk)
library(htmltools)


# data_t <- reactable( data, filterable = TRUE)


data <- tmp3 %>% filter(cor >0) %>%
  select(p, position, cor) %>% unique()
data$Pcor <- round( (-log10(data$p))  , digits = 2)
data$cor <- round( data$cor , digits = 3)
# working version ----

shared_mtcars <- SharedData$new(data)

crosstalk::bscols(widths = c(10),
                  list(
                    filter_checkbox("auto", "Sig Pval:", shared_mtcars, ~ifelse(p <= 0.05, "Yes", "No")),

                    crosstalk::filter_slider("position", "Position", shared_mtcars, ~position, width = "80%"),
                    crosstalk::filter_slider("Pvalue", "-log10 P-cor", shared_mtcars, ~Pcor, width = "80%"),
                    # test SNP check box
                    ggplotly(  ggplot( shared_mtcars, aes(position, -log10(p)) ) + geom_point(aes(color = cor)) ) %>% highlight("plotly_selected", opacityDim = getOption("opacityDim", 0.1)) ),
                  reactable(shared_mtcars, minRows = 10)
                  
)



# dt version - formatting doesn't work well. 
crosstalk::bscols(widths = c(10),
                  list(
                    DT::datatable(shared_mtcars, filter = 'top'),
                    ggplotly(  ggplot( shared_mtcars, aes(position, -log10(p)) ) + geom_point(aes(color = cor)) ) %>% highlight("plotly_selected", opacityDim = getOption("opacityDim", 0.1), width = 300) )
)





# data <- SharedData$new(data)
data <- crosstalk::SharedData$new(data)$data()

m<-highlight_key( data )
p <-ggplot( m, aes(position, -log10(p)) ) + geom_point(aes(color = cor))
gg<-highlight(ggplotly(p),"plotly_selected",
              opacityDim = getOption("opacityDim", 0.1),)


crosstalk::bscols(widths = c(3,3),
  #list(
    #filter_checkbox("type", "Type", data, ~p),
   # filter_slider("position", "Pos", data, ~position, width = "100%"),
  #  filter_select(" ", "Pval", data, ~p)
  #),
  gg, DT::datatable(m)
  )



sd <- SharedData$new(data)




p_filter <- filter_slider("pval", "pval", sd, "p",  step = NULL, width = '100%', dragRange = TRUE)

pos_filter <- filter_slider("pos", "pos", sd, "position",  step = NULL, width = '100%', dragRange = TRUE)




tags$div(class="well well-lg",
         tagList(
           tags$h2('Alcohol Related Motor Vehicle Accidents 2012 - 2017'),
           p_filter,
           gg,
           bscols(pos_filter,
                  list(pos_filter,
                       pos_filter),
                  type_filter)
         )
)    




gg<-highlight(ggplotly(p),"plotly_selected",
              opacityDim = getOption("opacityDim", 0.01),)

crosstalk::bscols(gg,DT::datatable(m))






# Wrap data frame in SharedData
sd <- SharedData$new(data)

# Create a filter input
filter_slider("pos", "pos", sd, "position",  step = NULL, width = '100%', dragRange = TRUE)

# Use SharedData like a dataframe with Crosstalk-enabled widgets
crosstalk::bscols(gg,DT::datatable(sd))


if (interactive()) {
  
  sd <- SharedData$new(data)
  filter_slider("pos", "pos", sd, "position",  step = NULL, width = '100%', dragRange = TRUE)
  crosstalk::bscols(gg,DT::datatable(sd))
}





dt <- bscols(
  widths = c(3,3,3),
  list(
    #filter_checkbox("type", "Type", data, ~p),
    filter_slider("position", "Pos", data, ~position, width = "100%"),
    filter_select(" ", "Pval", data, ~p)
  ),
  reactable(data, minRows = 10),
  gg
)
dt


crosstalk::bscols(dt)




# table with plots
library(plotly)
library(reactable)
library(ggplot2)
library(dplyr)


df = iris %>%
  as_tibble() %>%
  nest_by(Species) %>%
  mutate(plot = list(
    ggplot(data, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point() + ggtitle(Species)
  ))

reactable(df %>%select(Species), 
          details = function(index) {
              ggplotly(df$plot[[index]])
            })







# example ----
tmp2 - data
px  - ggplot
gpx  - ggplotly
react_table

##crosstalk ----
library(tidyverse)
library(plotly)
library(reactable)
library(crosstalk)
library(htmltools)


library(crosstalk)
tmp2 <- SharedData$new(tmp2)

div(
  h3("Compare and filter QBs by EPA or Success rate per play"),
  h4("Filter sliders for specific ranges affect plot & table"),
  bscols(
    widths = c(2, 10),
    list(
#      filter_slider("epa_per_play", "EPA/Play", tmp2, ~AA),
 #     filter_slider("success_per_play", "Success/Play", tmp2, ~start)
    ),
    ggplotly(p3)
  ),
  tmp2_t
)



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

tmp2_t <- 
  reactable(tmp2,
            compact = TRUE, 
            searchable = TRUE,
            #elementId = "download-table",
            defaultPageSize = 10,
            defaultColDef = colDef(minWidth = 93 ),
            columns = list(
              "type" = colDef(minWidth = 200),
              "Note" = colDef(minWidth = 150)
            ))

tmp2_t



df_b <- bscols( widths = c(2, 9),
                ( filter_checkbox("ClinGen classification", "ClinGen classification", tmp2, ~'ClinGen classification')),
                tmp2_t,
                device = c( "sm"))








# crosstalk ----
#Note: bsCols() Seems to completely override the flexdashboard CSS and can't be used on website
# make the data crosstalk for a checklist filter
library(crosstalk)
data <- SharedData$new(tmp2)

df_b <- bscols( widths = c(2, 9),
                ( filter_checkbox("ClinGen classification", "ClinGen classification", data, ~'ClinGen classification')),
                df_t,
                device = c( "sm"))

checkbox <- filter_checkbox("ClinGen classification", "ClinGen classification", data, ~'ClinGen classification')
htmltools::browsable(df_t, checkbox)

htmltools::browsable(data)
checkbox <- filter_checkbox("AA", data, ~'type')






# features assessed:
tmp2$label%>% unique()
tmp$type %>% unique()

tmp2$Note%>% unique()

