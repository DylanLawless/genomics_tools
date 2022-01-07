# legends:


library(dplyr)
library(tidyr)
library(plotly)

df <- read.table(file = "../data/XGYK6RVT114-Alignment.tsv", sep = '\t', header = TRUE)
df_meta <- read.csv("../data/sequences.csv",stringsAsFactors = FALSE, header = TRUE) 
df <- df %>% unique()
df_meta <- df_meta %>% unique()

colnames(df)[colnames(df) == 'subject.acc.ver'] <- 'Accession'

df <-  separate(df, Accession,  sep = "\\.", into = c("Accession", "Accession2"))

df <- merge(df, df_meta, all.x=TRUE)

names(df)
colnames(df)[colnames(df) == 'X..identity'] <- 'PC.identity'
colnames(df)[colnames(df) == 'q..start'] <- 'q.start'
colnames(df)[colnames(df) == 'q..end'] <- 'q.end'
colnames(df)[colnames(df) == 's..start'] <- 's.start'
colnames(df)[colnames(df) == 's..end'] <- 's.end'

subset <- df 

# compare "any gene" to the total distribution ----
subset %>%
  ggplot(aes(x=(`bit.score`))) +
  geom_histogram(bins = 10)





library(lubridate)
# df$cfidob <- as.numeric(ymd(df$cfidob))
#tmp$Year <- format(as.Date(tmp$Release_Date), "%Y")
#tmp$Month <- format(as.Date(tmp$Release_Date), "%b")
#tmp$Day <- format(as.Date(tmp$Release_Date), "%d")
#tmp$DayOfYear <- as.numeric(format(as.Date(tmp$Release_Date), "%j"))

tmp <- subset
# Filter rows with Collection_Date
tmp_collection <- tmp %>% 
  #mutate_all(na_if,"") %>% 
  drop_na(Collection_Date)

tmp_collection_date <- 
  separate(tmp_collection, Collection_Date, into = c("Year", "Month", "Day"))

tmp_collection_date$Year <- as.numeric(tmp_collection_date$Year)

tmp_collection_date %>% 
 group_by(Year) %>%
  ggplot(aes(x=Year))+
  geom_bar()

d <- tmp_collection_date

ggplot(d,aes(x = PC.identity)) + 
  facet_wrap(Year~Geo_Location ,scales = "free") + 
  geom_histogram(bins = 10) #+

d %>% ggplot(aes(x = Year,
                 y = PC.identity,
                 color=Geo_Location,
                 alpha = 0.5)) + 
  geom_point(position=position_jitter(width=.1,height=2)) +
  geom_line( aes(group = Geo_Location), position=position_jitter(width=.1,height=1) ) +
  ylim(0,100)





d <- d %>% unique()
d <- d %>% filter(alignment.length >=10 )

# gene_var <- "CHST14"
#gene_var_d <- d %>% filter(Gene == gene_var) %>% group_by(key)
# gene_var_d

d2 <- d %>% 
  select(Accession, Geo_Location, PC.identity, mismatches, alignment.length, s.start, s.end, bit.score, Year) %>%
  #  tidyr::gather(key, value)
  tidyr::gather(key = key, value = value, -Accession, -Geo_Location, -Year, -s.start, -s.end)

ggplot(d,aes(x = PC.identity)) + 
  facet_wrap(~Geo_Location ,scales = "free") + 
  geom_histogram() #+
  #geom_vline(data  = gene_var_d, aes(xintercept = value, colour = key))

ggplot(d2,aes(x = (value) )) + 
  facet_wrap(~key,scales = "free") + 
  geom_histogram(bins = 10)

class(d2$Year)
d2 <- as_tibble(d2)
plot <- d2 %>% ggplot(aes(x = Year,
                 y = value,
                 color=Geo_Location,
                 alpha = 0.5,
                 group = Accession)) + 
  geom_point(position=position_jitter(width=.1,height=2)) +
  geom_line( aes(group = Geo_Location), position=position_jitter(width=.1,height=1) ) +
  ylim(0,100)+ 
  facet_wrap(~key, scales = "free") + labs(
  title = "Forward primer match TGCCTATGGTTCAGGGCAAG" )

library(plotly)
ggplotly(plot) 


x



























names(df_clivar)
names(df)

colnames(df_clivar)[colnames(df_clivar) == 'Chromosome'] <- 'Chr'
colnames(df)[colnames(df) == 'CHROM'] <- 'Chr'
colnames(df_clivar)[colnames(df_clivar) == 'GeneSymbol'] <- 'Gene'
colnames(df)[colnames(df) == 'GENE'] <- 'Gene'
colnames(df_clivar)[colnames(df_clivar) == 'PositionVCF'] <- 'Pos'
colnames(df)[colnames(df) == 'POS'] <- 'Pos'
colnames(df_clivar)[colnames(df_clivar) == 'ReferenceAlleleVCF'] <- 'Ref'
colnames(df_clivar)[colnames(df_clivar) == 'AlternateAlleleVCF'] <- 'Alt'
colnames(df_clivar)[colnames(df_clivar) == 'LABEL'] <- 'Label'
colnames(df)[colnames(df) == 'LABEL'] <- 'Label'

colnames(df)[colnames(df) == 'ID'] <- 'HGMD'
colnames(df_clivar)[colnames(df_clivar) == 'AlleleID'] <- 'ClinVar'
colnames(df_clivar)[colnames(df_clivar) == 'Name'] <- 'RefSeqVar'

colnames(df)[colnames(df) == 'VARIANT_CLASS'] <- 'Variant class'

colnames(df)[colnames(df) == 'Selective_pressure'] <- 'Selective pressure'
colnames(df)[colnames(df) == 'Number_of_paralogs'] <- 'No. of paralogs'

# drop several columns that have few results and not required on webpage
df <- df %>% select(-"PTM", -"Phosphorylation", -"Acetylation", -"Methylation", -"Ubiquitination", -"Glycosylation")
df_clivar <- df_clivar %>% select(-"RSnumber") # remove rsID as it is less than useless for variant-level data - misleading for multiallelic position. 

#df_clivar <- separate(df_clivar, RefSeqVar, into = c("RefSeq", "var"), sep = ":")
#df_clivar$RefSeq <- gsub("\\([^()]*\\)", "", df_clivar$RefSeq)

df_merge <- merge(df, df_clivar, all=TRUE )

4775 # total
1449 #overlap

df_merge$Database  <- ifelse(is.na(df_merge$ClinVar), NA, "HGMD + ClinVar")

df_merge$Database[is.na(df_merge$ClinVar)] <- "HGMD"
df_merge$Database[is.na(df_merge$HGMD)] <- "ClinVar"

# column order ----
df_merge %>% names()
df_merge <- df_merge %>% select("Gene", "Label",
                    "Chr",
                    "Pos",
                    "Database",
                    "RefSeqVar",
                    "HGVSc",
                    "HGVSp", 
                    "MAX_AF", everything())

# arrange so that Databases ClinVar + HGMD for a nice presentation, since arrange based on ChrPos is not really necessary
df_merge <- df_merge %>% 
  arrange(desc(Database))

# export table ----
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

df_merge_t <- 
  reactable(df_merge,
            compact = TRUE,
            searchable = TRUE,
            #elementId = "download-table",
            defaultPageSize = 10,
            defaultColDef = colDef(minWidth = 93 ),
            columns = list(
              "RefSeqVar" = colDef(minWidth = 200),  # overrides the default
              "HGVSc" = colDef(minWidth = 200),
              "HGVSp" = colDef(minWidth = 200),
              "DOMAINS_VEP" = colDef(minWidth = 350),
              "Chr" = colDef(minWidth = 70),
              "Database" = colDef(minWidth = 100),
              "Selective pressure" = colDef(minWidth = 100)
              ),
            filterable = TRUE,
            showSortable = TRUE,
            showPageSizeOptions = TRUE,
            striped = TRUE,
            highlight = TRUE
  )

df_merge_t







# Filter to find top genes to use in candidate study
# Here we will find the genes that are predicted pathogenic in several databases
# using thresholds we think are useful based on the original sources.

# total number of genes ---
df %>% group_by(Gene) %>% tally() 
df_total_count <- df %>% group_by(Gene) %>% tally() %>% tally()
df_total_count

# strict threshold for ALL ----
df %>% 
  filter(GDI_Phred < 3 ) %>% 
  filter( ExACpLI > 0.9 ) %>% 
  filter( LoFtool > 0.5 ) %>% 
  filter( denovo > 0.01 ) %>% 
  filter( RVIS < 0 ) %>% 
  group_by(Gene) %>% tally()
# result in too few overlaps

# strict threshold for ANY ----
df %>% 
  filter(
    GDI_Phred < 3 |
      ExACpLI > 0.9 |
      LoFtool > 0.5 |
      denovo > 1.0 |
      RVIS < 0 ) %>% 
  group_by(Gene) %>% tally()
# results in too many
# Note alternative filter logic methods are: ! (NOT), & (AND), | (OR)

# Percentage of gene kept on individual filter ----
# Instead, find genes where >3 filters have overlapping genes
f1 <- df %>% filter( GDI_Phred < 3 ) %>% group_by(Gene) %>% tally() # genes after filter 
f2 <- df %>% filter( ExACpLI > 0.9 ) %>%  group_by(Gene) %>% tally() # genes after filter 
f3 <- df %>% filter( LoFtool > 0.5 ) %>%  group_by(Gene) %>% tally() # genes after filter 
f4 <- df %>% filter( denovo > 1.0 ) %>%  group_by(Gene) %>% tally() # genes after filter 
f5 <- df %>% filter( RVIS < 0 ) %>% group_by(Gene) %>% tally() # genes after filter 

filter_percent_remain <- c(
 f1 %>% tally(name = "pc_f1") / df_total_count*100, # % of genes remain
 f2 %>% tally(name = "pc_f2") / df_total_count*100 ,# % of genes remain
 f3 %>% tally(name = "pc_f3") / df_total_count*100 ,# % of genes remain
 f4 %>% tally(name = "pc_f4") / df_total_count*100 ,# % of genes remain
 f5 %>% tally(name = "pc_f5") / df_total_count*100  # % of genes remain
)

filter_percent_remain <- as.data.frame(filter_percent_remain) # tidy
row.names(filter_percent_remain ) <- c("percent_gene_remain") # tidy
filter_percent_remain <- t(filter_percent_remain) # tidy
filter_percent_remain # percentage of genes remaining after each filter

# keep the genes present after 3-4 filters ----
filtered <- rbind (f1,f2,f3,f4,f5) %>% group_by(Gene) %>% tally() %>% arrange(desc(n))
filtered_top_genes <- filtered %>% filter(n > 3)
filtered_top_genes

# Apply to your data ----
# 1. Merge "filtered_top_genes" with your data.
# 2. If no genes match, either: 
# ---> (i) lower the overlap count from >3 to >2 or 
# ---> (ii) adjust the individual filter values to your preference. 


# NOT USED: find intersects with venn diagram ----
# -> if this was only <5 filters it would be one of the few times where venn diagram is useful.

# if (!require(devtools)) install.packages("devtools")
# devtools::install_github("yanlinlin82/ggvenn")
library("ggvenn")

x <- list( A = f1$Gene,
           B = f2$Gene,
           C = f3$Gene, 
           D = f4$Gene,
           E = f5$Gene)

# Default plot
ggvenn(x)
# ggplotly(ggvenn(x))
  

# Explore the distributions ----

subset <- df #%>% filter(CHROM > 15)

subset <- subset %>%
  separate(Protein_position, 
           into = c("Protein_position", "Protein_length"),
           sep = "/")

subset <- subset %>%
  separate(Protein_position, 
           into = c("Protein_position_start", "Protein_position_end"),
           sep = "-")

columns <-c("Protein_position_start", "Protein_position_end", "Protein_length" )
subset[, columns] <- lapply(columns, function(x) as.numeric(subset[[x]]))

subset$Protein_position_relative <- 
  (subset$Protein_position_start/subset$Protein_length)

# compare "any gene" to the total distribution ----
subset %>%
  ggplot(aes(x=log(`Selective pressure`))) +
  geom_histogram(bins = 100)

d <- subset %>% select(Gene, LoFtool, ExACpLI, RVIS, Essentiality, GDI_Phred) 
d <- d %>% unique()
gene_var <- "CHST14"
gene_var_d <- d %>% filter(Gene == gene_var) %>% group_by(key)
gene_var_d

d <- d %>% 
#  tidyr::gather(key, value)
  tidyr::gather(key = key, value = value, -Gene)


ggplot(d,aes(x = value)) + 
  facet_wrap(~key,scales = "free") + 
  geom_histogram() +
  geom_vline(data  = gene_var_d, aes(xintercept = value, colour = key))

ggplot(d,aes(x = log(value) )) + 
  facet_wrap(~key,scales = "free") + 
  geom_histogram()





z






subset %>%
  ggplot(aes(x=Protein_position_relative)) +
  geom_histogram(bins = 100, aes(y = ..density..)) +
  geom_density()

dataLine <- subset %>%
  group_by(Inheritance) %>%
  summarize(mean_x = mean(log(Selective_pressure) ))


p1 <- subset %>%
  ggplot(aes(x=log(Selective_pressure))) +
  geom_histogram(bins = 50, 
                 aes(y = ..density..,
                     fill=Inheritance)) +
  geom_density() +
  facet_grid(Inheritance ~ ., scales = "free") +
  geom_vline(data  = dataLine, aes(xintercept = mean_x)) +
  labs(title = "Slective pressure and inheritance",
       #subtitle = "Subtitle of the plot",
       caption = "All known GoF/LoF from HGMD and CliVar;
       selective pressure per gene
       versus inheritance pattern.",
       tag = "Fig. 1")

p1
ggplotly(p1)

# reorder genes by selective pressure
subset$Gene <- reorder(subset$Gene, subset$Selective_pressure)

#subset %>% 
#  ggplot(aes(x=Gene, y=Protein_position_relative )) +
#  geom_boxplot(aes(group = Gene)) +
#  geom_point(aes(color=log(Selective_pressure))) +
#  facet_grid(Inheritance ~ .)

p2 <- subset %>% 
  ggplot() +
  geom_point(aes(x=Protein_position_relative, 
                 y=Selective_pressure,
                 group = Gene)) 

p2log <- subset %>% 
  ggplot() +
  geom_point(aes(x=Protein_position_relative, 
                 y= log(Selective_pressure),
                 group = Gene)) 

p2
p2gg <- ggplotly(p2)
p2log
p2loggg <- ggplotly(p2log)

subplot(p2gg, p2loggg,  titleX = TRUE, titleY = TRUE, 
        margin = 0.1 ) %>%
  layout(xaxis = list(title = "relative protein position"), 
         xaxis2 = list(title = "relative protein position")
         )

# medians ----
tmp_subsetProtein_position_relative_median <-
subset %>%
  group_by(Gene) %>%
  summarise(Protein_position_relative_median = median(Protein_position_relative))

subset <- 
  merge(subset, tmp_subsetProtein_position_relative_median)

rm(tmp_subsetProtein_position_relative_median)

p3 <- subset %>% 
  ggplot() +
  geom_point( aes(x=Protein_position_relative_median, 
                 y=Selective_pressure,
             group = Gene))

p4 <- subset %>% 
  ggplot() +
  geom_point(aes(x=Protein_position_relative_median, 
                 y=log(Selective_pressure), 
                 group = Gene)) 
p3
p3g <- ggplotly(p3)
p4
p4g <- ggplotly(p4)

subplot(p3g, p4g,  titleX = TRUE, titleY = TRUE, 
        margin = 0.1 ) %>%
  layout(xaxis = list(title = "median relative protein position"), 
         xaxis2 = list(title = "median relative protein position")
  )

# Postion =/= pressure ----
subplot(p2gg, p2loggg, p3g, p4g,  titleX = TRUE, titleY = TRUE, 
        nrows = 2,
        margin = 0.1 )
# Combined we see that there is no reason to assume, or test, that protein position of GoF/LoF is associated with selective pressure. 

