# legends:
# If you want to get the plot to work inside the <details> tag, you could use <embed> instead of <iframe>, and it should look something like this:
#<div style = "text-align: left;">
#  <embed style="border: none;" src="./sample_plotly.html" dpi="300" width="70%" height="500px" />
#    </div>

library(dplyr)
library(tidyr)
library(plotly)

df <-
  read.csv("../data/goflof_HGMD2019_v032021_allfeat.csv",
           stringsAsFactors = FALSE, header = TRUE) %>%
  na.omit()

# Filter to find top genes to use in candidate study
# Here we will find the genes that are predicted pathogenic in several databases
# using thresholds we think are useful based on the original sources.

# total number of genes ---
df %>% group_by(GENE) %>% tally() 
df_total_count <- df %>% group_by(GENE) %>% tally() %>% tally()
df_total_count

# strict threshold for ALL ----
df %>% 
  filter(GDI_Phred < 3 ) %>% 
  filter( ExACpLI > 0.9 ) %>% 
  filter( LoFtool > 0.5 ) %>% 
  filter( denovo > 0.01 ) %>% 
  filter( RVIS < 0 ) %>% 
  group_by(GENE) %>% tally()
# result in too few overlaps

# strict threshold for ANY ----
df %>% 
  filter(
    GDI_Phred < 3 |
      ExACpLI > 0.9 |
      LoFtool > 0.5 |
      denovo > 1.0 |
      RVIS < 0 ) %>% 
  group_by(GENE) %>% tally()
# results in too many
# Note alternative filter logic methods are: ! (NOT), & (AND), | (OR)

# Percentage of gene kept on individual filter ----
# Instead, find genes where >3 filters have overlapping genes
f1 <- df %>% filter( GDI_Phred < 3 ) %>% group_by(GENE) %>% tally() # genes after filter 
f2 <- df %>% filter( ExACpLI > 0.9 ) %>%  group_by(GENE) %>% tally() # genes after filter 
f3 <- df %>% filter( LoFtool > 0.5 ) %>%  group_by(GENE) %>% tally() # genes after filter 
f4 <- df %>% filter( denovo > 1.0 ) %>%  group_by(GENE) %>% tally() # genes after filter 
f5 <- df %>% filter( RVIS < 0 ) %>% group_by(GENE) %>% tally() # genes after filter 

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
filtered <- rbind (f1,f2,f3,f4,f5) %>% group_by(GENE) %>% tally() %>% arrange(desc(n))
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

x <- list( A = f1$GENE,
           B = f2$GENE,
           C = f3$GENE, 
           D = f4$GENE,
           E = f5$GENE)

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

subset %>%
  ggplot(aes(x=log(Selective_pressure))) +
  geom_histogram(bins = 100)

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
subset$GENE <- reorder(subset$GENE, subset$Selective_pressure)

#subset %>% 
#  ggplot(aes(x=GENE, y=Protein_position_relative )) +
#  geom_boxplot(aes(group = GENE)) +
#  geom_point(aes(color=log(Selective_pressure))) +
#  facet_grid(Inheritance ~ .)

p2 <- subset %>% 
  ggplot() +
  geom_point(aes(x=Protein_position_relative, 
                 y=Selective_pressure,
                 group = GENE)) 

p2log <- subset %>% 
  ggplot() +
  geom_point(aes(x=Protein_position_relative, 
                 y= log(Selective_pressure),
                 group = GENE)) 

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
  group_by(GENE) %>%
  summarise(Protein_position_relative_median = median(Protein_position_relative))

subset <- 
  merge(subset, tmp_subsetProtein_position_relative_median)

rm(tmp_subsetProtein_position_relative_median)

p3 <- subset %>% 
  ggplot() +
  geom_point( aes(x=Protein_position_relative_median, 
                 y=Selective_pressure,
             group = GENE))

p4 <- subset %>% 
  ggplot() +
  geom_point(aes(x=Protein_position_relative_median, 
                 y=log(Selective_pressure), 
                 group = GENE)) 
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
