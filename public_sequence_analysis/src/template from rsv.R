# !diagnostics off
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# //////////////////////////////////////
##### Load data ####
# //////////////////////////////////////

# Multiple sequence alignment file
msa.1 <- 
  read.table(file="../public_data/data/05_thomas_msa/G_4tree_all_fewergaps.msa_G_protein.tabbed.split", 
             header = FALSE, 
             sep = " ", 
             stringsAsFactors = FALSE,
             colClasses = c("character"))

# Sample lables
msa.2 <- 
  read.table(file="../public_data/data/05_thomas_msa/G_4tree_all_fewergaps.msa_G_protein.tabbed.labels", 
             header = FALSE, 
             sep = "\t", 
             stringsAsFactors = FALSE)

# Rename label column
msa.1$label <- msa.2$V1
# rm(msa.2)

# move last column, label, to start
msa.1 <- msa.1 %>%
  select(label, everything())

# check data
head(msa.1) %>% 
  select(1:3)


# Add meta_data (location, date, etc.)
# list of viral samples and their ids
tmp1 <- read.table("../public_data/data/00_ncbi_data/ncbi_sequences_metadata_nucleotide_thomas.csv",
                   sep = ",", header = T, stringsAsFactors = FALSE)

## The metadata contains every submission (all genes)
## We will just be using the ones for our protein sequence.

# //////////////////////////////////////
#### Merge metadata and cleanup ####
# //////////////////////////////////////

colnames(msa.1)[colnames(msa.1) == "label"] <- "Accession"

# add the metadata
msa_meta_data <- 
  merge(x=msa.1,
        y=tmp1,
        by = "Accession",
        all = F)

rm(msa.2, msa.1)

head(msa_meta_data) %>% 
  select(1076:1084)

# select msa.1 position of interest
# Sequece of intest start ~V406 for ANH57864
# "KDPKP"
msa_meta_data[3,407:417]
msa_meta_data[3,1:10]

# Metadata columns - 1076:1084
msa_meta_data[3,1076:1084]

# Headers: 1 = accession, 2 = label, data = 3:868, metadata= 1076:1084
single_position  <- 
  msa_meta_data %>% 
  select(Accession, 411:417, 1076:1084)

# plot and analyse
# DPK_P_QTT
# p.P217T/S (V221 in our cohort)
# relative position V412 in public data
head(msa_meta_data %>% select(Accession, 411, 1076:1084))

msa_meta_data %>% select(Accession, 411, 1076:1084) %>% 
  group_by(V410) %>%
  count() %>%
  arrange(desc(n))

# Will want to split the Geo_Location into just countries.
msa_meta_data %>% select(Accession, 411, 1076:1084) %>% 
  group_by(Geo_Location) %>%
  count()

tmp <- separate(msa_meta_data %>% select(Accession, 411, 1076:1084), 
                Geo_Location, 
                into = c("Geo", "other"),
                sep = "[:]"
)

tmp %>% 
  group_by(Geo) %>%
  count()

# Global total count
tmp %>% 
  group_by(V410, Geo) %>%
  count() %>% 
  ungroup() %>%
  ggplot(aes(y=V410, x=n, color=Geo))+
  geom_point()+
  theme(legend.position="none")

tmp %>% 
  ggplot(aes(y=V410, fill=Geo))+
  geom_bar(position = "stack")+
  theme(legend.position="none")


# Frequency per country
Geo_total <- 
  tmp %>% 
  group_by(Geo) %>%
  count(name="total")

tmp_2 <- tmp %>% 
  group_by(V410, Geo) %>%
  count(name="count")

tmp_3 <- merge(x=Geo_total, y=tmp_2)
tmp_3$Freq <- (tmp_3$count/tmp_3$total)

# Country freq count
tmp_3 %>% 
  ggplot(aes(y=V410, x=Freq, color=Geo))+
  geom_point()+
  theme(legend.position="none")


tmp_3 %>% 
  filter(V410=="S") %>% 
  filter(Freq > 0.05)%>% 
  arrange(desc(Freq))

tmp_3 %>% 
  filter(V410=="T") %>% 
  filter(Freq > 0) %>% 
  arrange(desc(Freq))


# Country freq count
Geo_total %>%
  ggplot(aes(y=Geo, x=total, color=Geo))+
  geom_point()+
  theme(legend.position="none")

Geo_total %>% 
  arrange(desc(total))  


# Time
# Global total count
tmp %>% 
  group_by(V410, Geo) %>%
  count() %>% 
  ungroup() %>%
  ggplot(aes(y=V410, x=n, color=Geo))+
  geom_point()+
  theme(legend.position="none")


tmp %>% 
  #group_by(V410, Geo, Release_Date)  
  group_by(as.Date(Release_Date)) %>%
  count()

# format months as categorical for plink covariates
library(lubridate)
# df$cfidob <- as.numeric(ymd(df$cfidob))

#tmp$Year <- format(as.Date(tmp$Release_Date), "%Y")
#tmp$Month <- format(as.Date(tmp$Release_Date), "%b")
#tmp$Day <- format(as.Date(tmp$Release_Date), "%d")
#tmp$DayOfYear <- as.numeric(format(as.Date(tmp$Release_Date), "%j"))

# Filter rows with Collection_Date
tmp_collection <- tmp %>% 
  mutate_all(na_if,"") %>% 
  drop_na(Collection_Date)

tmp_collection_date <- 
  separate(tmp_collection, Collection_Date, into = c("Year", "Month", "Day"))

tmp_collection_date$Year <- as.numeric(tmp_collection_date$Year)

tmp_collection_date %>% 
  group_by(Year) %>%
  count() %>%
  ggplot(aes(x=Year, y=n))+
  geom_point()

tmp_var_per_year <- 
  tmp_collection_date %>% 
  group_by(Year, V410) %>%
  count(name = "n_var_per_year") 

tmp_total_per_year <- 
  tmp_collection_date %>% 
  group_by(Year) %>%
  count(name = "n_total_per_year") 


tmp_per_year <- 
  merge(x=tmp_total_per_year,
        y=tmp_var_per_year)

tmp_per_year$Freq_per_year <- 
  (tmp_per_year$n_var_per_year / tmp_per_year$n_total_per_year)

tmp_per_year %>% 
  ggplot(aes(x=Year, y=Freq_per_year, color=V410))+
  geom_point()+
  geom_line(aes(group = V410))

tmp_per_year %>% 
  ggplot(aes(x=Year, y=Freq_per_year, color=V410))+
  geom_point()+
  geom_smooth(method = lm, alpha = 0.1, 
              show.legend = FALSE)
geom_line(aes(group = V410))


tmp_per_year %>%
  filter(Year==2006)

tmp_collection_date %>% filter(Year < 2006) %>%
  group_by(Authors, Year, Geo) %>%
  count()

tmp_collection_date %>%
  group_by(Year) %>%
  count()

xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# //////////////////////////////////////
#### Set REF and ALT ####
# //////////////////////////////////////

# df <- msa_meta_data %>% select(Accession, 1:1075)
df <- msa_meta_data %>% 
  select(1:1074) %>%
  tidyr::gather(var_pos, var, 2:1074, factor_key=TRUE)
  # select(1:25) %>% 
 # tidyr::gather(var_pos, var, 2:25, factor_key=TRUE)

colnames(df)[colnames(df) == "Accession"] <- "label"

var_count <- df %>% 
  group_by(var_pos, var) %>% 
  tally(name = "var_count")

# For each position count and rank frequency of residue
var_rank <- var_count %>% 
  group_by(var_pos) %>%
  mutate(rank = rank(desc(var_count))) %>%
  arrange(rank)

# Name the most frequent residue as REF.
var_rank$binary <- var_rank$rank
var_rank$binary[var_rank$rank == "1"] <- "REF"
var_rank$binary[var_rank$rank > "1"] <- "ALT"

x <- var_rank %>% select(var_pos, var, binary)

d <- merge(x=df,
      y=x)  %>% 
  select(var_pos, label, binary)

#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#### Test a corr measure for variants ####

# convert each individual (row) and variant (col) to 0/1
d$var_pos <- as.numeric( str_replace_all(d$var_pos, "V", "") )

d$binary[d$binary == "REF"] <- "0"
d$binary[d$binary == "ALT"] <- "1"

d$binary <- as.numeric(d$binary)

dw <- spread(d, var_pos, binary)
tmp_keep <- dw 

# meta data
# Collection date
tmp <- tmp1
rm(tmp1)
# format months as categorical for plink covariates
library(lubridate)
# df$cfidob <- as.numeric(ymd(df$cfidob))
#tmp$Year <- format(as.Date(tmp$Release_Date), "%Y")
#tmp$Month <- format(as.Date(tmp$Release_Date), "%b")
#tmp$Day <- format(as.Date(tmp$Release_Date), "%d")
#tmp$DayOfYear <- as.numeric(format(as.Date(tmp$Release_Date), "%j"))

# Filter rows with Collection_Date
tmp_collection <- tmp %>% 
  mutate_all(na_if,"") %>% 
  drop_na(Collection_Date)

tmp_collection_date <- 
  separate(tmp_collection, Collection_Date, into = c("Year", "Month", "Day"))

tmp_collection_date$Year <- as.numeric(tmp_collection_date$Year)

tmp_collection_date %>% 
  group_by(Year) %>%
  count() %>%
  ggplot(aes(x=Year, y=n))+
  geom_point()

tmp_per_year %>%
  filter(Year==2006)

# Rename the seqID header to match the nex file - tip.label 
colnames(tmp_collection_date)[colnames(tmp_collection_date) == "Accession"] <- "label"

years <- tmp_collection_date %>%
  select(label, Year)

# Merge
dw_meta <- 
  merge(x = years, 
        y = dw, 
        by = "label", all.y = T)

row.names(dw_meta) <- dw_meta$label
dw_meta <- within(dw_meta, rm(label))


##############################################################

x
x
x
x
x

# Year function ----
# remove any year with <2 sample (for cor later)
dw_meta$accession <- rownames(dw_meta)
dw_meta <- dw_meta %>%
  group_by(Year) %>% 
  filter(n() > 1) %>%
  ungroup() %>%
  select(- accession)

df.list.names <- dw_meta$Year %>% unique() %>% as.list()

df.list <- lapply(df.list.names, function(x) {
  dw <- dw_meta %>% 
    filter(Year %in% x ) %>%
    select(- Year)
}
)

names(df.list) <- df.list.names %>% as.data.frame()
names(df.list)
df.list[["2016"]]


#### Cor cutoff ####
library('caret')

# as function ----

#df.list <- list(dw_a, dw_a2, dw_b, dw_b2, dw_c, dw_c2, dw_d, dw_d2, dw_e, dw_e2, dw_g, dw_g2, dw_h)
#df.list.names <- c("1990", "1992", "1994", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014")

names(df.list)
df.listna <- df.list[["1999"]] %>% as.data.frame()

a <- cor(df.list[["1999"]] , use="pairwise.complete.obs") %>% as.data.frame() #%>% as.numeric()
a[is.na(a)] <- 0
b<- a[, colSums(a != 0) > 0]
df2 <- b[ rowSums(b)!=0, ] %>% as.matrix()
pca_res <- prcomp(df2, scale = TRUE)


names(df.list)
df.list.names
# note that this function will halt if a cor cannot be completed due to fewer that 2 samples in a year.
# a filter was run early to remove any year with < 2 samples. 
res <- lapply(df.list, function(x) {
  a <- cor(x, use="pairwise.complete.obs") %>% as.data.frame()
  a[is.na(a)] <- 0
  b<- a[, colSums(a != 0) > 0]
  c <- b[ rowSums(b)!=0, ] 
  df2<- as.matrix(c)
  
  # library(factoextra)
  pca_res <- prcomp(df2, scale = TRUE)
#  #fviz_eig(pca_res)
  
  
  # Variance explained
  var_explained <- pca_res$sdev^2/sum(pca_res$sdev^2)
  pca_res_x <- pca_res$x %>% as.data.frame()
  pca_res_x$Var <- as.numeric( row.names(pca_res_x))
  
  pca_res_x$Var_explained <- var_explained
  pca_res_x$Group <- "Group 1"
  
  pca_res_x <- pca_res_x %>% as.data.frame() %>% 
    select(PC1, Var, Var_explained )
}
)

res[[1]]
names(res)
df.list.names
head(res)

# add year ID ----
# main result ----
for (n in names(res))
  res[[n]]['name'] = n
res_named = do.call(rbind, res)

res_plot <- res_named %>%
  ggplot(aes(x=Var, y=Year))+
  geom_tile(aes(fill=(Var_explained)))+
  scale_fill_gradient(low = "orange", 
                      high = "brown")+
  theme(legend.position="bottom", 
        panel.background = element_rect("#F7F7F7"))

res_plot
library(plotly)
ggplotly(res_plot)


#pca_res_long <- gather(pca_res_x, PC, measurement, PC1:PC5, factor_key=TRUE)

# for loop to get each PC1 for every year


#################################
x
x
x
x
#################################

x
x
x
x
x


pca_res_long %>%
  ggplot(aes(x=Var, y=PC))+
  geom_tile(aes(fill=(measurement)))+
  scale_fill_gradient(low = "orange", 
                      high = "brown")+
  theme(legend.position="bottom", 
        panel.background = element_rect("#F7F7F7"))
# ./public_data/PC_values.pdf 4x7

#### Variance explained all variants ####
pca_res_long %>%
  # filter(Var ==221) %>%
  group_by(PC) %>%
  summarise(min = min(measurement),
            median = median(measurement),
            max = max(measurement),
            sd = sd(measurement)) %>%
  as.data.frame()

pca_res_long %>%
  group_by(PC) %>%
  filter(PC == "PC1" || PC == "PC2" ) %>%
  #  filter(Var == 221) %>%
  ggplot(aes(x=Var, y=measurement))+
  geom_point()+
  theme(legend.position="bottom", 
        panel.background = element_rect("#F7F7F7"))+
  facet_grid(.~PC)
#  summarise(mean = mean(measurement),         )

pca_res_long %>%
  filter(PC == "PC1" || PC == "PC2" ) %>%
  ggplot(aes(x=PC, y=measurement))+
  geom_jitter() +
  theme(legend.position="bottom", 
        panel.background = element_rect("#F7F7F7"))

# PCA for each sample?






#////////////////////////////////////////#
# Try SVD as Chris said
# Need to transpose df1
library('caret')
df1 <- dw 
df3 = cor(t(df1))

library(factoextra)
pca_res_2 <- prcomp(df3, scale = TRUE)
fviz_eig(pca_res_2)

# Quick look
pca_res_2$x[1:5,1:3]

# Variance explained
var_explained_2 <- pca_res_2$sdev^2/sum(pca_res_2$sdev^2)
var_explained_2[1:5]

pca_res_x_2 <- pca_res_2$x %>% as.data.frame()

pca_res_x_2$label <- as.numeric( row.names(pca_res_x_2))

# add_sample_id_info 
pca_res_x_2_samples <- merge(x = sample_list, 
                             y= pca_res_x_2, by="label")

library(ggrepel)
pca_res_x_2_samples %>% 
  ggplot(aes(x=PC1,y=PC2)) +
  geom_point(shape=21, aes(fill = strain),
             alpha=0.5, 
             position=position_jitter(width=1,height=1))+
  theme(legend.position="bottom", 
        panel.background = element_rect("#F7F7F7"))+
  geom_text_repel(
    data = subset(pca_res_x_2_samples, 
                  infection=="repeat"), 
    aes(label = infection), size = 4,
    box.padding = unit(1, "lines")
  )
# ./update_pca_figures/PCA_samples_before_prune_pc1.pdf 6x9

pca_res_x_2_samples %>% 
  ggplot(aes(x=PC2,y=PC3)) +
  geom_point(shape=21, aes(fill = strain),
             alpha=0.5, 
             position=position_jitter(width=0.1,height=0.1))+
  theme(legend.position="bottom", 
        panel.background = element_rect("#F7F7F7"))+
  geom_text_repel(
    data = subset(pca_res_x_2_samples, 
                  infection=="repeat"), 
    aes(label = infection), size = 4,
    box.padding = unit(1, "lines")
  )

mytheme <-
  theme(legend.position="none", 
        panel.background = element_rect("#F7F7F7"))

mytheme2 <-
  theme(legend.position="right", 
        panel.background = element_rect("#F7F7F7"))

mypoints< - 
  geom_point(shape=21, aes(fill = strain),
             alpha=0.5,
             position=position_jitter(width=0.05,
                                      height=0.05))

mytext<-geom_text_repel( 
  data = subset(pca_res_x_2_samples, infection=="repeat"), aes(label = infection), size = 4,
  box.padding = unit(1, "lines") 
)


p1 <- pca_res_x_2_samples %>% 
  ggplot(aes(x=PC1,y=PC2))+
  mytheme + mypoints + mytext

p2 <- pca_res_x_2_samples %>% 
  ggplot(aes(x=PC2,y=PC3))+
  mytheme + mypoints + mytext

p3 <- pca_res_x_2_samples %>% 
  ggplot(aes(x=PC3,y=PC4))+
  mytheme + mypoints + mytext

p4 <- pca_res_x_2_samples %>% 
  ggplot(aes(x=PC4,y=PC5))+
  mytheme + mypoints + mytext

p5 <- pca_res_x_2_samples %>% 
  ggplot(aes(x=PC5,y=PC6))+
  mytheme + mypoints + mytext

p6 <- pca_res_x_2_samples %>% 
  ggplot(aes(x=PC6,y=PC7))+
  mytheme + mypoints + mytext

p7 <- pca_res_x_2_samples %>% 
  ggplot(aes(x=PC7,y=PC8))+
  mytheme + mypoints + mytext

p8 <- pca_res_x_2_samples %>% 
  ggplot(aes(x=PC8,y=PC9))+
  mytheme2 + mypoints + mytext

require(gridExtra)
grid.arrange (p1,p2,p3,p4,p5,p6,p7,p8,  ncol=3, top = "Cohort Principal component analysis", left = "")
# ./update_pca_figures/PCA_samples_before_prune_pc1_9.pdf 24x36

# df2 is the variance for each residue in the gene.
# df3 is the population structure in cohort (sum of all residue variants). 



# The same is done in a single command:
princomp(tmp_keep,cor=TRUE)
princomp(covmat=cor(tmp_keep))




#   Reduced LD variants
e1 <- eigen(as.matrix(tmp_keep))
e2 <- eigen(dw.cor.Rsq.reduced)

sqrt(e2$values)

#Proportion of variance:
e2_prop <- 
  e2$values/sum(e2$values)
e2_prop <- as.data.frame(e2_prop)

e2_prop$PC <- row.names(e2_prop)
e2_prop %>%
  ggplot(aes(y=as.numeric( e2_prop) ,
             x=as.numeric(PC) )) +
  geom_bar(stat="identity")
# ./update_pca_figures/eigenval_after_prune.pdf 4x7














# Show the remaing SNPs being tested that are not in LD
# Gather matrix to df
dw.cor.Rsq.df <- dw.cor.Rsq.reduced %>% as.data.frame() 
dw.cor.Rsq.df$pos <- row.names(dw.cor.Rsq.df)

dw.cor.Rsq.df <- 
  gather(dw.cor.Rsq.df,
         key = "pos_B",
         value = "Rsqr",
         -pos)

dw.cor.Rsq.df$pos <- as.numeric(dw.cor.Rsq.df$pos) 
dw.cor.Rsq.df$pos_B <- as.numeric(dw.cor.Rsq.df$pos_B)

dw.cor.Rsq.df %>%
  filter(Rsqr < 1) %>%
  ggplot(aes(x=pos, y=pos_B, color=Rsqr)) +
  geom_point()+
  scale_color_gradient(low = "pink", high = "red", na.value = NA)+
  theme_light()
# nucleo_corr_plot_Rsq_0.8_full.pdf 5x6

# Later testing should be restricted to this test list. 

Rsqr_variants <- dw.cor.Rsq.df$pos%>% unique()



x
xxxxxxxxxxxxxxooooooooxxxxx
x







x
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# I can't get corrr to order the plot correctly:
#library(corrr)
#res.cor <- correlate(dw)
#res.cor$rowname <- as.numeric(res.cor$rowname)
#class(res.cor$rowname)

#res.cor %>% gather(-rowname, key = "colname", value = "cor") %>% filter(abs(cor) > 0.8)
#any_over_90 <- function(x) any(x > .8, na.rm = TRUE)
#res.cor_90 <- res.cor %>% focus_if(any_over_90, mirror = TRUE)

# Visualize the distribution of the correlation coefficients:
#res.cor %>% shave() %>% stretch(na.rm = TRUE) %>%  ggplot(aes(r)) + geom_histogram(bins = 10)
#res.cor$rowname <- row.names(res.cor)

# Turn your 'day' column into a character vector
#res.cor$rowname <- as.character(res.cor$rowname)
# Then turn it back into a factor with the levels in the correct order
#res.cor$rowname <- factor(res.cor$rowname, levels=unique(res.cor$rowname))

#res.cor %>% shave() %>% rplot()





#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# 
# test_set_freqRefAlt <-test_set_RefAlt %>%   group_by(var_pos, binary) %>%   tally(n="total") %>%   mutate(freq = total / sum(total)) %>% filter(binary=="ALT")
#d <- test_set_freqRefAlt %>%  select(var_pos, freq)  %>%  as.data.frame()
#d[as.character(d$freq)] <- as.list(d$freq)
#d <- within(d, rm(var_pos))


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

#x <- test_set_RefAlt %>% group_by(var_pos, infection, binary) %>% tally() 
#x2 <- x %>% group_by(var_pos) %>% summarise(total = sum(n)) 
#x3 <- merge(x, x2) %>%  group_by(var_pos, infection, binary, n, total) %>% summarise(FREQ= ((n) / (total)))
# Select freq >5pc in case or controls and list 
# test_set_RefAlt_pos <- x3 %>% filter(binary == "ALT" & FREQ > 0.05) %>% ungroup() %>%  select(var_pos) %>%   group_by(var_pos) %>%  tally() %>% filter(n=="2") %>% select(var_pos)

# To remove variants that are within 20% similar freq case/controls
# Easier to see - all Pvalue ~1 will be masked out. 
# Select a smaller subset
q <- test_set_RefAlt %>% 
  select(var_pos, 
         infection, 
         binary, 
         strain, 
         race_simple_dev, 
         efsex)
q %>% select(var_pos) %>% unique() %>% tally()

# Add distance as numeric
q$dist <- as.numeric(test_set_RefAlt$pairwise_dist)

# Make factors for glm
q$infection <- as.factor(q$infection)
q$race_simple_dev <- as.factor(q$race_simple_dev)
q$efsex <- as.factor(q$efsex)
q$race_simple_dev <- as.factor(q$race_simple_dev)

# q <- merge( q , test_set_RefAlt_pos)

fit <- 
  lapply(split(q,
               q$var_pos, drop = TRUE),
         function(x) 
           coef(summary(glm(
             infection ~ 
               binary+dist+efsex,
             family="binomial",
             data=x)))[,'Pr(>|z|)']
  )

fit <- as.data.frame(fit) 

# Wide to long transpose
library(data.table)
fit_long <-transpose(fit)

# get row and colnames in order
colnames(fit_long) <- rownames(fit)
rownames(fit_long) <- colnames(fit)

# rownames to column
fit_long$var_pos <- row.names(fit_long)   

fit_long$position <- as.numeric( str_replace_all(fit_long$var_pos, "V", "") )

fit_long %>%
  ggplot(aes(x=position, y=(-log10(binaryREF)) ))+
  geom_point()+
  theme(axis.title.x=element_blank(), 
        panel.background = element_rect("#F7F7F7"))+ labs(x = "Position", y = "-log10 (Pvalue) ")
#scale_y_continuous(trans = "reverse")
# 3x6
# update_G_phylogeny_nucleic_acid_msa_logist_covariates.pdf 


# Restrict to test set passing LD threshold
Rsqr_variants

subset(fit_long, position %in% Rsqr_variants) %>%
  ggplot(aes(x=position, y=(-log10(binaryREF)) ))+
  geom_point()+
  theme(axis.title.x=element_blank(), 
        panel.background = element_rect("#F7F7F7"))+ labs(x = "Position", y = "-log10 (Pvalue) ")+
  geom_hline(linetype="dotted", 
             yintercept=-log10(0.0031))
#scale_y_continuous(trans = "reverse")
# 3x6
# update_G_phylogeny_nucleic_acid_msa_logist_covariates_Rsqr_variants.pdf 

#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#### Effect size  ####
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


exp(0.1934154)

#///////////////////////////////////
####  Glm all versus strain B
####  OR and CI ####
#///////////////////////////////////
glm_1<- glm(infection ~  binary+dist+efsex, family="binomial", data= (q %>% filter(var_pos == "221" )) )

exp(glm_1$coefficients[2])
# So here increasing x by 1 unit multiplies the mean value of Y by exp(β1)= 51.08
glm_2<- glm(infection ~  binary+dist+efsex, family="binomial", data= (q %>% filter(var_pos == "V221" ) %>% filter(strain == "B") ) )

exp(glm_2$coefficients[2])

require(MASS)
CI <- rbind(
  (exp(cbind(coef(glm_1), confint(glm_1))) %>% as.data.frame() %>% .[2,]
  ),
  (exp(cbind(coef(glm_2), confint(glm_2)))  %>% as.data.frame() %>% .[2,]
  )
)

CI$group <- rownames(CI)
CI$min <- as.numeric( CI$`2.5 %` )
CI$max <- as.numeric( CI$`97.5 %` )
CI$OR <-  as.numeric( CI$V1 )

CI %>%
  ggplot(aes(y= group, x=OR, group=group )) +
  geom_point( size=3)+
  geom_errorbar(aes(xmin=min, xmax=max), width=.1)


summary(glm_1)
summary(glm_2)

exp(confint(glm_1))

P_1 <- coef(summary( glm_1 ))[,'Pr(>|z|)'] %>%  as.data.frame() %>% .[2,]
P_2 <- coef(summary( glm_2 ))[,'Pr(>|z|)']


coef(summary(
  glm(infection ~binary+dist+efsex, 
      family="binomial", 
      data= (q %>% filter( var_pos == "V221" )))
)[,'Pr(>|z|)']
)

tmp <- glm(infection ~binary+dist+efsex, 
           family="binomial", 
           data= (q %>% filter( var_pos == "V221" )))

coef(summary(tmp)[,'Pr(>|z|)'])

#///////////////////////////////////
####  Other testing ####
#///////////////////////////////////

# Use the glm to extract the sigma and degrees of freedom
# Then get the effect size using these values and emmeans
library(emmeans)
subset(fit_long, position %in% Rsqr_variants) %>%
  select(var_pos) %>% unique()

# Top hit effect size for full cohort vs strian B only 
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V210" )) )
q %>% filter(var_pos == "V210" ) %>% group_by(infection, binary) %>% tally()
emmeans(glm, "binary")
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) # %>%as.data.frame() %>% select(effect.size)

confint(glm, level = 0.95)
effectsize::effectsize(glm)

require(MASS)
exp(cbind(coef(glm), confint(glm)))  

# exponentiate the coefficients and interpret them as odds-ratios
exp(glm$coefficients)
# for a one unit increase in binaryREF, the odds of being repeat infect increase by a factor of 0.3.

summary(glm)


# Top hit B strain only ES
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V210" ) %>% filter(strain == "B") ) )
q %>% filter(var_pos == "V210" ) %>% filter(strain == "B") %>% group_by(infection, binary) %>% tally()
emmeans(glm, "binary")
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) # %>%as.data.frame() %>% select(effect.size)

confint(glm)



# The effect sizes for full cohort of all tested variants are:
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V20" )) )
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) %>%as.data.frame() %>% select(effect.size)
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V103" )) )
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) %>%as.data.frame() %>% select(effect.size)
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V209" )) )
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) %>%as.data.frame() %>% select(effect.size)
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V210" )) )
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) %>%as.data.frame() %>% select(effect.size)
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V213" )) )
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) %>%as.data.frame() %>% select(effect.size)
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V221" )) )
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) %>%as.data.frame() %>% select(effect.size)
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V229" )) )
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) %>%as.data.frame() %>% select(effect.size)
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V232" )) )
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) %>%as.data.frame() %>% select(effect.size)
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V270" )) )
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) %>%as.data.frame() %>% select(effect.size)
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V279" )) )
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) %>%as.data.frame() %>% select(effect.size)
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V299" )) )
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) %>%as.data.frame() %>% select(effect.size)
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V300" )) )
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) %>%as.data.frame() %>% select(effect.size)
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V304" )) )
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) %>%as.data.frame() %>% select(effect.size)
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V305" )) )
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) %>%as.data.frame() %>% select(effect.size)
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V314" )) )
eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) %>%as.data.frame() %>% select(effect.size)
glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V315" )) )

#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


x
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
####  Redo effect size checks  ####
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# interpret coeff
# http://environmentalcomputing.net/interpreting-coefficients-in-glms/

# Confidence interval 
# https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/confint.html


# Top hit effect size for full cohort vs strian B only 

glm<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V210" )) )

q %>% filter(var_pos == "V210" ) %>% group_by(infection, binary) %>% tally()

emmeans(glm, "binary")

eff_size(emmeans(glm, "binary"), sigma = sigma(glm), edf = df.residual(glm)) # %>%as.data.frame() %>% select(effect.size)

summary(glm)
confint(glm)
interpret_parameters(glm)
glm$coefficients
exp(glm$coefficients[2])

# The estimates (coefficients of the predictors – numeracy and anxiety) are now in logits. The coefficient of numeracy is: 1.94556, so that a one unit change in numeracy produces approximately a 1.95 unit change in the log odds (i.e. a 1.95 unit change in the logit).

# (exp) From the signs of the two predictors, we see that numeracy influences admission positively, but anxiety influences survival negatively.We can’t tell much more than that as most of us can’t think in terms of logits. Instead we can convert these logits to odds ratios. We do this by exponentiating each coefficient. (This means raise the value e –approximately 2.72–to the power of the coefficient. e^b).

#So here increasing x by 1 unit multiplies the mean value of Y by exp(β1)="1.25" (or whatever). The same thing is true for negative binomial glms as they have the same link function.


# Top hit B strain only ES
glm2<- glm(infection ~ binary+dist, family="binomial", data= (q %>% filter(var_pos == "V210" ) %>% filter(strain == "B") ) )
q %>% filter(var_pos == "V210" ) %>% filter(strain == "B") %>% group_by(infection, binary) %>% tally()
emmeans(glm2, "binary")
eff_size(emmeans(glm2, "binary"), sigma = sigma(glm2), edf = df.residual(glm2)) # %>%as.data.frame() %>% select(effect.size)

confint(glm2, 'binaryREF')
interpret_parameters(glm2)


require(effectsize)

x
# try as lm
p <- q %>% filter(var_pos == "V210" )%>% select(infection, binary, dist)

p$infection <- as.numeric(p$infection)

lm1 <- lm(infection ~ binary+dist, data= (p) )
summary(lm1)
effectsize(lm1)
anova_table <- anova(lm1)
effectsize(anova_table)

x


interpret_parameters(glm)
coef(standardize(model))

x
interpret_parameters(glm)
coef(standardize(glm))
x


# using caret package

library(caret)
varImp(glm)

V = caret::varImp(glm)

ggplot2::ggplot(V, aes(x=reorder(rownames(V),Overall), y=Overall)) +
  geom_point( color="blue", size=4, alpha=0.6)+
  geom_segment( aes(x=rownames(V), xend=rownames(V), y=0, yend=Overall), 
                color='skyblue') +
  xlab('Variable')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip() 
x

(q$dist) %>% hist()
as.numeric(q$infection) %>% hist()
as.numeric( as.factor(q$binary) ) %>% hist()

#
anova(glm)

RES = residuals(glm)  
qqnorm(RES)  
qqline(RES)  
x


# https://stackoverflow.com/questions/28633378/r-cohens-d-for-group-sizes-2-using-glm
glm<- glm(infection ~ binary+dist+strain, family="binomial", data= (q %>% filter(var_pos == "V210" )) )
ibrary(dplyr)      #%>%
library(emmeans)    #emmeans
library(DescTools)  #EtaSq
library(car)        #sigmaHat
library(ARTool)

q %>% head() %>% as_tibble()
p
p$binary <- as.factor(p$binary)
m.art = art(infection ~ binary, data= (p) )
m.art.anova = anova(m.art)
print(m.art.anova, verbose=TRUE)
m.art.anova$eta.sq.part = with(m.art.anova, `Sum Sq`/(`Sum Sq` + `Sum Sq.res`))
m.art.anova


# my data version
x2.contrasts = summary(pairs(emmeans(glm, "strain")))

x2.contrasts$d = x2.contrasts$estimate / sigmaHat(glm)
x2.contrasts

# Results are averaged over the levels of: X1 
# P value adjustment: tukey method for comparing a family of 3 estimates

x2.contrasts.ci = confint(pairs(emmeans(glm, ~ "strain"))) %>%
  mutate(d = estimate / sigmaHat(glm)) %>%
  cbind(d = plyr::ldply(.$d, psych::d.ci, n1 = 100, n2 = 100))

x2.contrasts.ci



x2.contrasts.ci %>%
  ggplot(aes(x=contrast, y=d, ymin=d.lower, ymax=d.upper)) +
  geom_pointrange() +
  coord_flip()

x










x









q %>% select(var_pos) %>% unique() %>% tally()
.05/169 = .00029
.0004784832

test_set_RefAlt %>% 
  group_by(var_pos, var, infection, binary) %>%
  tally() %>%
  filter(var_pos == 'V126' )

q %>% 
  group_by(var_pos, infection, binary) %>%
  filter(var_pos == 'V126') %>%
  tally() %>%
  ungroup() %>%
  select(infection, binary,n) 

p <- q %>% filter(var_pos == 'V221')
summary(glm(infection ~ 
              binary+dist+efsex,
            family="binomial",
            data=p))

# correct for:
# asthma, sex, race
x <- fit_long
x$row <- "G protein"
x %>%
  ggplot(aes(x=position, y=row, fill=(-log10(binaryREF)) ))+
  geom_tile()+
  theme(axis.title.x=element_blank(), 
        panel.background = element_rect("#F7F7F7"))+
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)

# Note, within I have added annotation to the master data set to have each SNP per sample in single line. It is possible to run the glm on s summary (number of case/control per SNP) however, it is easy to incorrectly summary the numbers since there are many multiallelic postions and missing in one group, etc. Therefore we have each SNP*sample, approx (350*1500)=525,000 and keeping only SNPs that can be tested as present in both groups, approx 104,000. Check the prep source to see a few varaiants that are present only in cases.
# Merge the binary genotype data back onto individual subjects.
# Number of test SNPs = 683. 
# Pval sig - .05/308 = .00016 (1.6e-4)


tmp1_binary_list <- tmp1_binary %>%
  ungroup() %>%
  select(var_pos, var, binary) %>%
  unique()

head(merged_long_clinical)
merged_longer <- merge(x=merged_long_clinical, 
                       y=tmp1_binary_list)

# We will only test variants that are at least present in repeat infection. 
# V1 - 5 only present in controls. 
# V6 is the first SNP (and >1 alt) but only controls. 
# V11 is the first SNP present in both case and controls. 
# base lists SNPs to be tested (i.e. V11 will be first SNP)
# Add a column to base to flag test SNPS
base$testSNP <- "test"
TESTlist <- base %>% select(var_pos, var, testSNP) %>% unique()
merged_testSNPs <- merge(merged_longer, TESTlist) 
rm(TESTlist)
rm(merged_long)
rm(merged_longer)
rm(df)
rm(base)

# df contains a summary per SNP position.
# merged_testSNPs can now be tested. 
# Check to replace "NA" with a sybol if not already done. 
# Rough give to expect top hist:
# Chi
# Single strain top hits:
# V891 0.07571747
# V314 0.08421856
# V120 0.11513685

# For GLM;
# Family	Default Link Function
# binomial	(link = "logit")

# Loop for every SNPtest.
# Then add the claade group 
# Intro to stat learn p131

df <- merged_testSNPs %>% 
  select(strain, var_pos, position, binary, infection,
         efsex, efgestagewk, race_simple_dev, fy_current_asthma_dev_2) %>%
  ungroup()

# Need to remove na in ashtma
df[df==""] <- NA
df_asthma <-
  na.omit(df, cols=c("fy_current_asthma_dev_2"))

df_asthma$infection <- as.factor(df_asthma$infection)
# split must be set to drop level if not present, otherwise it will factors rows with no data.

# Dichotomous predictor, dichotomous outcome
# binary predictor, infection outcome
# [,'Pr(>|z|)'] shows the Pval only
fit <- lapply(split(df_asthma, 
                    df_asthma$var_pos, drop = TRUE), 
              function(x) 
                coef(summary(glm(infection ~ binary+strain+fy_current_asthma_dev_2,
                                 family="binomial",
                                 data=x)))[,'Pr(>|z|)']
)

fit <- as.data.frame(fit) 

# Wide to long transpose
library(data.table)
fit_long <-transpose(fit)

# get row and colnames in order
colnames(fit_long) <- rownames(fit)
rownames(fit_long) <- colnames(fit)

# rownames to column
fit_long$var_pos <- row.names(fit_long)   

fit_long$position <- as.numeric( str_replace_all(fit_long$var_pos, "V", "") )


fit_long %>%
  ggplot(aes(x=position, y=(-log10(binary))))+
  geom_point()

# V178 =    .001192050
# V517 =    .001119232 (match to nucleotide file?)
# .05/308 = .00016 (result in nucleotide)
# .05/59  = .00084

df %>%
  filter(var_pos == 'V178') %>% 
  group_by(binary, infection) %>%
  tally()

c










#\\\\\\\\\\\\\\\\\\\\\\\\\
####  Single SNP test ####
#\\\\\\\\\\\\\\\\\\\\\\\\\

test_df <- merged_testSNPs %>% 
  filter(var_pos == "V891") %>% 
  select(binary, infection, strain)

# Show chi-like table to expect outcome
test_tally <- test_df %>% 
  group_by(binary, infection, strain) %>% 
  tally()

test_df %>%
  ggplot(aes(x=infection, y=binary, color=strain)) + 
  geom_point(shape=1, position=position_jitter(width=.1,height=.2)) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) + facet_grid(strain ~ .)


test_df$infection <- as.factor(test_df$infection)
logit2 <- 
  glm(infection ~ binary,
      data=test_df,
      family="binomial")
summary(logit2)

plot(logit2)
summary(logit2) # display results
confint(logit2) # 95% CI for the coefficients
exp(coef(logit2)) # exponentiated coefficients
exp(confint(logit2)) # 95% CI for exponentiated coefficients
predict(logit2, type="response") # predicted values
residuals(logit2, type="deviance") # residuals












sig_freq <- 
  read.table(file="./G_sig_variant_frequency.csv",              header = T, 
             sep = ",", 
             stringsAsFactors = FALSE,
             colClasses = c("character"))


sig_freq$Freq <- as.numeric(sig_freq$Freq)

sig_freq %>%
  filter(Position=="p.126") %>%
  ggplot(aes(x=Infection, 
             y=(Freq), 
             fill=AA)) +
  geom_bar(stat = "identity")+
  scale_fill_manual(breaks = c("REF", "ALT"), 
                    values=c("darkred","darkblue"))

sig_freq %>%
  filter(Position=="p.221") %>%
  ggplot(aes(x=Infection, 
             y=(Freq), 
             fill=AA)) +
  geom_bar(stat = "identity")+
  scale_fill_manual(breaks = c("REF", "ALT"), 
                    values=c("black","darkgreen"))









