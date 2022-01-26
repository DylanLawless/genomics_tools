# legends:
library(dplyr)
library(tidyr)
library(plotly)
library(stringr)

msa.1 <-
  read.table(file="../data/nextalign_A_NC_038235.aligned.fasta.msa.tabbed.split",
             header = FALSE,
             sep = " ",
             stringsAsFactors = FALSE,
             colClasses = c("character"))

msa.2 <-
  read.table(file="../data/nextalign_A_NC_038235.aligned.fasta.msa.tabbed.labels",
             header = FALSE,
             sep = "\t",
             stringsAsFactors = FALSE)


msa.2 <-  separate(msa.2, V1,  sep = "\\.", into = c("Accession", "Accession2"))


# Rename label column
msa.1$Accession <- msa.2$Accession
rm(msa.2)


# move last column, label, to start
msa.1 <- msa.1 %>%
  select(Accession, everything())

# Add meta_data (location, date, etc.)
# list of viral samples and their ids
tmp1 <- read.table("../data/HRSV_virus11250_attachment_glycoprotein_host9605_19891231_20220101.csv",
                   sep = ",", header = T, stringsAsFactors = FALSE)

## The metadata contains every submission (all genes)
## We will just be using the ones for our protein sequence.

# //////////////////////////////////////
#### Merge metadata and cleanup ####
# //////////////////////////////////////


# add the metadata
msa_meta_data <- 
  merge(y=msa.1,
        x=tmp1,
        by = "Accession",
        all = F)

rm(msa.1)
rm(tmp1)

# Selected variant summary
#msa_meta_data %>% select(Accession, Geo_Location, 411, 686:694) %>% 
#  group_by(V668) %>%
#  count() %>%
#  arrange(desc(n))

# Will want to split the Geo_Location into just countries.
#msa_meta_data %>% select(Accession, Geo_Location, 411, 686:694) %>% 
#  group_by(Geo_Location) %>%
#  count()

tmp <- msa_meta_data %>% 
  # select(Accession, Geo_Location, Collection_Date, 411, 686:694) %>%
  separate( Geo_Location, 
                into = c("Geo_Location", "other"),
                sep = "[:]"
)

rm(msa_meta_data)
## Global total count
#tmp %>% 
#  group_by(V668, Geo_Location) %>%
#  count() %>% 
#  ungroup() %>%
#  ggplot(aes(y=V668, x=n, color=Geo_Location))+
#  geom_point()+
#  theme(legend.position="none")

#tmp %>% 
#  ggplot(aes(y=V668, fill=Geo_Location))+
#  geom_bar(position = "stack")+
#  theme(legend.position="none")


# Frequency per country
Geo_Location_total <- 
  tmp %>% 
  group_by(Geo_Location) %>%
  count(name="total")


# Time
# Global total count
tmp %>% 
  group_by(V668, Geo_Location) %>%
  count() %>% 
  ungroup() %>%
  ggplot(aes(y=V668, x=n, color=Geo_Location))+
  geom_point()+
  theme(legend.position="none")


tmp %>% 
  #group_by(V668, Geo_Location, Release_Date)  
  group_by(as.Date(Collection_Date)) %>%
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

rm(tmp)
tmp_collection_date <- 
  separate(tmp_collection, Collection_Date, into = c("Year", "Month", "Day"))

rm(tmp_collection)
tmp_collection_date$Year <- as.numeric(tmp_collection_date$Year)

# I think this will be main df ----
dw_meta <- tmp_collection_date %>% select(Accession, Year,  V1:ncol(.) ) # column v1 to last col
dw_meta_no_geno <- tmp_collection_date %>% select(Accession:GenBank_Title )
  
rm(tmp_collection_date)



# //////////////////////////////////////
#### Set REF and ALT ####
# //////////////////////////////////////

# df <- msa_meta_data %>% select(Accession, 1:1075)
df <- dw_meta %>% 
 # select(V1:ncol(.) ) %>%
  tidyr::gather(var_pos, var,V1:ncol(.) , factor_key=TRUE)

rm(dw_meta)

df$var[df$var == "-"] <- NA 

#colnames(df)[colnames(df) == "Accession"] <- "label"

var_count <- df %>% 
  na.omit() %>%
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
           y=x) # %>% 
  #select(var_pos, label, binary)

rm(df, x)
rm(df.listna)
rm(var_count, var_rank)
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#### Test a corr measure for variants ####

# convert each individual (row) and variant (col) to 0/1
d$var_pos <- as.numeric( str_replace_all(d$var_pos, "V", "") )

d$binary[d$binary == "REF"] <- "0"
d$binary[d$binary == "ALT"] <- "1"

d$binary <- as.numeric(d$binary)

dw <- spread(d, var_pos, binary)
rm(d)
#tmp_keep <- dw 
# remove col with all na
not_all_na <- function(x) any(!is.na(x))
dw2 <- dw %>% select(where(not_all_na))

# Year function ----
# remove any year with <2 sample (for cor later)
dw_meta <- # dw_meta %>%
  dw %>%
  group_by(Year) %>% 
  filter(n() > 1) %>%
  ungroup() %>%
  select(-Accession, -var)

# TEMP TEST
dw_meta <- dw %>% filter(Year > 2000)
  
# remove col with all na
not_all_na <- function(x) any(!is.na(x))
dw_meta <- dw_meta %>% select(where(not_all_na))

df.list.names <- dw_meta$Year %>% unique() %>% as.list()

df.list <- lapply(df.list.names, function(x) {
  dw <- dw_meta %>% 
    filter(Year %in% x ) %>%
    select(- Year)

  # remove col with all na
  not_all_na <- function(x) any(!is.na(x))
  dw <- dw %>% select(where(not_all_na))
  
  # remove row with all na
  dw <- dw %>% 
    select(- Accession, - var ) %>%
    select_if(~ !all(is.na(.)))

}
)

names(df.list) <- df.list.names %>% as.data.frame()
names(df.list)
#temp <- df.list[["2016"]] %>% as.data.frame()

#### Cor cutoff ####
library('caret')

# as function ----
names(df.list)

#a <- cor(temp2 , use="pairwise.complete.obs") %>% as.data.frame() #%>% as.numeric()
#a <- cor(df.list[["2016"]] , use="pairwise.complete.obs") %>% as.data.frame() #%>% as.numeric()
#a[is.na(a)] <- 0
#b<- a[, colSums(a != 0) > 0]
#df2 <- b[ rowSums(b)!=0, ] %>% as.matrix()
#pca_res <- prcomp(df2, scale = TRUE)

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
#  pca_res <- prcomp(df2, scale = FALSE)
  #  #fviz_eig(pca_res)
  
  
  # Variance explained
  var_explained <- pca_res$sdev^2/sum(pca_res$sdev^2)
  pca_res_x <- pca_res$x %>% as.data.frame()
  pca_res_x$Var <- as.numeric( row.names(pca_res_x))
  
  pca_res_x$Var_explained <- var_explained
 # pca_res_x$Group <- "Group 1"
  
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
  res[[n]]['Year'] = n
res_named = do.call(rbind, res)

res_plot <- res_named %>%
  ggplot(aes(x=Var, y=Year))+
  geom_tile(aes(fill=(Var_explained)))+
  scale_fill_gradient(low = "orange", 
                      high = "brown")+
  theme(legend.position="bottom", 
        panel.background = element_rect("#F7F7F7"))

  res_named %>%
 # filter(Year == 2018) %>% 
  ggplot(aes(y=(Var_explained*10), x=Var))+
  geom_line(aes(color=Year, alpha = 0.4 ))+
  theme(legend.position="bottom", 
        panel.background = element_rect("#F7F7F7"))

  res_named %>%
    # filter(Year == 2018) %>% 
    ggplot(aes(y=(Var_explained*10), x=Var))+
    geom_point(aes(color=Year, alpha = 0.4 ))+
    theme(legend.position="bottom", 
          panel.background = element_rect("#F7F7F7"))

# There is something wrong since var explained is high then drops, every year. 
# It may be from the steps where we remove col or rows that are all NAs.
# It may also be a problem with the alignment method: alignments starts bad but improves downstream?
# Try with MAFFT.


res_plot
library(plotly)
ggplotly(res_plot)




x


























tmp_collection_date_long <- tmp_collection_date %>% 
  select(1:V923) %>%
  tidyr::gather(var_pos, var, V1:V923, factor_key=TRUE)
# select(1:25) %>% 
# tidyr::gather(var_pos, var, 2:25, factor_key=TRUE)

library(stringr)
# Make a column with the gene position as a numeric for easier filtering.
tmp_collection_date_long$position <- as.numeric( str_replace_all(tmp_collection_date_long$var_pos, "V", "") )


head(tmp_collection_date_long)
# N per year
#tmp_collection_date %>% 
#  group_by(Year) %>%
#  count() %>%
#  ggplot(aes(x=Year, y=n))+
#  geom_point()

tmp_var_per_year <- 
  tmp_collection_date_long %>% 
  group_by(Year, position, var) %>%
  count(name = "n_var_per_year") 

tmp_total_per_year <- 
  tmp_collection_date_long %>% 
  group_by(Year, var_pos)%>%
  count(name = "n_total_per_year") 


tmp_per_year <- 
  merge(x=tmp_total_per_year,
        y=tmp_var_per_year)

#rm (tmp_total_per_year, tmp_var_per_year)

tmp_per_year$Freq_per_year <- 
  (tmp_per_year$n_var_per_year / tmp_per_year$n_total_per_year)


# main for shiny ----
tmp_per_year %>% 
  filter(position >= 664) %>%
  filter(position <= 668) %>%
  group_by(var, position) %>%
  ggplot(aes(x=Year, y=Freq_per_year, color=var))+
  geom_point()+
  geom_line(aes(group=interaction(var, position)))

tmp_per_year %>% 
  filter(position >= 668) %>%
  group_by(Year, position) %>%
  ggplot(aes(x=position, y=Freq_per_year, color=var))+
  geom_point()#+
  #geom_line(aes(group=interaction(Year, position)))

tmp_per_year %>% 
  filter(position >= 668) %>%
  ggplot(aes(x=Year, y=Freq_per_year, color=V668))+
  geom_point()+
  geom_smooth(method = lm, alpha = 0.1, 
              show.legend = FALSE,  linetype= "dashed") +
geom_line(aes(group = V668))


#tmp_per_year %>%
#  filter(Year==2006)

#tmp_collection_date %>% filter(Year > 2006) %>%
#  group_by(Authors, Year, Geo_Location) %>%
#  count()

tmp_collection_date %>%
  group_by(Year) %>%
  count()

# 
# l





# //////////////////////////////////////
#### Set REF and ALT ####
# //////////////////////////////////////



# df <- msa_meta_data %>% select(Accession, 1:1075)
df <- tmp_collection_date %>% 
  select(1:V923) %>%
  tidyr::gather(var_pos, var, V1:V923, factor_key=TRUE)
# select(1:25) %>% 
# tidyr::gather(var_pos, var, 2:25, factor_key=TRUE)

head(df)
tail(df)

#colnames(df)[colnames(df) == "Accession"] <- "Accession"

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

df2 <- merge(x=df,
           y=x) # %>% 
#  select(var_pos, Accession, binary)



df2 %>% 
  filter(var_pos == "V668") %>%
  ggplot(aes(x=Year, y=Freq_per_year, color=V668))+
  geom_point()+
  geom_line(aes(group = V668))


x

























# #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# #### Test a corr measure for variants ####
# 
# library(stringr)
# # convert each individual (row) and variant (col) to 0/1
# d$var_pos <- as.numeric( str_replace_all(d$var_pos, "V", "") )
# 
# d$binary[d$binary == "REF"] <- "0"
# d$binary[d$binary == "ALT"] <- "1"
# 
# d$binary <- as.numeric(d$binary)
# 
# dw <- spread(d, var_pos, binary)
# tmp_keep <- dw 
# 
# # meta data
# # Collection date
# tmp <- tmp1
# rm(tmp1)
# # format months as categorical for plink covariates
# library(lubridate)
# # df$cfidob <- as.numeric(ymd(df$cfidob))
# #tmp$Year <- format(as.Date(tmp$Release_Date), "%Y")
# #tmp$Month <- format(as.Date(tmp$Release_Date), "%b")
# #tmp$Day <- format(as.Date(tmp$Release_Date), "%d")
# #tmp$DayOfYear <- as.numeric(format(as.Date(tmp$Release_Date), "%j"))
# 
# # Filter rows with Collection_Date
# tmp_collection <- tmp %>% 
#   mutate_all(na_if,"") %>% 
#   drop_na(Collection_Date)
# 
# tmp_collection_date <- 
#   separate(tmp_collection, Collection_Date, into = c("Year", "Month", "Day"))
# 
# tmp_collection_date$Year <- as.numeric(tmp_collection_date$Year)
# 
# tmp_collection_date %>% 
#   group_by(Year) %>%
#   count() %>%
#   ggplot(aes(x=Year, y=n))+
#   geom_point()
# 
# tmp_per_year %>%
#   filter(Year==2006)
# 
# # Rename the seqID header to match the nex file - tip.Accession 
# colnames(tmp_collection_date)[colnames(tmp_collection_date) == "Accession"] <- "Accession"
# 
# years <- tmp_collection_date %>%
#   select(Accession, Year)
# 
# # Merge
# dw_meta <- 
#   merge(x = years, 
#         y = dw, 
#         by = "Accession", all.y = T)
# 
# row.names(dw_meta) <- dw_meta$Accession
# dw_meta <- within(dw_meta, rm(Accession))
# 
# 
# ##############################################################
# 
# 
# 
# 








# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# x
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# df <- read.table(file = "../data/XGYK6RVT114-Alignment.tsv", sep = '\t', header = TRUE)
# df_meta <- read.csv("../data/sequences.csv",stringsAsFactors = FALSE, header = TRUE) 
# df <- df %>% unique()
# df_meta <- df_meta %>% unique()
# 
# colnames(df)[colnames(df) == 'subject.acc.ver'] <- 'Accession'
# 
# df <-  separate(df, Accession,  sep = "\\.", into = c("Accession", "Accession2"))
# 
# df <- merge(df, df_meta, all.x=TRUE)
# 
# names(df)
# colnames(df)[colnames(df) == 'X..identity'] <- 'PC.identity'
# colnames(df)[colnames(df) == 'q..start'] <- 'q.start'
# colnames(df)[colnames(df) == 'q..end'] <- 'q.end'
# colnames(df)[colnames(df) == 's..start'] <- 's.start'
# colnames(df)[colnames(df) == 's..end'] <- 's.end'
# 
# subset <- df 
# 
# # compare "any gene" to the total distribution ----
# subset %>%
#   ggplot(aes(x=(`bit.score`))) +
#   geom_histogram(bins = 10)
# 
# 
# 
# 
# 
# library(lubridate)
# # df$cfidob <- as.numeric(ymd(df$cfidob))
# #tmp$Year <- format(as.Date(tmp$Release_Date), "%Y")
# #tmp$Month <- format(as.Date(tmp$Release_Date), "%b")
# #tmp$Day <- format(as.Date(tmp$Release_Date), "%d")
# #tmp$DayOfYear <- as.numeric(format(as.Date(tmp$Release_Date), "%j"))
# 
# tmp <- subset
# # Filter rows with Collection_Date
# tmp_collection <- tmp %>% 
#   #mutate_all(na_if,"") %>% 
#   drop_na(Collection_Date)
# 
# tmp_collection_date <- 
#   separate(tmp_collection, Collection_Date, into = c("Year", "Month", "Day"))
# 
# tmp_collection_date$Year <- as.numeric(tmp_collection_date$Year)
# 
# tmp_collection_date %>% 
#  group_by(Year) %>%
#   ggplot(aes(x=Year))+
#   geom_bar()
# 
# d <- tmp_collection_date
# 
# ggplot(d,aes(x = PC.identity)) + 
#   facet_wrap(Year~Geo_Location ,scales = "free") + 
#   geom_histogram(bins = 10) #+
# 
# d %>% ggplot(aes(x = Year,
#                  y = PC.identity,
#                  color=Geo_Location,
#                  alpha = 0.5)) + 
#   geom_point(position=position_jitter(width=.1,height=2)) +
#   geom_line( aes(group = Geo_Location), position=position_jitter(width=.1,height=1) ) +
#   ylim(0,100)
# 
# 
# 
# 
# 
# d <- d %>% unique()
# d <- d %>% filter(alignment.length >=10 )
# 
# # gene_var <- "CHST14"
# #gene_var_d <- d %>% filter(Gene == gene_var) %>% group_by(key)
# # gene_var_d
# 
# d2 <- d %>% 
#   select(Accession, Geo_Location, PC.identity, mismatches, alignment.length, s.start, s.end, bit.score, Year) %>%
#   #  tidyr::gather(key, value)
#   tidyr::gather(key = key, value = value, -Accession, -Geo_Location, -Year, -s.start, -s.end)
# 
# ggplot(d,aes(x = PC.identity)) + 
#   facet_wrap(~Geo_Location ,scales = "free") + 
#   geom_histogram() #+
#   #geom_vline(data  = gene_var_d, aes(xintercept = value, colour = key))
# 
# ggplot(d2,aes(x = (value) )) + 
#   facet_wrap(~key,scales = "free") + 
#   geom_histogram(bins = 10)
# 
# class(d2$Year)
# d2 <- as_tibble(d2)
# plot <- d2 %>% ggplot(aes(x = Year,
#                  y = value,
#                  color=Geo_Location,
#                  alpha = 0.5,
#                  group = Accession)) + 
#   geom_point(position=position_jitter(width=.1,height=2)) +
#   geom_line( aes(group = Geo_Location), position=position_jitter(width=.1,height=1) ) +
#   ylim(0,100)+ 
#   facet_wrap(~key, scales = "free") + labs(
#   title = "Forward primer match TGCCTATGGTTCAGGGCAAG" )
# 
# library(plotly)
# ggplotly(plot) 
# 
# 
# x
# 
# 
# 
# 
# 
# 
# 
