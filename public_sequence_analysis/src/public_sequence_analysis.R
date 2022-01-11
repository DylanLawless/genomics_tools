# legends:
library(dplyr)
library(tidyr)
library(plotly)


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
tmp1 <- read.table("../data/HRSV_virus11250_attachment_glycoprotein_host9605_20180101_20220108.csv",
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

#tmp_2 <- tmp %>% 
#  group_by(V668, Geo_Location) %>%
#  count(name="count")

#tmp_3 <- merge(x=Geo_Location_total, y=tmp_2)
#tmp_3$Freq <- (tmp_3$count/tmp_3$total)

## Country freq count
#tmp_3 %>% 
#  ggplot(aes(y=V668, x=Freq, color=Geo_Location))+
#  geom_point()+
#  theme(legend.position="none")

## Country freq count
#Geo_Location_total %>%
#  ggplot(aes(y=Geo_Location, x=total, color=Geo_Location))+
#  geom_point()+
#  theme(legend.position="none")

#Geo_Location_total %>% 
#  arrange(desc(total))  

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

tmp_collection_date <- 
  separate(tmp_collection, Collection_Date, into = c("Year", "Month", "Day"))

tmp_collection_date$Year <- as.numeric(tmp_collection_date$Year)




tmp_collection_date_long <- tmp_collection_date %>% 
  select(1:V923) %>%
  tidyr::gather(var_pos, var, V1:V923, factor_key=TRUE)
# select(1:25) %>% 
# tidyr::gather(var_pos, var, 2:25, factor_key=TRUE)


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

























#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#### Test a corr measure for variants ####

library(stringr)
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

# Rename the seqID header to match the nex file - tip.Accession 
colnames(tmp_collection_date)[colnames(tmp_collection_date) == "Accession"] <- "Accession"

years <- tmp_collection_date %>%
  select(Accession, Year)

# Merge
dw_meta <- 
  merge(x = years, 
        y = dw, 
        by = "Accession", all.y = T)

row.names(dw_meta) <- dw_meta$Accession
dw_meta <- within(dw_meta, rm(Accession))


##############################################################












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
