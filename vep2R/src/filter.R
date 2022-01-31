library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

# The tab output from VEP dbNSFP has the annotation data sep by "|".

file="../data/test_sample_vep.tsv"
# Import data ----
headers = read.csv(file, skip = 0, header = F, nrows = 814, as.is = T)
# tail(headers, 1)

df = read.csv(file, skip = 814, header = F, sep = "\t", )

names <- strsplit( (headers[814, c(1)]), "\\t") %>% as.data.frame()
names(names) <- c("v1")
names(df) <- t(names)

# equity plot ----
p1 <- df %>% 
  ggplot(aes(x = Date, y = ID, group=ID))+
  geom_line()+
  geom_point(aes(size= ask, color=Event)) +
  facet_grid(company ~.)

ggplotly(p1)
