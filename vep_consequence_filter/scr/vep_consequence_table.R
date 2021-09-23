# Import vep conseqence table
vep <-  read.table(file="../data/vep_consequence_table.tsv",
                   header = T, 
                   sep = "\t")

# Import data after converting vcf2tsv
table<-  read.table(file="../data/example_data_vcf2tsv.txt",
                    header = T, 
                    sep = "\t")

require(dplyr)
require(stringr )
# Select the desired consequence terms (IMPACT == "HIGH", "MODERATE", "LOW", "MODIFIER" )
# E.g.1
conq_high_moder <- vep %>% filter(IMPACT == "HIGH" | IMPACT == "MODERATE") %>% select(SO.term)
conq_high_moder <- as.character(conq_high_moder$SO.term)

# E.g.2
conq_high <- vep %>% filter(IMPACT == "HIGH") %>% select(SO.term)
conq_high <- as.character(conq_high$SO.term)

# E.g.3
conq_moder <- vep %>% filter(IMPACT == "MODERATE") %>% select(SO.term)
conq_moder <- as.character(conq_moder$SO.term)

# Filter data for query terms 
table_conq_high_moder <- table %>% 
  filter(str_detect(consequence, str_c(conq_high_moder, collapse="|")))

table_conq_moder <- table %>% 
  filter(str_detect(consequence, str_c(conq_moder, collapse="|")))

# Note that the following would only produce exact matches, ignoring multi-consequence annotation: table %>% filter(consequence %in% conq_high) 
