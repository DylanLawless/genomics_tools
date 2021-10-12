require(dplyr)
require(tidyr)
require(stringr)
require(ggplot2)

# //////////////////////////////////////
#### Import multiple sequence alignment of amino acids ####
# //////////////////////////////////////
df <- 
  read.table(file="../data/amino.msa.tabbed.split",
             header = FALSE, 
             sep = " ", 
             stringsAsFactors = FALSE,
             colClasses = c("character"))

df$sample_ID <- rownames(df)

# Wide to long
df_long <- df %>% tidyr::gather(var_pos, var, V1:V257, factor_key=TRUE)

# Remove "V" for variant position label
df_long$var_pos <- as.numeric( str_replace_all(df_long$var_pos, "V", "") )

# //////////////////////////////////////
#### Variant frequency per position #####
# //////////////////////////////////////
# We need to define REF and ALT for each amino acid position

# Total count position / variant per position
count <-
  df_long %>% 
  group_by(var_pos, var) %>% 
  tally(name = "var_count")

# //////////////////////////////////////
#### Set REF and ALT ####
# //////////////////////////////////////

# For each position count and rank frequency of residue
rank <- count %>% 
  group_by(var_pos) %>%
  mutate(rank = rank(desc(var_count))) %>%
  arrange(rank)

# Name the most frequent residue as REF.
# Name the least frequent residue as ALT
# Add these labels as both strings and allele (0/1) so that it is easy to understand
rank$binary <- rank$rank
rank$binary[rank$rank == "1"] <- "REF"
rank$binary[rank$rank > "1"] <- "ALT"

rank$allele <- rank$rank
rank$allele[rank$rank == "1"] <- "0"
rank$allele[rank$rank > "1"] <- "1"
rank$allele <- as.numeric(rank$allele)

head(rank)

# Add this new annotation back onto the dataset
df_long <- merge(df_long, (rank %>% select(var_pos, var, allele))) %>% 
  select(var_pos, sample_ID, allele)

# set sample_ID as number so that they are in order instead of alphabetic characters
df_long$sample_ID <- as.numeric(df_long$sample_ID)

head(df_long)

# //////////////////////////////////////
#### Return to genotype matrix style layout ####
# //////////////////////////////////////

# convert long to wide format - matches the origin input data
genotypes <- spread(df_long, var_pos, allele)
rownames(genotypes) <- genotypes$sample_ID
genotypes <- genotypes %>% select(-sample_ID)

# clean up
rm(df, count, rank)

# Plot the variant genotypes visually
df_long %>%
  ggplot(aes(y=sample_ID, x=var_pos))+
  geom_tile(aes( fill=as.factor(allele) ), color="black")

# //////////////////////////////////////
##### Run assoc test on each SNP #### 
# compare cases to control
# //////////////////////////////////////

# Test 1 SNP
# SNP 10 has variants in our dataset, so try this one
df_pos1 <- 
  df_long %>% 
  filter(var_pos == 10)

# Create labels for 15 cases and 15 controls
df_pos1_case <- df_pos1 %>% filter(sample_ID <= 15)
df_pos1_control <- df_pos1 %>% filter(sample_ID >= 16)

df_pos1_case$status <- "1" # case = 1
df_pos1_control$status <- "0" # control = 0

# bind into one dataset again
df_pos_status <- rbind(df_pos1_case, df_pos1_control) 

# clean up
rm(df_pos1_case, df_pos1_control)

# Sort by sample ID for aesthetics
df_pos_status <- df_pos_status[order(df_pos_status$sample_ID),]

# make the binary response "status" a factor for logistic regression
df_pos_status$status <- as.factor(df_pos_status$status)

# for examle, it will be interpreted as:
as.numeric(df_pos_status$status)

df_pos_status$allele

# logistic regression model may be as follows:
# glm( A ~ B + C + D,  family=’binomial’ )
# Binary response:  A disease status: case/control.
# Predictors:       B genotype (allele REF/ALT amino acid).
#                   C genetic PCA: PC1,PC2, etc.
#                   D hospital: site A/B/C.



logistic.model <- glm(status ~ allele, family = "binomial", data = df_pos_status)
summary(logistic.model)

# Odds ratio and CI
exp(cbind(coef(logistic.model), confint(logistic.model))) 
# Having a variant is assoc with being case (disease) 
# P-val = 0.004, OR = 27.99 (95% CI 3.94-586.83)

# Note that if you had zero carriers in controls, like with SNP11, then logistic model would not be fit appropriate and P-value= ~1. Try repeat with filter(var_pos == 11) to see the result). 

# //////////////////////////////////////
##### Run assoc test on whole gene combined #### 
# compare cases to control
# //////////////////////////////////////
# Add the "status" label (case/control) to the whole dataset
status <- df_pos_status %>% select(status, sample_ID)
df_long_status <- merge(status, y=df_long, by="sample_ID" )

# run logistic regression
df_long_status$status <- as.factor(df_long_status$status)

logistic.model <- glm(status ~ allele, family = "binomial", data = df_long_status)
summary(logistic.model)

# Odds ratio and CI
exp(cbind(coef(logistic.model), confint(logistic.model))) 
# The combined effect of all variants is not assoc with being case (disease) 
# P-val = 0.19, OR = 0.73 (95% CI 0.45-1.17)

# //////////////////////////////////////
##### Run assoc test on whole gene accounting for individual SNPs #### 
# compare cases to control
# //////////////////////////////////////
logistic.model <- glm(status ~ allele + var_pos, family = "binomial", data = df_long_status)
summary(logistic.model)
exp(cbind(coef(logistic.model), confint(logistic.model))) 
# The combined effect of all variants is not assoc with being case (disease) even when we account for them idividually; SNP10 alone was not strong enough to see the effect combined with all variants. There is no gene burden, just a single SNP association. 
# P-val = 0.19, OR = 0.73 (95% CI 0.45-1.17)

# compare result, P-values
# model1 = 0.004 # SNP 10 assoc with disease
# model2 = 0.19 # all SNPs combined, no assoc
# model3 = 0.19 # all SNPs combined, accounting for individual effects


# //////////////////////////////////////
#### Run a count test using Fisher test or Chi Sqr ####
# Similar to CMC method
# //////////////////////////////////////

# Select position 10 again
fisher_df <- df_long_status %>%
  filter(var_pos == 10)

# Count case/control REF/ALT
fisher_df_tally <- 
  fisher_df %>%
  group_by(status, allele) %>%
  tally()

# Put that in a 2x2 format
fisher_mat1 <- matrix(fisher_df_tally$n ,nrow=2,ncol=2,byrow=F)
fisher_mat1

# Run the test
chisq.test(fisher_mat1)
fisher.test(fisher_mat1)
# You may like testing on https://www.socscistatistics.com/tests/

# What happens if we swap the order of groups/category?
# The results are unaffected
fisher_mat2 <- t(fisher_mat1)

fisher_mat1
fisher_mat2
fisher.test(fisher_mat1)
fisher.test(fisher_mat2)

### END ####



# //////////////////////////////////////
#### Think about toy datasets ####
# //////////////////////////////////////
# examples where sig assoc may be due to a batch effect due hospital A and B sequencing differently. For 15 cases and 15 controls, sequenced in hospital A or B, how many have SNPs:

# case  10
# control 5

# case A 0
# control A 0

# case B 10
# control B 5

# There may be a "batch effect" from hospital B since variants only found there.
# Although, in our real dataset we do not have hospital info to worry about. 


# //////////////////////////////////////
#### Other notes ####
# //////////////////////////////////////
# CMC test threshold say if a variant is rare, then merge all those variants into one 
# If variant is common then test this snp alone.

# if only a single person has each SNP, then CMC detect this better.
# SKAT can better detect complex situations. 
# e.g. one SNP shared in multiple people, or multiple SNPs from single person. 
# In CMC a sample is labeled as "1" if they have ANY SNP and "0" if none. 
# CMC is like a simple GWAS with Fisher test, but GWAS can also be run with a linear/logistic model to account for covariates. 

# CMC is based on counts: affected or not.
# SKAT is based on allele frequency: SNP1 is freq 0.01, and SNP2 is 0.3 then each will have a different "weight" in the model. 

# In the glm, the "var_pos" might also have a allele frequency weight applied for each SNP.
# The CMC can be replicated in R code by counting the number of affected case/control and doing a fisher.test: affect versus allele.



# //////////////////////////////////////
#### Other example code that could be useful sometime later ####
# //////////////////////////////////////

# set the matrix orientation : PCA for variant or for sample
genotypes <- t(genotypes)

# remove columns with no variation
genotypes_var <- genotypes[, colSums(genotypes != 0) > 0]
cor(genotypes_var) 

#### Cor cutoff ####
library('caret')
df1 <- genotypes_var
df2 = cor(df1)

# Use library factoextra
library(factoextra)
pca_res <- prcomp(df2, scale = TRUE)
fviz_eig(pca_res)
get_eig(pca_res) 
# ./update_pca_figures/eigenval_scree_before_prune.pdf 4x7

fviz_pca_ind(pca_res, col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE     # Avoid text overlapping
)

# Quick look
pca_res$x[1:5,1:3]

# Variance explained
var_explained <- pca_res$sdev^2/sum(pca_res$sdev^2)
var_explained[1:5]
pca_res_x <- pca_res$x %>% as.data.frame()
pca_res_x$Var <- as.numeric( row.names(pca_res_x))

pca_res_x %>% 
  ggplot(aes(x=PC1,y=PC2, 
             label=Var)) +
  scale_fill_gradient(low = "steelblue1 ", high = "navy")+
  geom_label(aes(fill = Var), colour = "white", size = 2, alpha=0.7, 
             position=position_jitter(width=1,height=1))+
  theme(legend.position="bottom", 
        panel.background = element_rect("#F7F7F7"))+
  labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
       y=paste0("PC2: ",round(var_explained[2]*100,1),"%"))

# ./update_pca_figures/PCA_before_prune.pdf 4x7


pca_res_x$Var_explained <- var_explained

pca_res_x$Group <- "Group 1"

pca_res_x <- pca_res_x %>% as.data.frame() %>% select(PC1:PC5, Var, Var_explained, Group )

pca_res_long <- gather(pca_res_x, PC, measurement, PC1:PC5, factor_key=TRUE)

pca_res_long %>%
  ggplot(aes(x=Var, y=PC))+
  geom_tile(aes(fill=(measurement)))+
  scale_fill_gradient(low = "orange", 
                      high = "brown")+
  theme(legend.position="bottom", 
        panel.background = element_rect("#F7F7F7"))
# ./update_pca_figures/PC_values.pdf 4x7

