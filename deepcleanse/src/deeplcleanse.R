library(dplyr)
library(tidyr)
require(stringr)

df <- 
  read.table(file="../data/rsv.msa.tabbed.split",
             header = FALSE, 
             sep = " ", 
             stringsAsFactors = FALSE,
             colClasses = c("character"))

df$labels <- rownames(df)

# Wide to long
df_long <- df %>% tidyr::gather(var_pos, var, V1:V257, factor_key=TRUE)

df_long$var_pos <- as.numeric( str_replace_all(df_long$var_pos, "V", "") )

# Make a column with the gene position as a numeric for easier filtering.
# df_long$position <- as.numeric( str_replace_all(df_long$var_pos, "V", "") )


# //////////////////////////////////////
#### Variant frequency per position #####
# //////////////////////////////////////

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
# Name the less frequent residue as ALT
rank$binary <- rank$rank
rank$binary[rank$rank == "1"] <- "REF"
rank$binary[rank$rank > "1"] <- "ALT"

rank$value <- rank$rank
rank$value[rank$rank == "1"] <- "0"
rank$value[rank$rank > "1"] <- "1"
rank$value <- as.numeric(rank$value)

# Add this new annotation back onto the dataset
df_long <- merge(df_long, (rank %>% select(var_pos, var, value))) %>% 
  select(var_pos, labels, value)

# set lables as number so that they are in order instead of alphabetic
df_long$labels <- as.numeric(df_long$labels)

# convert long to wide format
genotypes <- spread(df_long, var_pos, value)
rownames(genotypes) <- genotypes$labels
genotypes <- genotypes %>% select(-labels)

rm(df, count, rank)

require(ggplot2)
# Plot the variants
df_long %>%
  ggplot(aes(y=labels, x=var_pos))+
  geom_tile(aes( fill=as.factor(value) ), color="black")

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


# fviz_pca_ind(pca_res, col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE     # Avoid text overlapping
# )

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





pca_res_x$Var_explained <- 
  var_explained

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




# run assoc test on each SNP.
# compare cases to control.


# test 1 SNP
df_pos1 <- 
  df_long %>% 
  filter(var_pos == 10)

df_pos1_case <- df_pos1[1:15, 1:3]
df_pos1_control <- df_pos1[16:30, 1:3]

df_pos1_case$status <- "case"
df_pos1_control$status <- "control"

df_pos_status <- rbind(df_pos1_case, df_pos1_control)

df_pos_status
df_pos_status$status <- as.factor(df_pos_status$status)

logistic.model <- glm(status ~ value, family = "binomial", data = df_pos_status)
summary(logistic.model)

# Odds ratio and CI
exp(cbind(coef(logistic.model), confint(logistic.model))) 





# test whole gene

status <- df_pos_status %>% select(status, labels)

df_long_status <- merge(status, y=df_long, by="labels" )

df_long_status$status <- as.factor(df_long_status$status)

logistic.model <- glm(status ~ value, family = "binomial", data = df_long_status)
summary(logistic.model)

# Odds ratio and CI
exp(cbind(coef(logistic.model), confint(logistic.model))) 

# Now accounting for individual SNPs

logistic.model <- glm(status ~ value + var_pos, family = "binomial", data = df_long_status)
summary(logistic.model)

# compare result
model1 = 0.000502 ***
model2 = 0.000427 ***

# example where sig assoc may be due to a batch effect
# case  5
# control 10

# case A 0
# control A 0

# case B 5
# control B 10


remove var if < 0.05 (5%) population has it

# CMC test threshold say if a variant is rare, then merge all those variants into one 
# If variant is common then test this snp alone.

# if only a single person has each SNP, then CMC detect this better.
# SKAT can better detect complex situations. 
# e.g. one SNP shared in multiple people, or multiple SNPs from single person. 
# In CMC a sample is labeled as "1" if they have ANY SNP and "0" if none. 
# CMC is like a simple GWAS with Fisher test, but GWAS can also be run with a linear/logistic model to account for covariates. 

# CMC is based on counts: affected or not.
# SKAT is based on allele frequency: SNP1 is freq 0.01, and SNP2 is 0.3 then each will have a different "weight" in the model. 
logistic.model <- glm(status ~ value + var_pos*weight(freq), family = "binomial", data = df_long_status)
summary(logistic.model)


fisher.test(status ~ value)





hc = findCorrelation(df2, cutoff=0.8) # put any value as a "cutoff" 
hc = sort(hc)
reduced_Data = df1[,-c(hc)]
print (reduced_Data)

dw.cor.Rsq.reduced <- ( cor(reduced_Data) )^2

corrplot(dw.cor.Rsq.reduced, method="color",  cl.lim = c(0, 1), type = "upper") 
# ./update_pca_figures/nucleo_corr_plot_Rsq_0.8_split.pdf 10x10


####  eigen values  ####
#   Reduced LD variants
e1 <- eigen(df2)
e2 <- eigen(dw.cor.Rsq.reduced)

sqrt(e2$values)

#### Proportion of variance ####
e2_prop <- 
  e2$values/sum(e2$values)
e2_prop <- as.data.frame(e2_prop)

e2_prop$PC <- row.names(e2_prop)
e2_prop %>%
  ggplot(aes(y=as.numeric( e2_prop) ,
             x=as.numeric(PC) )) +
  geom_bar(stat="identity")
# ./update_pca_figures/eigenval_after_prune.pdf 4x7

####  eigen values  ####
#   Full gene variants
sqrt(e1$values)

#Proportion of variance:
e1_prop <- 
  e1$values/sum(e1$values)
e1_prop <- as.data.frame(e1_prop)

e1_prop$PC <- row.names(e1_prop)
e1_prop %>%
  ggplot(aes(y=as.numeric( e1_prop) ,
             x=as.numeric(PC) )) +
  geom_bar(stat="identity")
# ./update_pca_figures/eigenval_before_prune.pdf 4x7

# The same is done in a single command:
princomp(df2,cor=TRUE)
princomp(covmat=cor(df2))












findCorrelation(df, cutoff=0.8)

library(corrplot)
dw.cor<-cor(df) 
dw.cor.Rsq <- (dw.cor)^2
corrplot(dw.cor.Rsq, method="color",  cl.lim = c(0, 1)) 
# ./update_pca_figures/nucleo_corr_plot_Rsq.pdf 40x40





#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#### Test a corr measure for variants ####

# convert each individual (row) and variant (col) to 0/1
d <- test_set_RefAlt %>% 
  select(var_pos, label, binary)
d$var_pos <- as.numeric( str_replace_all(d$var_pos, "V", "") )

d$binary[d$binary == "REF"] <- "0"
d$binary[d$binary == "ALT"] <- "1"

d$binary <- as.numeric(d$binary)

dw <- spread(d, var_pos, binary)
tmp_keep <- dw 
row.names(dw) <- dw$label
dw <- within(dw, rm(label))

# https://www.datanovia.com/en/blog/easy-correlation-matrix-analysis-in-r-using-corrr-package/

library(corrplot)
dw.cor<-cor(dw) 
dw.cor.Rsq <- (dw.cor)^2
corrplot(dw.cor.Rsq, method="color",  cl.lim = c(0, 1)) 
# ./update_pca_figures/nucleo_corr_plot_Rsq.pdf 40x40

#### Cor cutoff ####
library('caret')
df1 <- dw 
df2 = cor(df1)
hc = findCorrelation(df2, cutoff=0.8) # put any value as a "cutoff" 
hc = sort(hc)
reduced_Data = df1[,-c(hc)]
print (reduced_Data)

dw.cor.Rsq.reduced <- ( cor(reduced_Data) )^2

corrplot(dw.cor.Rsq.reduced, method="color",  cl.lim = c(0, 1), type = "upper") 
# ./update_pca_figures/nucleo_corr_plot_Rsq_0.8_split.pdf 10x10


####  eigen values  ####
#   Reduced LD variants
e1 <- eigen(df2)
e2 <- eigen(dw.cor.Rsq.reduced)

sqrt(e2$values)

#### Proportion of variance ####
e2_prop <- 
  e2$values/sum(e2$values)
e2_prop <- as.data.frame(e2_prop)

e2_prop$PC <- row.names(e2_prop)
e2_prop %>%
  ggplot(aes(y=as.numeric( e2_prop) ,
             x=as.numeric(PC) )) +
  geom_bar(stat="identity")
# ./update_pca_figures/eigenval_after_prune.pdf 4x7

####  eigen values  ####
#   Full gene variants
sqrt(e1$values)

#Proportion of variance:
e1_prop <- 
  e1$values/sum(e1$values)
e1_prop <- as.data.frame(e1_prop)

e1_prop$PC <- row.names(e1_prop)
e1_prop %>%
  ggplot(aes(y=as.numeric( e1_prop) ,
             x=as.numeric(PC) )) +
  geom_bar(stat="identity")
# ./update_pca_figures/eigenval_before_prune.pdf 4x7

# The same is done in a single command:
princomp(df2,cor=TRUE)
princomp(covmat=cor(df2))


# Use library factoextra
library(factoextra)
pca_res <- prcomp(df2, scale = TRUE)
fviz_eig(pca_res)
get_eig(pca_res) 
# ./update_pca_figures/eigenval_scree_before_prune.pdf 4x7


# fviz_pca_ind(pca_res, col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE     # Avoid text overlapping
# )

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


pca_res_x$Var_explained <- 
  var_explained

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

#### Variance explained var of interest ####
pca_res_long %>%
  filter(Var >=221) %>%
  ggplot(aes(x=Var, y=PC))+
  geom_tile(aes(fill=(measurement)))+
  scale_fill_gradient(low = "orange", 
                      high = "brown")+
  theme(legend.position="bottom", 
        panel.background = element_rect("#F7F7F7"))

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









