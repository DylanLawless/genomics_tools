library(ggplot2)
library(dplyr)
library(gridExtra)

#/////////////////////
# Eigen vector plot
#/////////////////////

# import data
vec <- read.table("./CASE.QC.impute2_plink2.allfiles.king_mind1_maf01_geno05.pca.eigenvec", header=F) 
phenotypes <- read.csv("./phenotypes/sepsis_gwas_v2.csv", header=T, sep = "," )
pheno <- phenotypes %>% select(V1, V2, age.days, study.site, gender, ethnicity)
vec_pheno <- merge(x=vec, y=pheno, by="V1", "V2", all=TRUE ) 

# test plot
vec_pheno %>% 
  ggplot(aes(x=V3, y=V4))+ 
  geom_point(aes( fill = study.site ), shape=21, alpha = 0.8)+ 
  labs(x = "PC1", y = "PC2")

vec_pheno %>% 
  ggplot(aes(x=V3, y=V4))+ 
  geom_point(aes( fill = ethnicity ), shape=21, alpha = 0.8)+ 
  labs(x = "PC1", y = "PC2")

# Set plot style
theme_set <- theme(text = element_text(face="bold"), legend.position="none", panel.background = element_rect("#F7F7F7"))
geom_set <- geom_point(aes(fill = ethnicity), shape=21, alpha = 0.5)

# replace with a loop function here
p1 <- vec_pheno %>% ggplot(aes(x=V3, y=V4))+geom_set + labs(x = "PC1", y = "PC2")+ theme_set
p2 <- vec_pheno %>% ggplot(aes(x=V4, y=V5))+geom_set + labs(x = "PC2", y = "PC3")+ theme_set
p3 <- vec_pheno %>% ggplot(aes(x=V5, y=V6))+geom_set + labs(x = "PC3", y = "PC4")+ theme_set
p4 <- vec_pheno %>% ggplot(aes(x=V6, y=V7))+geom_set + labs(x = "PC4", y = "PC5")+ theme_set
p5 <- vec_pheno %>% ggplot(aes(x=V7, y=V8))+geom_set + labs(x = "PC5", y = "PC6")+ theme_set
p6 <- vec_pheno %>% ggplot(aes(x=V8, y=V9))+geom_set + labs(x = "PC6", y = "PC7")+ theme_set
p7 <- vec_pheno %>% ggplot(aes(x=V9, y=V10))+geom_set + labs(x = "PC7", y = "PC8")+ theme_set
p8 <- vec_pheno %>% ggplot(aes(x=V10, y=V11))+geom_set + labs(x = "PC8", y = "PC9")+ theme_set
p9 <- vec_pheno %>% ggplot(aes(x=V11, y=V12))+geom_set + labs(x = "PC9", y = "PC10")+ theme_set
p10 <- vec_pheno %>% ggplot(aes(x=V12, y=V13))+geom_set + labs(x = "PC10", y = "PC11")+ theme_set
p11 <- vec_pheno %>% ggplot(aes(x=V13, y=V14))+geom_set + labs(x = "PC11", y = "PC12")+ theme_set
p12 <- vec_pheno %>% ggplot(aes(x=V14, y=V15))+geom_set + labs(x = "PC12", y = "PC13")+ theme_set
p13 <- vec_pheno %>% ggplot(aes(x=V15, y=V16))+geom_set + labs(x = "PC13", y = "PC14")+ theme_set
p14 <- vec_pheno %>% ggplot(aes(x=V16, y=V17))+geom_set + labs(x = "PC14", y = "PC15")+ theme_set
p15 <- vec_pheno %>% ggplot(aes(x=V17, y=V18))+geom_set + labs(x = "PC15", y = "PC16")+ theme_set
p16 <- vec_pheno %>% ggplot(aes(x=V18, y=V19))+geom_set + labs(x = "PC16", y = "PC17")+ theme_set
p17 <- vec_pheno %>% ggplot(aes(x=V19, y=V20))+geom_set + labs(x = "PC17", y = "PC18")+ theme(text = element_text(face="bold"), legend.position="right", panel.background = element_rect("#F7F7F7"))

grid.arrange (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16, p17, ncol=4, top = "Cohort Principal component analysis", left = "")
# pca_color.pdf 15x12

#/////////////////////
# Eigen value plot
#/////////////////////
val <- read.table("./CASE.QC.impute2_plink2.allfiles.king_mind1_maf01_geno05.pca.eigenval", header=F) 

# Give row name column to plot
val <- val %>% mutate(V2 = rownames(val))

# Plot
val %>% 
  ggplot(aes(x = as.numeric(V2), y = V1))+  
  geom_point()

