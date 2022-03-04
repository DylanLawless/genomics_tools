# Make binary matrix ----
# Row = Individuals, Col = Position
set.seed(3)
mm <- matrix(0, 10, 5)
d <- apply(mm, c(1, 2), function(x) sample(c(0, 1), 1))
d

# View the matrix ----
library(tidyr)
library(stringr)
df <- as.data.frame(d)
data_long <- reshape2::melt(d)
colnames(data_long)[colnames(data_long) == "Var1"] <- "Individual"
colnames(data_long)[colnames(data_long) == "Var2"] <- "Position"

data_long$value <- as.factor(data_long$value)
data_long$Individual <- as.factor(data_long$Individual)
g <- ggplot(data_long, aes(Individual, Position))
p1 <- g + geom_tile(aes(fill=value),  colour = "black")
# p1 

# PCA of Individual ----
pca_res <- prcomp(d, scale = TRUE)
var_explained <- pca_res$sdev^2/sum(pca_res$sdev^2)

library(dplyr)
pca_res_x <- pca_res$x %>% as.data.frame()
pca_res_x$Var <- as.numeric( row.names(pca_res_x))

p2A <- pca_res_x %>% 
	ggplot(aes(x=PC1,y=PC2, label=Var)) +
	geom_label(aes(fill = Var), colour = "white", size = 2, alpha=0.7, 
				  position=position_jitter(width=0.3,height=0.3)
				  )+
	labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
		  y=paste0("PC2: ",round(var_explained[2]*100,1),"%"))

library(factoextra)
p2B <- fviz_pca_ind(pca_res, col.ind = "cos2", # Color by the quality of representation
						  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE     # Avoid text overlapping
) 

p2C <- fviz_eig(pca_res)
# get_eig(pca_res) 

# p2A
# p2B
# p2C

# rotate matrix
d <- t(d)
d

# PCA of Variant ----
pca_res <- prcomp(d, scale = TRUE)
var_explained <- pca_res$sdev^2/sum(pca_res$sdev^2)

library(dplyr)
pca_res_x <- pca_res$x %>% as.data.frame()
pca_res_x$Var <- as.numeric( row.names(pca_res_x))

p3A <- pca_res_x %>% 
	ggplot(aes(x=PC1,y=PC2, label=Var)) +
	geom_label(aes(fill = Var), colour = "white", size = 2, alpha=0.7, 
				  position=position_jitter(width=0.3,height=0.3)
	)+
	labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
		  y=paste0("PC2: ",round(var_explained[2]*100,1),"%"))


library(factoextra)
p3B <- fviz_pca_ind(pca_res, col.ind = "cos2", # Color by the quality of representation
						  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE     # Avoid text overlapping
) + labs(title ="Variants - PCA")

p3C <- fviz_eig(pca_res)
# get_eig(pca_res) 

# p3A
# p3B
# p3C

library(cowplot)
mid_row <- plot_grid(p2A, p2B, p2C, labels = c('B'), ncol = 3)
bottom_row <- plot_grid(p3A, p3B, p3C, labels = c('C'), ncol = 3)
plot <- plot_grid(p1, mid_row, bottom_row, labels = c('A', '', ''), ncol = 1)

ggdraw(add_sub(vjust = 0.5, plot,
					"(A) Genotyope matrix.\n(B) PCA variance explained per Individual [typical]\n(C) PCA variance explained per Position [rotated matrix].\nPCAs (left) are manually derived from pca_res$sdev^2/sum(pca_res$sdev^2).\nPCAs (center) are made by fviz_pca_ind."))

