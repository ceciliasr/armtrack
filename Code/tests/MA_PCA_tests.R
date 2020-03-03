if (!require("matrixStats"))
{
  install.packages("matrixStats")
  library(matrixStats)
}
if (!require("ggplot2"))
{
  install.packages("ggplot2")
  library(ggplot2)
}

load("c:/TFM/Data/BDistrophyD.rda")
load("c:/TFM/Data/HealthyD.rda")
#Timestamp and data type are not relevant variables for PCA
x<-as.matrix(subset(BMD,select=-c(Timestamp,Type)))
#Performing PCA
pca<-prcomp(x,center=T,scale=T)
eigenvectors<-as.matrix(pca$rotation)
mu<-unname(pca$center)
mu<-matrix(mu,nrow=nrow(x),ncol=ncol(x),byrow=TRUE)
indiv_components<-as.matrix(pca$x)
res<-indiv_components%*%t(eigenvectors)
res_cent<-res+mu
res_sca<-scale(res_cent)
x_sca<-scale(x)
#Reconstructing first PCA component:
pc1<-scale(x)%*%eigenvectors[,1]
#  1) RHandPos_4 and LHandPos_4 (19.8\% total contrib.)
summary(scale(BMD$RHandPos_4))

x_sca_df<-data.frame(x_sca)



x_sca_df$RHandPos_4<-seq(min(x_sca_df$RHandPos_4),max(x_sca_df$RHandPos_4),(max(x_sca_df$RHandPos_4)-min(x_sca_df$RHandPos_4))/(nrow(x_sca_df)-1))
x_sca_df$LHandPos_4<-seq(max(x_sca_df$LHandPos_4),min(x_sca_df$LHandPos_4),-(max(x_sca_df$LHandPos_4)-min(x_sca_df$LHandPos_4))/(nrow(x_sca_df)-1))

pc1_2<-data.frame(as.matrix(x_sca_df)%*%eigenvectors[,1:2])

pc1_2<-cbind.data.frame(pc1_2,data.frame(x_sca_df$RHandPos_4))
names(pc1_2)[3]<-"RHandPos_4"

p <- ggplot(
  pc1_2, 
  aes(x = PC1, y=PC2)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "PC1", y = "PC2")
p
p + transition_time(pc1_2$RHandPos_4) +
  labs(title = "RHandPos_4: {frame_time}")


