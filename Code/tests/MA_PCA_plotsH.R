
setwd("c:/TFM/Data")

if (!require("rmatio"))
{
  install.packages("rmatio")
  library(rmatio)
}
if (!require("gganimate"))
{
  install.packages("gganimate")
  library(gganimate)
}
if (!require("ggplot2"))
{
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("plotly"))
{
  install.packages("plotly")
  library(plotly)
}
if (!require("rgl"))
{
  install.packages("rgl")
  library(rgl)
}
if (!require("gg3D"))
{
  install.packages("C:/temp/gg3D_0.0.0.9000.tar.gz", repos = NULL, type = "source")
  library(gg3D)
}
if (!require("matrixStats"))
{
  install.packages("matrixStats")
  library(matrixStats)
}
load("c:/TFM/Data/HealthyD.rda")
#Timestamp and data type are not relevant variables for PCA
x<-as.matrix(subset(HD,select=-c(Timestamp,Type)))
#Performing PCA
pca<-prcomp(x,center=T,scale=T)
eigenvectors<-as.matrix(pca$rotation) #PCs as columns, Initial variables as rows
mu<-unname(pca$center)
mu<-matrix(mu,nrow=nrow(x),ncol=ncol(x),byrow=TRUE)
sigma<-unname(apply(HD[,2:18], 2, sd, na.rm = TRUE))
sigma<-matrix(sigma,nrow=nrow(x),ncol=ncol(x),byrow=TRUE)
indiv_components<-as.matrix(pca$x) #PCs as columns, Individuals as rows

res1<-indiv_components[,1]%*%t(eigenvectors[,1])
res2<-indiv_components[,2]%*%t(eigenvectors[,2])
res3<-indiv_components[,3]%*%t(eigenvectors[,3])
res4<-indiv_components[,4]%*%t(eigenvectors[,4])
res5<-indiv_components[,5]%*%t(eigenvectors[,5])
res6<-indiv_components[,6]%*%t(eigenvectors[,6])
dd1<-(res1*sigma)+mu #Reconstructed database using only PC1
dd2<-(res2*sigma)+mu #Reconstructed database using only PC2
dd3<-(res3*sigma)+mu #Reconstructed database using only PC3
dd4<-(res4*sigma)+mu #Reconstructed database using only PC4
dd5<-(res5*sigma)+mu #Reconstructed database using only PC5
dd6<-(res6*sigma)+mu #Reconstructed database using only PC6

rm(indiv_components,res1,res2,res3,res4,res5,res6,sigma,mu,eigenvectors,pca,x)
dd1<-data.frame(dd1)
dd2<-data.frame(dd2)
dd3<-data.frame(dd3)
dd4<-data.frame(dd4)
dd5<-data.frame(dd5)
dd6<-data.frame(dd6)

dd<-read.mat("ShoulderPositions.mat")

R<-matrix(c(-1,0,0,0,-1,0,0,0,1),nrow=3)
p<-matrix(c(dd$HealthyLSh[1],dd$HealthyLSh[2],dd$HealthyLSh[3]),nrow=3)
LSh<-R%*%p
p<-matrix(c(dd$HealthyRSh[1],dd$HealthyRSh[2],dd$HealthyRSh[3]),nrow=3)
RSh<-R%*%p

dd1$LShPos_2<-LSh[1]
dd1$LShPos_3<-LSh[2]
dd1$LShPos_4<-LSh[3]
dd1$RShPos_2<-RSh[1]
dd1$RShPos_3<-RSh[2]
dd1$RShPos_4<-RSh[3]
dd1$Head_2<--129.10
dd1$Head_3<-0
dd1$Head_4<-1610
dd1$Neck_2<- -129.10
dd1$Neck_3<- 0
dd1$Neck_4<-1550
dd1$torso_2<--129.10
dd1$torso_3<-0
dd1$torso_4<-1285
dd1$waist_2<--129.10
dd1$waist_3<-0
dd1$waist_4<-1100

dd1$LThighPos_2<--129.1
dd1$LThighPos_3<-180
dd1$LThighPos_4<-950
dd1$RThighPos_2<--129.1
dd1$RThighPos_3<--180
dd1$RThighPos_4<-950

dd1$LKneePos_2<--129.1
dd1$LKneePos_3<-180
dd1$LKneePos_4<-730
dd1$RKneePos_2<--129.1
dd1$RKneePos_3<--180
dd1$RKneePos_4<-730

dd1$LAnklePos_2<--129.1
dd1$LAnklePos_3<-180
dd1$LAnklePos_4<-480
dd1$RAnklePos_2<--129.1
dd1$RAnklePos_3<--180
dd1$RAnklePos_4<-480


dd2$LShPos_2<-LSh[1]
dd2$LShPos_3<-LSh[2]
dd2$LShPos_4<-LSh[3]
dd2$RShPos_2<-RSh[1]
dd2$RShPos_3<-RSh[2]
dd2$RShPos_4<-RSh[3]
dd2$Head_2<--129.10
dd2$Head_3<-0
dd2$Head_4<-1610
dd2$Neck_2<- -129.10
dd2$Neck_3<- 0
dd2$Neck_4<-1550
dd2$torso_2<--129.10
dd2$torso_3<-0
dd2$torso_4<-1285
dd2$waist_2<--129.10
dd2$waist_3<-0
dd2$waist_4<-1100

dd2$LThighPos_2<--129.1
dd2$LThighPos_3<-180
dd2$LThighPos_4<-950
dd2$RThighPos_2<--129.1
dd2$RThighPos_3<--180
dd2$RThighPos_4<-950

dd2$LKneePos_2<--129.1
dd2$LKneePos_3<-180
dd2$LKneePos_4<-730
dd2$RKneePos_2<--129.1
dd2$RKneePos_3<--180
dd2$RKneePos_4<-730

dd2$LAnklePos_2<--129.1
dd2$LAnklePos_3<-180
dd2$LAnklePos_4<-480
dd2$RAnklePos_2<--129.1
dd2$RAnklePos_3<--180
dd2$RAnklePos_4<-480


dd3$LShPos_2<-LSh[1]
dd3$LShPos_3<-LSh[2]
dd3$LShPos_4<-LSh[3]
dd3$RShPos_2<-RSh[1]
dd3$RShPos_3<-RSh[2]
dd3$RShPos_4<-RSh[3]
dd3$Head_2<--129.10
dd3$Head_3<-0
dd3$Head_4<-1610
dd3$Neck_2<- -129.10
dd3$Neck_3<- 0
dd3$Neck_4<-1550
dd3$torso_2<--129.10
dd3$torso_3<-0
dd3$torso_4<-1285
dd3$waist_2<--129.10
dd3$waist_3<-0
dd3$waist_4<-1100

dd3$LThighPos_2<--129.1
dd3$LThighPos_3<-180
dd3$LThighPos_4<-950
dd3$RThighPos_2<--129.1
dd3$RThighPos_3<--180
dd3$RThighPos_4<-950

dd3$LKneePos_2<--129.1
dd3$LKneePos_3<-180
dd3$LKneePos_4<-730
dd3$RKneePos_2<--129.1
dd3$RKneePos_3<--180
dd3$RKneePos_4<-730

dd3$LAnklePos_2<--129.1
dd3$LAnklePos_3<-180
dd3$LAnklePos_4<-480
dd3$RAnklePos_2<--129.1
dd3$RAnklePos_3<--180
dd3$RAnklePos_4<-480


dd4$LShPos_2<-LSh[1]
dd4$LShPos_3<-LSh[2]
dd4$LShPos_4<-LSh[3]
dd4$RShPos_2<-RSh[1]
dd4$RShPos_3<-RSh[2]
dd4$RShPos_4<-RSh[3]
dd4$Head_2<--129.10
dd4$Head_3<-0
dd4$Head_4<-1610
dd4$Neck_2<- -129.10
dd4$Neck_3<- 0
dd4$Neck_4<-1550
dd4$torso_2<--129.10
dd4$torso_3<-0
dd4$torso_4<-1285
dd4$waist_2<--129.10
dd4$waist_3<-0
dd4$waist_4<-1100

dd4$LThighPos_2<--129.1
dd4$LThighPos_3<-180
dd4$LThighPos_4<-950
dd4$RThighPos_2<--129.1
dd4$RThighPos_3<--180
dd4$RThighPos_4<-950

dd4$LKneePos_2<--129.1
dd4$LKneePos_3<-180
dd4$LKneePos_4<-730
dd4$RKneePos_2<--129.1
dd4$RKneePos_3<--180
dd4$RKneePos_4<-730

dd4$LAnklePos_2<--129.1
dd4$LAnklePos_3<-180
dd4$LAnklePos_4<-480
dd4$RAnklePos_2<--129.1
dd4$RAnklePos_3<--180
dd4$RAnklePos_4<-480


dd5$LShPos_2<-LSh[1]
dd5$LShPos_3<-LSh[2]
dd5$LShPos_4<-LSh[3]
dd5$RShPos_2<-RSh[1]
dd5$RShPos_3<-RSh[2]
dd5$RShPos_4<-RSh[3]
dd5$Head_2<--129.10
dd5$Head_3<-0
dd5$Head_4<-1610
dd5$Neck_2<- -129.10
dd5$Neck_3<- 0
dd5$Neck_4<-1550
dd5$torso_2<--129.10
dd5$torso_3<-0
dd5$torso_4<-1285
dd5$waist_2<--129.10
dd5$waist_3<-0
dd5$waist_4<-1100

dd5$LThighPos_2<--129.1
dd5$LThighPos_3<-180
dd5$LThighPos_4<-950
dd5$RThighPos_2<--129.1
dd5$RThighPos_3<--180
dd5$RThighPos_4<-950

dd5$LKneePos_2<--129.1
dd5$LKneePos_3<-180
dd5$LKneePos_4<-730
dd5$RKneePos_2<--129.1
dd5$RKneePos_3<--180
dd5$RKneePos_4<-730

dd5$LAnklePos_2<--129.1
dd5$LAnklePos_3<-180
dd5$LAnklePos_4<-480
dd5$RAnklePos_2<--129.1
dd5$RAnklePos_3<--180
dd5$RAnklePos_4<-480


dd6$LShPos_2<-LSh[1]
dd6$LShPos_3<-LSh[2]
dd6$LShPos_4<-LSh[3]
dd6$RShPos_2<-RSh[1]
dd6$RShPos_3<-RSh[2]
dd6$RShPos_4<-RSh[3]
dd6$Head_2<--129.10
dd6$Head_3<-0
dd6$Head_4<-1610
dd6$Neck_2<- -129.10
dd6$Neck_3<- 0
dd6$Neck_4<-1550
dd6$torso_2<--129.10
dd6$torso_3<-0
dd6$torso_4<-1285
dd6$waist_2<--129.10
dd6$waist_3<-0
dd6$waist_4<-1100

dd6$LThighPos_2<--129.1
dd6$LThighPos_3<-180
dd6$LThighPos_4<-950
dd6$RThighPos_2<--129.1
dd6$RThighPos_3<--180
dd6$RThighPos_4<-950

dd6$LKneePos_2<--129.1
dd6$LKneePos_3<-180
dd6$LKneePos_4<-730
dd6$RKneePos_2<--129.1
dd6$RKneePos_3<--180
dd6$RKneePos_4<-730

dd6$LAnklePos_2<--129.1
dd6$LAnklePos_3<-180
dd6$LAnklePos_4<-480
dd6$RAnklePos_2<--129.1
dd6$RAnklePos_3<--180
dd6$RAnklePos_4<-480

rm(p,R,LSh,RSh)
dd1$Timestamp<-HD$Timestamp
dd2$Timestamp<-HD$Timestamp
dd3$Timestamp<-HD$Timestamp
dd4$Timestamp<-HD$Timestamp
dd5$Timestamp<-HD$Timestamp
dd6$Timestamp<-HD$Timestamp


save(dd1,file="c:/TFM/Data/HealthyPC1.rda")
save(dd2,file="c:/TFM/Data/HealthyPC2.rda")
save(dd3,file="c:/TFM/Data/HealthyPC3.rda")
save(dd4,file="c:/TFM/Data/HealthyPC4.rda")
save(dd5,file="c:/TFM/Data/HealthyPC5.rda")
save(dd6,file="c:/TFM/Data/HealthyPC6.rda")

dd1<-subset(dd1,select=c(Timestamp,LHandPos_2,RHandPos_2,LHandPos_3,RHandPos_3,LHandPos_4,RHandPos_4,
                         LElbPos_2,RElbPos_2,LElbPos_3,RElbPos_3,LElbPos_4,RElbPos_4,
                         LShPos_2,RShPos_2,LShPos_3,RShPos_3,LShPos_4,RShPos_4,Head_2,Head_3,Head_4,
                         Neck_2, Neck_3, Neck_4, torso_2, torso_3, torso_4, waist_2, waist_3, waist_4,
                         LThighPos_2, LThighPos_3, LThighPos_4, RThighPos_2, RThighPos_3, RThighPos_4,
                         LKneePos_2, LKneePos_3, LKneePos_4, RKneePos_2, RKneePos_3, RKneePos_4,
                         LAnklePos_2, LAnklePos_3, LAnklePos_4, RAnklePos_2, RAnklePos_3, RAnklePos_4))
dd2<-subset(dd2,select=c(Timestamp,LHandPos_2,RHandPos_2,LHandPos_3,RHandPos_3,LHandPos_4,RHandPos_4,
                         LElbPos_2,RElbPos_2,LElbPos_3,RElbPos_3,LElbPos_4,RElbPos_4,
                         LShPos_2,RShPos_2,LShPos_3,RShPos_3,LShPos_4,RShPos_4,Head_2,Head_3,Head_4,
                         Neck_2, Neck_3, Neck_4, torso_2, torso_3, torso_4, waist_2, waist_3, waist_4,
                         LThighPos_2, LThighPos_3, LThighPos_4, RThighPos_2, RThighPos_3, RThighPos_4,
                         LKneePos_2, LKneePos_3, LKneePos_4, RKneePos_2, RKneePos_3, RKneePos_4,
                         LAnklePos_2, LAnklePos_3, LAnklePos_4, RAnklePos_2, RAnklePos_3, RAnklePos_4))
dd3<-subset(dd3,select=c(Timestamp,LHandPos_2,RHandPos_2,LHandPos_3,RHandPos_3,LHandPos_4,RHandPos_4,
                         LElbPos_2,RElbPos_2,LElbPos_3,RElbPos_3,LElbPos_4,RElbPos_4,
                         LShPos_2,RShPos_2,LShPos_3,RShPos_3,LShPos_4,RShPos_4,Head_2,Head_3,Head_4,
                         Neck_2, Neck_3, Neck_4, torso_2, torso_3, torso_4, waist_2, waist_3, waist_4,
                         LThighPos_2, LThighPos_3, LThighPos_4, RThighPos_2, RThighPos_3, RThighPos_4,
                         LKneePos_2, LKneePos_3, LKneePos_4, RKneePos_2, RKneePos_3, RKneePos_4,
                         LAnklePos_2, LAnklePos_3, LAnklePos_4, RAnklePos_2, RAnklePos_3, RAnklePos_4))
dd4<-subset(dd4,select=c(Timestamp,LHandPos_2,RHandPos_2,LHandPos_3,RHandPos_3,LHandPos_4,RHandPos_4,
                         LElbPos_2,RElbPos_2,LElbPos_3,RElbPos_3,LElbPos_4,RElbPos_4,
                         LShPos_2,RShPos_2,LShPos_3,RShPos_3,LShPos_4,RShPos_4,Head_2,Head_3,Head_4,
                         Neck_2, Neck_3, Neck_4, torso_2, torso_3, torso_4, waist_2, waist_3, waist_4,
                         LThighPos_2, LThighPos_3, LThighPos_4, RThighPos_2, RThighPos_3, RThighPos_4,
                         LKneePos_2, LKneePos_3, LKneePos_4, RKneePos_2, RKneePos_3, RKneePos_4,
                         LAnklePos_2, LAnklePos_3, LAnklePos_4, RAnklePos_2, RAnklePos_3, RAnklePos_4))
dd5<-subset(dd5,select=c(Timestamp,LHandPos_2,RHandPos_2,LHandPos_3,RHandPos_3,LHandPos_4,RHandPos_4,
                         LElbPos_2,RElbPos_2,LElbPos_3,RElbPos_3,LElbPos_4,RElbPos_4,
                         LShPos_2,RShPos_2,LShPos_3,RShPos_3,LShPos_4,RShPos_4,Head_2,Head_3,Head_4,
                         Neck_2, Neck_3, Neck_4, torso_2, torso_3, torso_4, waist_2, waist_3, waist_4,
                         LThighPos_2, LThighPos_3, LThighPos_4, RThighPos_2, RThighPos_3, RThighPos_4,
                         LKneePos_2, LKneePos_3, LKneePos_4, RKneePos_2, RKneePos_3, RKneePos_4,
                         LAnklePos_2, LAnklePos_3, LAnklePos_4, RAnklePos_2, RAnklePos_3, RAnklePos_4))
dd6<-subset(dd6,select=c(Timestamp,LHandPos_2,RHandPos_2,LHandPos_3,RHandPos_3,LHandPos_4,RHandPos_4,
                         LElbPos_2,RElbPos_2,LElbPos_3,RElbPos_3,LElbPos_4,RElbPos_4,
                         LShPos_2,RShPos_2,LShPos_3,RShPos_3,LShPos_4,RShPos_4,Head_2,Head_3,Head_4,
                         Neck_2, Neck_3, Neck_4, torso_2, torso_3, torso_4, waist_2, waist_3, waist_4,
                         LThighPos_2, LThighPos_3, LThighPos_4, RThighPos_2, RThighPos_3, RThighPos_4,
                         LKneePos_2, LKneePos_3, LKneePos_4, RKneePos_2, RKneePos_3, RKneePos_4,
                         LAnklePos_2, LAnklePos_3, LAnklePos_4, RAnklePos_2, RAnklePos_3, RAnklePos_4))

se<-seq(0,nrow(dd6),50)
dd1<-dd1[rownames(dd1) %in% se,]
dd2<-dd2[rownames(dd2) %in% se,]
dd3<-dd3[rownames(dd3) %in% se,]
dd4<-dd4[rownames(dd4) %in% se,]
dd5<-dd5[rownames(dd5) %in% se,]
dd6<-dd6[rownames(dd6) %in% se,]
rm(se,dd)


LHand<-subset(dd1,select=c(Timestamp,LHandPos_2,LHandPos_3,LHandPos_4))
names(LHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LElb<-subset(dd1,select=c(Timestamp,LElbPos_2,LElbPos_3,LElbPos_4))
names(LElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LSh<-subset(dd1,select=c(Timestamp,LShPos_2,LShPos_3,LShPos_4))
names(LSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
Neck<-subset(dd1,select=c(Timestamp,Neck_2,Neck_3,Neck_4))
names(Neck)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
Head<-subset(dd1,select=c(Timestamp,Head_2,Head_3,Head_4))
names(Head)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RSh<-subset(dd1,select=c(Timestamp,RShPos_2,RShPos_3,RShPos_4))
names(RSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RElb<-subset(dd1,select=c(Timestamp,RElbPos_2,RElbPos_3,RElbPos_4))
names(RElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RHand<-subset(dd1,select=c(Timestamp,RHandPos_2,RHandPos_3,RHandPos_4))
names(RHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")

torso<-subset(dd1,select=c(Timestamp,torso_2,torso_3,torso_4))
names(torso)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
waist<-subset(dd1,select=c(Timestamp,waist_2,waist_3,waist_4))
names(waist)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LThighPos<-subset(dd1,select=c(Timestamp,LThighPos_2,LThighPos_3,LThighPos_4))
names(LThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RThighPos<-subset(dd1,select=c(Timestamp,RThighPos_2,RThighPos_3,RThighPos_4))
names(RThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LKneePos<-subset(dd1,select=c(Timestamp,LKneePos_2,LKneePos_3,LKneePos_4))
names(LKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RKneePos<-subset(dd1,select=c(Timestamp,RKneePos_2,RKneePos_3,RKneePos_4))
names(RKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LAnklePos<-subset(dd1,select=c(Timestamp,LAnklePos_2,LAnklePos_3,LAnklePos_4))
names(LAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RAnklePos<-subset(dd1,select=c(Timestamp,RAnklePos_2,RAnklePos_3,RAnklePos_4))
names(RAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")

d1<-rbind.data.frame(Head,Neck,LSh,LElb,LHand,LElb,LSh,Neck,RSh,RElb,RHand,RElb,RSh, Neck, torso, waist,
                     LThighPos,LKneePos,LAnklePos,LKneePos,LThighPos,waist,RThighPos,RKneePos,RAnklePos,
                     RKneePos,RThighPos,waist,torso,Neck)

d1<-d1[order(d1$Timestamp),]



LHand<-subset(dd2,select=c(Timestamp,LHandPos_2,LHandPos_3,LHandPos_4))
names(LHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LElb<-subset(dd2,select=c(Timestamp,LElbPos_2,LElbPos_3,LElbPos_4))
names(LElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LSh<-subset(dd2,select=c(Timestamp,LShPos_2,LShPos_3,LShPos_4))
names(LSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
Neck<-subset(dd2,select=c(Timestamp,Neck_2,Neck_3,Neck_4))
names(Neck)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
Head<-subset(dd2,select=c(Timestamp,Head_2,Head_3,Head_4))
names(Head)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RSh<-subset(dd2,select=c(Timestamp,RShPos_2,RShPos_3,RShPos_4))
names(RSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RElb<-subset(dd2,select=c(Timestamp,RElbPos_2,RElbPos_3,RElbPos_4))
names(RElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RHand<-subset(dd2,select=c(Timestamp,RHandPos_2,RHandPos_3,RHandPos_4))
names(RHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")

torso<-subset(dd2,select=c(Timestamp,torso_2,torso_3,torso_4))
names(torso)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
waist<-subset(dd2,select=c(Timestamp,waist_2,waist_3,waist_4))
names(waist)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LThighPos<-subset(dd2,select=c(Timestamp,LThighPos_2,LThighPos_3,LThighPos_4))
names(LThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RThighPos<-subset(dd2,select=c(Timestamp,RThighPos_2,RThighPos_3,RThighPos_4))
names(RThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LKneePos<-subset(dd2,select=c(Timestamp,LKneePos_2,LKneePos_3,LKneePos_4))
names(LKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RKneePos<-subset(dd2,select=c(Timestamp,RKneePos_2,RKneePos_3,RKneePos_4))
names(RKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LAnklePos<-subset(dd2,select=c(Timestamp,LAnklePos_2,LAnklePos_3,LAnklePos_4))
names(LAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RAnklePos<-subset(dd2,select=c(Timestamp,RAnklePos_2,RAnklePos_3,RAnklePos_4))
names(RAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")

d2<-rbind.data.frame(Head,Neck,LSh,LElb,LHand,LElb,LSh,Neck,RSh,RElb,RHand,RElb,RSh, Neck, torso, waist,
                     LThighPos,LKneePos,LAnklePos,LKneePos,LThighPos,waist,RThighPos,RKneePos,RAnklePos,
                     RKneePos,RThighPos,waist,torso,Neck)

d2<-d2[order(d2$Timestamp),]


LHand<-subset(dd3,select=c(Timestamp,LHandPos_2,LHandPos_3,LHandPos_4))
names(LHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LElb<-subset(dd3,select=c(Timestamp,LElbPos_2,LElbPos_3,LElbPos_4))
names(LElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LSh<-subset(dd3,select=c(Timestamp,LShPos_2,LShPos_3,LShPos_4))
names(LSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
Neck<-subset(dd3,select=c(Timestamp,Neck_2,Neck_3,Neck_4))
names(Neck)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
Head<-subset(dd3,select=c(Timestamp,Head_2,Head_3,Head_4))
names(Head)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RSh<-subset(dd3,select=c(Timestamp,RShPos_2,RShPos_3,RShPos_4))
names(RSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RElb<-subset(dd3,select=c(Timestamp,RElbPos_2,RElbPos_3,RElbPos_4))
names(RElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RHand<-subset(dd3,select=c(Timestamp,RHandPos_2,RHandPos_3,RHandPos_4))
names(RHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")

torso<-subset(dd3,select=c(Timestamp,torso_2,torso_3,torso_4))
names(torso)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
waist<-subset(dd3,select=c(Timestamp,waist_2,waist_3,waist_4))
names(waist)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LThighPos<-subset(dd3,select=c(Timestamp,LThighPos_2,LThighPos_3,LThighPos_4))
names(LThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RThighPos<-subset(dd3,select=c(Timestamp,RThighPos_2,RThighPos_3,RThighPos_4))
names(RThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LKneePos<-subset(dd3,select=c(Timestamp,LKneePos_2,LKneePos_3,LKneePos_4))
names(LKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RKneePos<-subset(dd3,select=c(Timestamp,RKneePos_2,RKneePos_3,RKneePos_4))
names(RKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LAnklePos<-subset(dd3,select=c(Timestamp,LAnklePos_2,LAnklePos_3,LAnklePos_4))
names(LAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RAnklePos<-subset(dd3,select=c(Timestamp,RAnklePos_2,RAnklePos_3,RAnklePos_4))
names(RAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")

d3<-rbind.data.frame(Head,Neck,LSh,LElb,LHand,LElb,LSh,Neck,RSh,RElb,RHand,RElb,RSh, Neck, torso, waist,
                     LThighPos,LKneePos,LAnklePos,LKneePos,LThighPos,waist,RThighPos,RKneePos,RAnklePos,
                     RKneePos,RThighPos,waist,torso,Neck)

d3<-d3[order(d3$Timestamp),]


LHand<-subset(dd4,select=c(Timestamp,LHandPos_2,LHandPos_3,LHandPos_4))
names(LHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LElb<-subset(dd4,select=c(Timestamp,LElbPos_2,LElbPos_3,LElbPos_4))
names(LElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LSh<-subset(dd4,select=c(Timestamp,LShPos_2,LShPos_3,LShPos_4))
names(LSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
Neck<-subset(dd4,select=c(Timestamp,Neck_2,Neck_3,Neck_4))
names(Neck)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
Head<-subset(dd4,select=c(Timestamp,Head_2,Head_3,Head_4))
names(Head)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RSh<-subset(dd4,select=c(Timestamp,RShPos_2,RShPos_3,RShPos_4))
names(RSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RElb<-subset(dd4,select=c(Timestamp,RElbPos_2,RElbPos_3,RElbPos_4))
names(RElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RHand<-subset(dd4,select=c(Timestamp,RHandPos_2,RHandPos_3,RHandPos_4))
names(RHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")

torso<-subset(dd4,select=c(Timestamp,torso_2,torso_3,torso_4))
names(torso)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
waist<-subset(dd4,select=c(Timestamp,waist_2,waist_3,waist_4))
names(waist)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LThighPos<-subset(dd4,select=c(Timestamp,LThighPos_2,LThighPos_3,LThighPos_4))
names(LThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RThighPos<-subset(dd4,select=c(Timestamp,RThighPos_2,RThighPos_3,RThighPos_4))
names(RThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LKneePos<-subset(dd4,select=c(Timestamp,LKneePos_2,LKneePos_3,LKneePos_4))
names(LKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RKneePos<-subset(dd4,select=c(Timestamp,RKneePos_2,RKneePos_3,RKneePos_4))
names(RKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LAnklePos<-subset(dd4,select=c(Timestamp,LAnklePos_2,LAnklePos_3,LAnklePos_4))
names(LAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RAnklePos<-subset(dd4,select=c(Timestamp,RAnklePos_2,RAnklePos_3,RAnklePos_4))
names(RAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")

d4<-rbind.data.frame(Head,Neck,LSh,LElb,LHand,LElb,LSh,Neck,RSh,RElb,RHand,RElb,RSh, Neck, torso, waist,
                     LThighPos,LKneePos,LAnklePos,LKneePos,LThighPos,waist,RThighPos,RKneePos,RAnklePos,
                     RKneePos,RThighPos,waist,torso,Neck)

d4<-d4[order(d4$Timestamp),]


LHand<-subset(dd5,select=c(Timestamp,LHandPos_2,LHandPos_3,LHandPos_4))
names(LHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LElb<-subset(dd5,select=c(Timestamp,LElbPos_2,LElbPos_3,LElbPos_4))
names(LElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LSh<-subset(dd5,select=c(Timestamp,LShPos_2,LShPos_3,LShPos_4))
names(LSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
Neck<-subset(dd5,select=c(Timestamp,Neck_2,Neck_3,Neck_4))
names(Neck)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
Head<-subset(dd5,select=c(Timestamp,Head_2,Head_3,Head_4))
names(Head)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RSh<-subset(dd5,select=c(Timestamp,RShPos_2,RShPos_3,RShPos_4))
names(RSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RElb<-subset(dd5,select=c(Timestamp,RElbPos_2,RElbPos_3,RElbPos_4))
names(RElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RHand<-subset(dd5,select=c(Timestamp,RHandPos_2,RHandPos_3,RHandPos_4))
names(RHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")

torso<-subset(dd5,select=c(Timestamp,torso_2,torso_3,torso_4))
names(torso)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
waist<-subset(dd5,select=c(Timestamp,waist_2,waist_3,waist_4))
names(waist)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LThighPos<-subset(dd5,select=c(Timestamp,LThighPos_2,LThighPos_3,LThighPos_4))
names(LThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RThighPos<-subset(dd5,select=c(Timestamp,RThighPos_2,RThighPos_3,RThighPos_4))
names(RThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LKneePos<-subset(dd5,select=c(Timestamp,LKneePos_2,LKneePos_3,LKneePos_4))
names(LKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RKneePos<-subset(dd5,select=c(Timestamp,RKneePos_2,RKneePos_3,RKneePos_4))
names(RKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LAnklePos<-subset(dd5,select=c(Timestamp,LAnklePos_2,LAnklePos_3,LAnklePos_4))
names(LAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RAnklePos<-subset(dd5,select=c(Timestamp,RAnklePos_2,RAnklePos_3,RAnklePos_4))
names(RAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")

d5<-rbind.data.frame(Head,Neck,LSh,LElb,LHand,LElb,LSh,Neck,RSh,RElb,RHand,RElb,RSh, Neck, torso, waist,
                     LThighPos,LKneePos,LAnklePos,LKneePos,LThighPos,waist,RThighPos,RKneePos,RAnklePos,
                     RKneePos,RThighPos,waist,torso,Neck)

d5<-d5[order(d5$Timestamp),]


LHand<-subset(dd6,select=c(Timestamp,LHandPos_2,LHandPos_3,LHandPos_4))
names(LHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LElb<-subset(dd6,select=c(Timestamp,LElbPos_2,LElbPos_3,LElbPos_4))
names(LElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LSh<-subset(dd6,select=c(Timestamp,LShPos_2,LShPos_3,LShPos_4))
names(LSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
Neck<-subset(dd6,select=c(Timestamp,Neck_2,Neck_3,Neck_4))
names(Neck)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
Head<-subset(dd6,select=c(Timestamp,Head_2,Head_3,Head_4))
names(Head)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RSh<-subset(dd6,select=c(Timestamp,RShPos_2,RShPos_3,RShPos_4))
names(RSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RElb<-subset(dd6,select=c(Timestamp,RElbPos_2,RElbPos_3,RElbPos_4))
names(RElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RHand<-subset(dd6,select=c(Timestamp,RHandPos_2,RHandPos_3,RHandPos_4))
names(RHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")

torso<-subset(dd6,select=c(Timestamp,torso_2,torso_3,torso_4))
names(torso)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
waist<-subset(dd6,select=c(Timestamp,waist_2,waist_3,waist_4))
names(waist)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LThighPos<-subset(dd6,select=c(Timestamp,LThighPos_2,LThighPos_3,LThighPos_4))
names(LThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RThighPos<-subset(dd6,select=c(Timestamp,RThighPos_2,RThighPos_3,RThighPos_4))
names(RThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LKneePos<-subset(dd6,select=c(Timestamp,LKneePos_2,LKneePos_3,LKneePos_4))
names(LKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RKneePos<-subset(dd6,select=c(Timestamp,RKneePos_2,RKneePos_3,RKneePos_4))
names(RKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LAnklePos<-subset(dd6,select=c(Timestamp,LAnklePos_2,LAnklePos_3,LAnklePos_4))
names(LAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RAnklePos<-subset(dd6,select=c(Timestamp,RAnklePos_2,RAnklePos_3,RAnklePos_4))
names(RAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")

d6<-rbind.data.frame(Head,Neck,LSh,LElb,LHand,LElb,LSh,Neck,RSh,RElb,RHand,RElb,RSh, Neck, torso, waist,
                     LThighPos,LKneePos,LAnklePos,LKneePos,LThighPos,waist,RThighPos,RKneePos,RAnklePos,
                     RKneePos,RThighPos,waist,torso,Neck)

d6<-d6[order(d6$Timestamp),]


save(d1,file="c:/TFM/Data/HealthyPC1_plot.rda")
save(d2,file="c:/TFM/Data/HealthyPC2_plot.rda")
save(d3,file="c:/TFM/Data/HealthyPC3_plot.rda")
save(d4,file="c:/TFM/Data/HealthyPC4_plot.rda")
save(d5,file="c:/TFM/Data/HealthyPC5_plot.rda")
save(d6,file="c:/TFM/Data/HealthyPC6_plot.rda")

load("HealthyD.rda")
dd1<-HD
rm(HD)
dd<-read.mat("ShoulderPositions.mat")

R<-matrix(c(-1,0,0,0,-1,0,0,0,1),nrow=3)
p<-matrix(c(dd$HealthyLSh[1],dd$HealthyLSh[2],dd$HealthyLSh[3]),nrow=3)
LSh<-R%*%p
p<-matrix(c(dd$HealthyRSh[1],dd$HealthyRSh[2],dd$HealthyRSh[3]),nrow=3)
RSh<-R%*%p

dd1$LShPos_2<-LSh[1]
dd1$LShPos_3<-LSh[2]
dd1$LShPos_4<-LSh[3]
dd1$RShPos_2<-RSh[1]
dd1$RShPos_3<-RSh[2]
dd1$RShPos_4<-RSh[3]
dd1$Head_2<--129.10
dd1$Head_3<-0
dd1$Head_4<-1610
dd1$Neck_2<- -129.10
dd1$Neck_3<- 0
dd1$Neck_4<-1550
dd1$torso_2<--129.10
dd1$torso_3<-0
dd1$torso_4<-1285
dd1$waist_2<--129.10
dd1$waist_3<-0
dd1$waist_4<-1100

dd1$LThighPos_2<--129.1
dd1$LThighPos_3<-180
dd1$LThighPos_4<-950
dd1$RThighPos_2<--129.1
dd1$RThighPos_3<--180
dd1$RThighPos_4<-950

dd1$LKneePos_2<--129.1
dd1$LKneePos_3<-180
dd1$LKneePos_4<-730
dd1$RKneePos_2<--129.1
dd1$RKneePos_3<--180
dd1$RKneePos_4<-730

dd1$LAnklePos_2<--129.1
dd1$LAnklePos_3<-180
dd1$LAnklePos_4<-480
dd1$RAnklePos_2<--129.1
dd1$RAnklePos_3<--180
dd1$RAnklePos_4<-480

ddT<-dd1
save(ddT,file="HealthyT.rda")
se<-seq(0,nrow(dd1),50)
dd1<-dd1[rownames(dd1) %in% se,]
rm(se)


dd1<-subset(dd1,select=c(Timestamp,LHandPos_2,RHandPos_2,LHandPos_3,RHandPos_3,LHandPos_4,RHandPos_4,
                         LElbPos_2,RElbPos_2,LElbPos_3,RElbPos_3,LElbPos_4,RElbPos_4,
                         LShPos_2,RShPos_2,LShPos_3,RShPos_3,LShPos_4,RShPos_4,Head_2,Head_3,Head_4,
                         Neck_2, Neck_3, Neck_4, torso_2, torso_3, torso_4, waist_2, waist_3, waist_4,
                         LThighPos_2, LThighPos_3, LThighPos_4, RThighPos_2, RThighPos_3, RThighPos_4,
                         LKneePos_2, LKneePos_3, LKneePos_4, RKneePos_2, RKneePos_3, RKneePos_4,
                         LAnklePos_2, LAnklePos_3, LAnklePos_4, RAnklePos_2, RAnklePos_3, RAnklePos_4))
rm(dd,LSh,RSh,p,R)

LHand<-subset(dd1,select=c(Timestamp,LHandPos_2,LHandPos_3,LHandPos_4))
names(LHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LElb<-subset(dd1,select=c(Timestamp,LElbPos_2,LElbPos_3,LElbPos_4))
names(LElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LSh<-subset(dd1,select=c(Timestamp,LShPos_2,LShPos_3,LShPos_4))
names(LSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
Neck<-subset(dd1,select=c(Timestamp,Neck_2,Neck_3,Neck_4))
names(Neck)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
Head<-subset(dd1,select=c(Timestamp,Head_2,Head_3,Head_4))
names(Head)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RSh<-subset(dd1,select=c(Timestamp,RShPos_2,RShPos_3,RShPos_4))
names(RSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RElb<-subset(dd1,select=c(Timestamp,RElbPos_2,RElbPos_3,RElbPos_4))
names(RElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RHand<-subset(dd1,select=c(Timestamp,RHandPos_2,RHandPos_3,RHandPos_4))
names(RHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")

torso<-subset(dd1,select=c(Timestamp,torso_2,torso_3,torso_4))
names(torso)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
waist<-subset(dd1,select=c(Timestamp,waist_2,waist_3,waist_4))
names(waist)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LThighPos<-subset(dd1,select=c(Timestamp,LThighPos_2,LThighPos_3,LThighPos_4))
names(LThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RThighPos<-subset(dd1,select=c(Timestamp,RThighPos_2,RThighPos_3,RThighPos_4))
names(RThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LKneePos<-subset(dd1,select=c(Timestamp,LKneePos_2,LKneePos_3,LKneePos_4))
names(LKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RKneePos<-subset(dd1,select=c(Timestamp,RKneePos_2,RKneePos_3,RKneePos_4))
names(RKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
LAnklePos<-subset(dd1,select=c(Timestamp,LAnklePos_2,LAnklePos_3,LAnklePos_4))
names(LAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
RAnklePos<-subset(dd1,select=c(Timestamp,RAnklePos_2,RAnklePos_3,RAnklePos_4))
names(RAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")

dd<-rbind.data.frame(Head,Neck,LSh,LElb,LHand,LElb,LSh,Neck,RSh,RElb,RHand,RElb,RSh, Neck, torso, waist,
                     LThighPos,LKneePos,LAnklePos,LKneePos,LThighPos,waist,RThighPos,RKneePos,RAnklePos,
                     RKneePos,RThighPos,waist,torso,Neck)

dd<-dd[order(dd$Timestamp),]

rm(LHand,LElb,LSh,RSh,RElb,RHand,Head,Neck, torso, LAnklePos,LKneePos,LThighPos,waist,RAnklePos,
   RKneePos,RThighPos,dd1,dd2,dd3,dd4,dd5,dd6)
save(dd,file="c:/TFM/Data/HealthyT_plot.rda")
# create a canvas 


canvas <- ggplot(dd[347355:455355,], aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
  theme_void()+ coord_equal()+ transition_time(Timestamp)


p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = -85, phi = 0)  + 
  stat_3D(data = dd[347355:455355,],mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = -85, phi = 0)
pT<-animate(p_with_canv,1100)
anim_save("c:/TFM/Results/HDT_347355_455355_1100.gif",pT)



canvas <- ggplot(d1[347355:455355,], aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
  theme_void()+ coord_equal()+ transition_time(Timestamp)


p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = -85, phi = 0)  + 
  stat_3D(data = d1[347355:455355,],mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = -85, phi = 0)
p1<-animate(p_with_canv,1100)
anim_save("c:/TFM/Results/HDPC1_347355_455355_1100.gif",p1)

canvas <- ggplot(d2[347355:455355,], aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
  theme_void()+ coord_equal()+ transition_time(Timestamp)


p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = -85, phi = 0)  + 
  stat_3D(data = d2[347355:455355,],mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = -85, phi = 0)
p2<-animate(p_with_canv,1100)
anim_save("c:/TFM/Results/HDPC2_347355_455355_1100.gif",p2)

canvas <- ggplot(d3[347355:455355,], aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
  theme_void()+ coord_equal()+ transition_time(Timestamp)

p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = -85, phi = 0)  + 
  stat_3D(data = d3[347355:455355,],mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = -85, phi = 0)
p3<-animate(p_with_canv,1100)
anim_save("c:/TFM/Results/HDPC3_347355_455355_1100.gif",p3)

canvas <- ggplot(d4[347355:455355,], aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
  theme_void()+ coord_equal()+ transition_time(Timestamp)

p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = -85, phi = 0)  + 
  stat_3D(data = d4[347355:455355,],mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = -85, phi = 0)
p4<-animate(p_with_canv,1100)
anim_save("c:/TFM/Results/HDPC4_347355_455355_1100.gif",p4)

canvas <- ggplot(d5[347355:455355,], aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
  theme_void()+ coord_equal()+ transition_time(Timestamp)

p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = -85, phi = 0)  + 
  stat_3D(data = d5[347355:455355,],mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = -85, phi = 0)
p5<-animate(p_with_canv,1100)
anim_save("c:/TFM/Results/HDPC5_347355_455355_1100.gif",p5)

canvas <- ggplot(d6[347355:455355,], aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
  theme_void()+ coord_equal()+ transition_time(Timestamp)

p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = -85, phi = 0)  + 
  stat_3D(data = d6[347355:455355,],mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = -85, phi = 0)
p6<-animate(p_with_canv,1100)
anim_save("c:/TFM/Results/HDPC6_347355_455355_1100.gif",p6)

