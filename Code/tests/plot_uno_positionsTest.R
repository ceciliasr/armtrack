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

load("BMDPC4.rda")
dd1<-ddT
dd1$LShPos_3<-110
dd1$RShPos_3<--110
dd1$Neck_2<- -129.10
dd1$Neck_3<- 0
dd1$Neck_4<-1480
dd1$Head_2<--129.10
dd1$Head_3<-0
dd1$Head_4<-1580
dd1$torso_2<--129.10
dd1$torso_3<-0
dd1$torso_4<-1200
dd1$waist_2<--129.10
dd1$waist_3<-0
dd1$waist_4<-1050

dd1$LThighPos_2<--129.1
dd1$LThighPos_3<-80
dd1$LThighPos_4<-900
dd1$RThighPos_2<--129.1
dd1$RThighPos_3<--80
dd1$RThighPos_4<-900

dd1$LKneePos_2<--129.1
dd1$LKneePos_3<-80
dd1$LKneePos_4<-650
dd1$RKneePos_2<--129.1
dd1$RKneePos_3<--80
dd1$RKneePos_4<-650

dd1$LAnklePos_2<--129.1
dd1$LAnklePos_3<-80
dd1$LAnklePos_4<-400
dd1$RAnklePos_2<--129.1
dd1$RAnklePos_3<--80
dd1$RAnklePos_4<-400

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
   RKneePos,RThighPos)
d2<-dd
#save(d1,file="c:/TFM/Data/BMDPC1_plot.rda")
# create a canvas 

theta<--90
phi<-0

canvas <- ggplot(dd[1:60,], aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
  theme_void()+ coord_equal()+ transition_time(Timestamp)


p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = theta, phi = phi)  + 
  stat_3D(data = dd[1:60,],mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = theta, phi = phi)+
  labs(title = " Timestamp (in sec.): {round(as.integer(frame_time)/1000)}")
animate(p_with_canv,2)
#<anim_save("c:/TFM/Results/HPC1_1aHora_500.gif")
