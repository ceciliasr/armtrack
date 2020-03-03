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


###################HEALTHY#####################
load("HealthyD.rda")
dd1<-read.mat("DadesVideoCecilia.mat")
dd1<-data.frame(dd1$TimeStamp,dd1$ang,dd1$LElbAngle,dd1$RElbAngle,dd1$LSAngle,
                dd1$RSAngle,dd1$RHandPos[,1],dd1$RHandPos[,2],dd1$RHandPos[,3],
                dd1$LHandPos[,1],dd1$LHandPos[,2],dd1$LHandPos[,3],
                dd1$RElbPos[,1],dd1$RElbPos[,2],dd1$RElbPos[,3],
                dd1$LElbPos[,1],dd1$LElbPos[,2],dd1$LElbPos[,3])
names(dd1)<-names(HD)[1:18]
rm(HD)

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

se<-seq(0,nrow(dd1),10)
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

# create a canvas 
canvas <- ggplot(d1[347355:34735,], aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
  theme_void()+ coord_equal()+ transition_time(Timestamp)


p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = -85, phi = 0)  + 
  stat_3D(data = dd,mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = -85, phi = 0)
animate(p_with_canv,900)

anim_save("c:/TFM/Results/VideoCurt_900_cossencer.gif")
anim_save("c:/TFM/Results/VideoCurt_900_cossencer.mp4")

