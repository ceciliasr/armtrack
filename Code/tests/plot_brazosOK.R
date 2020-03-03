setwd("c:/TFM/Data")

if (!require("rmatio"))
{
  install.packages("rmatio")
  library(rmatio)
}
if (!require("gganimate"))
{
  install.packages("C:/TFM/Code/Packages/gganimate-0.1.1.tar.gz", repos = NULL, type = "source")
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
dd1$Head_4<-1550

dd1<-subset(dd1,select=c(Timestamp,LHandPos_2,RHandPos_2,LHandPos_3,RHandPos_3,LHandPos_4,RHandPos_4,LElbPos_2,RElbPos_2,LElbPos_3,RElbPos_3,LElbPos_4,RElbPos_4,LShPos_2,RShPos_2,LShPos_3,RShPos_3,LShPos_4,RShPos_4,Head_2,Head_3,Head_4))
rm(dd,LSh,RSh,p,R)

LHand<-subset(dd1,select=c(Timestamp,LHandPos_2,LHandPos_3,LHandPos_4))
LHand$color<-"1"
names(LHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z","color")
LElb<-subset(dd1,select=c(Timestamp,LElbPos_2,LElbPos_3,LElbPos_4))
LElb$color<-"2"
names(LElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z","color")
LSh<-subset(dd1,select=c(Timestamp,LShPos_2,LShPos_3,LShPos_4))
LSh$color<-"3"
names(LSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z","color")
Head<-subset(dd1,select=c(Timestamp,Head_2,Head_3,Head_4))
Head$color<-"4"
names(Head)<-c("Timestamp","Pos_x","Pos_y","Pos_z","color")
RSh<-subset(dd1,select=c(Timestamp,RShPos_2,RShPos_3,RShPos_4))
RSh$color<-"3"
names(RSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z","color")
RElb<-subset(dd1,select=c(Timestamp,RElbPos_2,RElbPos_3,RElbPos_4))
RElb$color<-"2"
names(RElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z","color")
RHand<-subset(dd1,select=c(Timestamp,RHandPos_2,RHandPos_3,RHandPos_4))
RHand$color<-"1"
names(RHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z","color")

dd<-rbind.data.frame(LHand,LElb,LSh,Head,RSh,RElb,RHand)
dd<-dd[order(dd$Timestamp),]

rm(LHand,LElb,LSh,RSh,RElb,RHand,Head)

p <- ggplot(dd[1:70,], aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
  theme_void()+ stat_3D(geom = "point",size=5,theta = -60, phi = 10)  + 
  stat_3D(data = dd[1:70,],mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = -60, phi = 10)+
  coord_equal()+  transition_time(Timestamp)
p2<-animate(p, nframes = 2500, fps = 20)
anim_save("anim_gg3D_VideoCorto.gif",animation = p2)

