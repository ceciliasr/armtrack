setwd("c:/TFM/Data")

if (!require("rmatio"))
{
  install.packages("rmatio")
  llibrary(rmatio)
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
if (!require("animation"))
{
  install.packages("animation")
  library(animation)
}
if (!require("gganimate"))
{
  install.packages("gganimate")
  library(gganimate)
}

library(`gg3D-master`)

install.packages("c:/TFM/Code/gg3D.tar.gz", repos = NULL, type = "source")
library("gg3D")

dd<-read.mat("ShoulderPositions.mat")
load("BDistrophyD.rda")

se<-seq(0,nrow(BMD),10)
BMD<-BMD[rownames(BMD) %in% se,]
rm(se)

BMD_LSh<-t(as.matrix(dd$BMDLSh))
BMD_LSh<-data.frame(matrix(BMD_LSh,nrow=nrow(BMD),ncol=ncol(BMD_LSh),byrow=TRUE))
names(BMD_LSh)<-c("LShPos_2","LShPos_3","LShPos_4")
BMD_RSh<-t(as.matrix(dd$BMDRSh))
BMD_RSh<-data.frame(matrix(BMD_RSh,nrow=nrow(BMD),ncol=ncol(BMD_RSh),byrow=TRUE))
names(BMD_RSh)<-c("RShPos_2","RShPos_3","RShPos_4")

BMD<-cbind.data.frame(BMD,BMD_LSh,BMD_RSh)
BMD<-subset(BMD,select=c(Timestamp,RHandPos_2,RHandPos_3,RHandPos_4,LHandPos_2,LHandPos_3,LHandPos_4,RElbPos_2,RElbPos_3,RElbPos_4,LElbPos_2,LElbPos_3,LElbPos_4,RShPos_2,RShPos_3,RShPos_4,LShPos_2,LShPos_3,LShPos_4))
rm(dd,BMD_LSh,BMD_RSh)

graf<-subset(BMD,select=c(Timestamp,LHandPos_2,RHandPos_2,LHandPos_4,RHandPos_4,LElbPos_2,RElbPos_2,LElbPos_4,RElbPos_4,LShPos_3,RShPos_3,LShPos_4,RShPos_4))

graf2<-graf
graf<-graf2[1:1500,]


p<-ggplot(graf, aes(x = LShPos_3, y = LShPos_4,frame=Timestamp))  +
  geom_point(data=graf, mapping=aes(x = RShPos_3, y = RShPos_4),shape = 21, colour = "black", fill = "white", size = 3)  +
  geom_point(shape = 21, colour = "black", fill = "white", size = 3)+ xlim(-300,300)+ylim(500,1500) +
  geom_point(mapping=aes(x = LElbPos_2, y = LElbPos_4),shape = 21, colour = "black", fill = "white", size = 3) +
  geom_point(mapping=aes(x = RElbPos_2, y = RElbPos_4),shape = 21, colour = "black", fill = "white", size = 3) +
  geom_point(mapping=aes(x = LHandPos_2, y = LHandPos_4),shape = 21, colour = "black", fill = "white", size = 3) +
  geom_point(mapping=aes(x = RHandPos_2, y = RHandPos_4),shape = 21, colour = "black", fill = "white", size = 3) +
  geom_segment(aes(x=LShPos_3, y=LShPos_4, xend=RShPos_3, yend=RShPos_4), colour="red",size=2)+
  geom_segment(aes(x=LShPos_3, y=LShPos_4, xend=LElbPos_2, yend=LElbPos_4), colour="red",size=2)+
  geom_segment(aes(x=RShPos_3, y=RShPos_4, xend=RElbPos_2, yend=RElbPos_4), colour="red",size=2)+
  geom_segment(aes(x=LElbPos_2, y=LElbPos_4, xend=LHandPos_2, yend=LHandPos_4), colour="red",size=2)+
  geom_segment(aes(x=RElbPos_2, y=RElbPos_4, xend=RHandPos_2, yend=RHandPos_4), colour="red",size=2)
ani.options(interval = 0.02) #animation speed, seconds per frame
gif<-gganimate(p,title_frame = F)
gganimate_save(gif,file="GIF_B1.gif")

p<-ggplot(graf, aes(x = LShPos_3, y = LShPos_4, z = LShPos_2,frame=Timestamp))  +
  axes_3D()+
  geom_point(shape = 21, colour = "black", fill = "white", size = 3)+ xlim(-300,300)+ylim(500,1500) +
  geom_point(data=graf, mapping=aes(x = RShPos_3, y = RShPos_4, z = RShPos_2),shape = 21, colour = "black", fill = "white", size = 3)  +
  geom_point(mapping=aes(x = LElbPos_2, y = LElbPos_4, z = LElbPos_3),shape = 21, colour = "black", fill = "white", size = 3) +
  geom_point(mapping=aes(x = RElbPos_2, y = RElbPos_4, z = RElbPos_3),shape = 21, colour = "black", fill = "white", size = 3) +
  geom_point(mapping=aes(x = LHandPos_2, y = LHandPos_4, z = LHandPos_3),shape = 21, colour = "black", fill = "white", size = 3) +
  geom_point(mapping=aes(x = RHandPos_2, y = RHandPos_4, z = RHandPos_3),shape = 21, colour = "black", fill = "white", size = 3) +
  geom_segment(aes(x=LShPos_3, y=LShPos_4, z = LShPos_2, xend=RShPos_3, yend=RShPos_4, zend=RShPos_2), colour="red",size=2)+
  geom_segment(aes(x=LShPos_3, y=LShPos_4, z = LShPos_2, xend=LElbPos_2, yend=LElbPos_4, zend=LElbPos_3), colour="red",size=2)+
  geom_segment(aes(x=RShPos_3, y=RShPos_4, z=RShPos_2, xend=RElbPos_2, yend=RElbPos_4, zend=RElbPos_3), colour="red",size=2)+
  geom_segment(aes(x=LElbPos_2, y=LElbPos_4, z=LElbPos_3, xend=LHandPos_2, yend=LHandPos_4, zend=LHandPos_3), colour="red",size=2)+
  geom_segment(aes(x=RElbPos_2, y=RElbPos_4, z=RElbPos_3, xend=RHandPos_2, yend=RHandPos_4, zend=RHandPos_3), colour="red",size=2)
ani.options(interval = 0.02) #animation speed, seconds per frame
gif<-gganimate(p,title_frame = F)
gganimate_save(gif,file="GIF_B2.gif")






###################HEALTHY#####################
dd<-read.mat("ShoulderPositions.mat")
load("HealthyD.rda")
se<-seq(0,nrow(HD),10)
HD<-HD[rownames(HD) %in% se,]
rm(se)


HD_LSh<-t(as.matrix(dd$HealthyLSh))
HD_LSh<-data.frame(matrix(HD_LSh,nrow=nrow(HD),ncol=ncol(HD_LSh),byrow=TRUE))
names(HD_LSh)<-c("LShPos_2","LShPos_3","LShPos_4")
HD_RSh<-t(as.matrix(dd$HealthyRSh))
HD_RSh<-data.frame(matrix(HD_RSh,nrow=nrow(HD),ncol=ncol(HD_RSh),byrow=TRUE))
names(HD_RSh)<-c("RShPos_2","RShPos_3","RShPos_4")

HD<-cbind.data.frame(HD,HD_LSh,HD_RSh)
HD<-subset(HD,select=c(Timestamp,LHandPos_2,RHandPos_2,LHandPos_3,RHandPos_3,LHandPos_4,RHandPos_4,LElbPos_2,RElbPos_2,LElbPos_3,RElbPos_3,LElbPos_4,RElbPos_4,LShPos_2,RShPos_2,LShPos_3,RShPos_3,LShPos_4,RShPos_4))
rm(dd,HD_LSh,HD_RSh)
graf2<-subset(HD,select=c(Timestamp,LHandPos_2,RHandPos_2,LHandPos_4,RHandPos_4,LElbPos_2,RElbPos_2,LElbPos_4,RElbPos_4,LShPos_3,RShPos_3,LShPos_4,RShPos_4))

graf<-graf2[1:300,]



















p<-ggplot(graf, aes(x = LShPos_3, y = LShPos_4,frame=Timestamp))  +
  geom_point(data=graf, mapping=aes(x = RShPos_3, y = RShPos_4),shape = 21, colour = "black", fill = "white", size = 3)  +
  geom_point(shape = 21, colour = "black", fill = "white", size = 3)+ xlim(-300,300)+ylim(500,1500) +
  geom_point(mapping=aes(x = LElbPos_2, y = LElbPos_4),shape = 21, colour = "black", fill = "white", size = 3) +
  geom_point(mapping=aes(x = RElbPos_2, y = RElbPos_4),shape = 21, colour = "black", fill = "white", size = 3) +
  geom_point(mapping=aes(x = LHandPos_2, y = LHandPos_4),shape = 21, colour = "black", fill = "white", size = 3) +
  geom_point(mapping=aes(x = RHandPos_2, y = RHandPos_4),shape = 21, colour = "black", fill = "white", size = 3) +
  geom_segment(aes(x=LShPos_3, y=LShPos_4, xend=RShPos_3, yend=RShPos_4), colour="red",size=2)+
  geom_segment(aes(x=LShPos_3, y=LShPos_4, xend=LElbPos_2, yend=LElbPos_4), colour="red",size=2)+
  geom_segment(aes(x=RShPos_3, y=RShPos_4, xend=RElbPos_2, yend=RElbPos_4), colour="red",size=2)+
  geom_segment(aes(x=LElbPos_2, y=LElbPos_4, xend=LHandPos_2, yend=LHandPos_4), colour="red",size=2)+
  geom_segment(aes(x=RElbPos_2, y=RElbPos_4, xend=RHandPos_2, yend=RHandPos_4), colour="red",size=2)
ani.options(interval = 0.02) #animation speed, seconds per frame
gif<-gganimate(p,title_frame = F)
gganimate_save(gif,file="GIF_H1.gif")




