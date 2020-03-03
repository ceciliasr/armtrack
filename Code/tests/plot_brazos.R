setwd("c:/TFM/Data")

if (!require("gg3D"))
{
  install.packages("C:/temp/gg3D_0.0.0.9000.tar.gz", repos = NULL, type = "source")
  library(gg3D)
}

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

dd<-read.mat("ShoulderPositions.mat")
load("HealthyVideo.rda")
#Hem de fer una correcció ja que està tot rotat. Hem de rotar els punts 180º.
#Per a fer-ho, multipliquem R*p sent R:
#[cos(180) -sin(180) 0
#sin(180) cos(180) 0
#0 0 1]
#[x
#y
#z]
R<-t(matrix(c(-1,0,0,0,-1,0,0,0,1),nrow=3))
p<-matrix(c(dd$HealthyLSh[1],dd$HealthyLSh[2],dd$HealthyLSh[3]),nrow=3)
dd1$LShPos_2<-(R%*%p)[1]
dd1$LShPos_3<-(R%*%p)[2]
dd1$LShPos_4<-(R%*%p)[3]
p<-matrix(c(dd$HealthyRSh[1],dd$HealthyRSh[2],dd$HealthyRSh[3]),nrow=3)
dd1$RShPos_2<-(R%*%p)[1]
dd1$RShPos_3<-(R%*%p)[2]
dd1$RShPos_4<-(R%*%p)[3]


dd1<-subset(dd1,select=c(Timestamp,LHandPos_2,RHandPos_2,LHandPos_3,RHandPos_3,LHandPos_4,RHandPos_4,LElbPos_2,RElbPos_2,LElbPos_3,RElbPos_3,LElbPos_4,RElbPos_4,LShPos_2,RShPos_2,LShPos_3,RShPos_3,LShPos_4,RShPos_4))
rm(dd,p,R)

head(dd1[c(5000:11000),c(2,8,14)])
head(dd1[c(5000:11000),c(3,9,15)])
head(dd1[c(5000:11000),c(4,10,16)])
head(dd1[c(5000:11000),c(5,11,17)])

graf<-dd1[5000:7999,]

p<-ggplot(graf, aes(x = LShPos_2, y = LShPos_3, z = LShPos_4,frame=Timestamp))  + theme_void()+
  stat_3D()+axes_3D()+axis_labs_3D()+stat_3D(data=graf,mapping=aes(x = RShPos_2, y = RShPos_3, z = RShPos_4),geom="point")+
  xlim(-700,400)+ylim(-700,800)+zlim(500,1800)+
  geom_point(data=graf, mapping=aes(x = RShPos_2, y = RShPos_3, z = RShPos_4),shape = 21, colour = "black", fill = "white", size = 3)  +
  geom_point(mapping=aes(x = LElbPos_2, y = LElbPos_3, z = LElbPos_4),shape = 21, colour = "black", fill = "white", size = 3) +
  geom_point(mapping=aes(x = RElbPos_2, y = RElbPos_3, z = RElbPos_4),shape = 21, colour = "black", fill = "white", size = 3) +
  geom_point(mapping=aes(x = LHandPos_2, y = LHandPos_3, z = LHandPos_4),shape = 21, colour = "black", fill = "white", size = 3) +
  geom_point(mapping=aes(x = RHandPos_2, y = RHandPos_3, z = RHandPos_4),shape = 21, colour = "black", fill = "white", size = 3) 

install.packages("scatterplot3d")
library(scatterplot3d)
spl <- scatterplot3d(graf$LShPos_2, graf$LShPos_3, graf$LShPos_4, pch=20, type="o",xlim=c(-700,400),ylim=c(-700,800),zlim=c(500,1800))
spl$points3d(graf$RShPos_2, graf$RShPos_3, graf$RShPos_4, pch=20,type='o')


