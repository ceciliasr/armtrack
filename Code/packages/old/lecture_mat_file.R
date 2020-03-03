#####################################
#Author: Cecilia Siliato Robles######
#Project: Final Project CSR##########
#Objective: Lecture of .mat file#####
#####################################

#Working directory:
setwd("c:/Users/cecil/OneDrive/Escritorio/TFM/Code")
#setwd("c:/Users/cecilia.siliato/Desktop/CECI/Altres coses/MESIO/Master Thesis/Data")

#Needed packages:
#Reading .mat files:
install.packages("rmatio")
library("rmatio")

#Reading Data from healthy subject:
HData<-read.mat("HealthyData.mat")

#Reading Data from unhealthy subject:
BMDData<-read.mat("BMDData.mat")

#1139095
HData2<-data.frame(HData$TimeStamp)
HData2<-data.frame(HData2,HData$wQic,HData$wQi1,HData$wQi2,HData$wQi3,HData$wQi4,HData$ang,HData$cq1,HData$cq3,HData$s1q2,HData$s3q4,HData$LElbAngle,
                   HData$RElbAngle,HData$LSAngle,HData$RSAngle,HData$RHandPos,HData$LHandPos,HData$RElbPos,HData$LElbPos)

names(HData2)<-c("Timestamp","wQic_1","wQic_2","wQic_3","wQic_4","wQi1_1","wQi1_2","wQi1_3","wQi1_4","wQi2_1","wQi2_2",
                 "wQi2_3","wQi2_4","wQi3_1","wQi3_2","wQi3_3","wQi3_4","wQi4_1","wQi4_2","wQi4_3","wQi4_4",
                 "ang","cq1_1","cq1_2","cq1_3","cq1_4","cq3_1","cq3_2","cq3_3","cq3_4","s1q2_1","s1q2_2",
                 "s1q2_3","s1q2_4","s3q4_1","s3q4_2","s3q4_3","s3q4_4","LElbAngle","RElbAngle","LSAngle",
                 "RSAngle","RHandPos_1","RHandPos_2","RHandPos_3","RHandPos_4","LHandPos_1","LHandPos_2",
                 "LHandPos_3","LHandPos_4","RElbPos_1","RElbPos_2","RElbPos_3","RElbPos_4","LElbPos_1","LElbPos_2",
                 "LElbPos_3","LElbPos_4")
HData2<-subset(HData2,select=-c(RHandPos_1))
HData2<-subset(HData2,select=-c(LHandPos_1))
HData2<-subset(HData2,select=-c(RElbPos_1))
HData2<-subset(HData2,select=-c(LElbPos_1))

BMDData2<-data.frame(BMDData$TimeStamp)
BMDData2<-data.frame(BMDData2,BMDData$wQic,BMDData$wQi1,BMDData$wQi2,BMDData$wQi3,BMDData$wQi4,BMDData$ang,BMDData$cq1,BMDData$cq3,BMDData$s1q2,BMDData$s3q4,BMDData$LElbAngle,
                   BMDData$RElbAngle,BMDData$LSAngle,BMDData$RSAngle,BMDData$RHandPos,BMDData$LHandPos,BMDData$RElbPos,BMDData$LElbPos)

names(BMDData2)<-c("Timestamp","wQic_1","wQic_2","wQic_3","wQic_4","wQi1_1","wQi1_2","wQi1_3","wQi1_4","wQi2_1","wQi2_2",
                 "wQi2_3","wQi2_4","wQi3_1","wQi3_2","wQi3_3","wQi3_4","wQi4_1","wQi4_2","wQi4_3","wQi4_4",
                 "ang","cq1_1","cq1_2","cq1_3","cq1_4","cq3_1","cq3_2","cq3_3","cq3_4","s1q2_1","s1q2_2",
                 "s1q2_3","s1q2_4","s3q4_1","s3q4_2","s3q4_3","s3q4_4","LElbAngle","RElbAngle","LSAngle",
                 "RSAngle","RHandPos_1","RHandPos_2","RHandPos_3","RHandPos_4","LHandPos_1","LHandPos_2",
                 "LHandPos_3","LHandPos_4","RElbPos_1","RElbPos_2","RElbPos_3","RElbPos_4","LElbPos_1","LElbPos_2",
                 "LElbPos_3","LElbPos_4")

BMDData2<-subset(BMDData2,select=-c(RHandPos_1))
BMDData2<-subset(BMDData2,select=-c(LHandPos_1))
BMDData2<-subset(BMDData2,select=-c(RElbPos_1))
BMDData2<-subset(BMDData2,select=-c(LElbPos_1))


#Timestamp study:
n<-nrow(HData2)-1
j<-0
for(i in 1:n)
{
  if(HData2$Timestamp[i+1]-HData2$Timestamp[i]>20)
  {
    j<-j+1
  }
}
cat("Hay",j,"timestamps con diferencia de + de 20ms")

n<-nrow(BMDData2)-1
j<-0
for(i in 1:n)
{
  if(BMDData2$Timestamp[i+1]-BMDData2$Timestamp[i]>20)
  {
    j<-j+1
  }
}
cat("Hay",j,"timestamps con diferencia de + de 20ms en Unhealthy Data")


install.packages("lattice")
library(lattice)
xyplot(HData2$wQic_1 ~ HData2$Timestamp, type = c("l", "p"))
xyplot(BMDData2$wQic_1 ~BMDData2$Timestamp , type = c("l", "p"), add=T,col="red")


#Packages for orientations:
install.packages("orientlib")
library(orientlib)
par(mfrow=c(3,4))
for (i in 1:12)
{
  j<-i*500
  x <- quaternion(c(HData2$wQic_1[j],HData2$wQic_2[j],HData2$wQic_3[j],HData2$wQic_4[j]))
  x
  rotmatrix(x)
  boat3d(x, k = 1:length(x), y = 0,
         z = 0, col = 'red',  axes = TRUE,
         graphics = c( 'scatterplot3d'),main=paste0("Timestamp:",HData2$Timestamp[j]),cex.main=3.5)
}

par(mfrow=c(3,4))
for (i in 1:12)
{
  j<-i*500
  x <- quaternion(c(BMDData2$wQic_1[j],BMDData2$wQic_2[j],BMDData2$wQic_3[j],BMDData2$wQic_4[j]))
  x
  rotmatrix(x)
  boat3d(x, k = 1:length(x), y = 0,
         z = 0, col = 'red',  axes = TRUE,
         graphics = c( 'scatterplot3d'),main=paste0("Timestamp:",BMDData2$Timestamp[j]),cex.main=3.5)
}


#Package for rotations:
install.packages("rotations")
library(rotations)
as.Q4(c(HData2$wQic_1[i],HData2$wQic_2[i],HData2$wQic_3[i],HData2$wQic_4[i]))


install.packages("plot3D")
library(plot3D)
par(mfrow=c(1,2))
scatter3D(BMDData2$RElbPos_2, BMDData2$RElbPos_3, BMDData2$RElbPos_4,bty = "g", main="Unhealthy",cex.main=2)
scatter3D(HData2$RElbPos_2, HData2$RElbPos_3, HData2$RElbPos_4, bty = "g", main="Healthy",cex.main=2)

par(mfrow=c(1,2))
scatter3D(BMDData2$RHandPos_2, BMDData2$RHandPos_3, BMDData2$RHandPos_4,bty = "g", main="Unhealthy",cex.main=2)
scatter3D(HData2$RHandPos_2, HData2$RHandPos_3, HData2$RHandPos_4, bty = "g", main="Healthy",cex.main=2)

install.packages("car")
library(car)

scatter3d(BMDData2$RHandPos_2, BMDData2$RHandPos_3, BMDData2$RHandPos_4, col=c(1,2,3))
scatter3d(HData2$RHandPos_2, HData2$RHandPos_3, HData2$RHandPos_4, col=c(1,2,3), surface=FALSE,point.col="blue")

#Angles
summary(HData2$LSAngle)
summary(HData2$RSAngle)
summary(HData2$LElbAngle)
summary(HData2$RElbAngle)



