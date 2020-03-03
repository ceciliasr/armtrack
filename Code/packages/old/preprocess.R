######PREPROCESS OF DATA######

#Installing package
install.packages("C:/Users/cecilia.siliato/Desktop/CECI/Altres coses/MESIO/Master Thesis/Code/armtrack_0.0.0.9000.tar.gz", repos = NULL, type = "source")
library(armtrack)

#Reading data
Hdata<-"c:/Users/cecilia.siliato/Desktop/CECI/Altres coses/MESIO/Master Thesis/Data/HealthyData.mat"
Hdata<-at.read(Hdata)

#Imputing missing values
Hd_nomis<-at.missing(Hdata)

#Variables validation:
summary(Hd_nomis)
#QUATERNIONS:
pdf("c:/temp/Histograms.pdf")
for (i in seq(2,21,4)){
    par(mfrow=c(2,2))
  hist(Hd_nomis[[i]],main=names(Hd_nomis)[i],xlab="",freq=F,xlim=c(-1,1))
  hist(Hd_nomis[[i+1]],main=names(Hd_nomis)[i+1],xlab="",freq=F,xlim=c(-1,1))
  hist(Hd_nomis[[i+2]],main=names(Hd_nomis)[i+2],xlab="",freq=F,xlim=c(-1,1))
  hist(Hd_nomis[[i+3]],main=names(Hd_nomis)[i+3],xlab="",freq=F,xlim=c(-1,1))
  if (substr(names(Hd_nomis)[i],4,4)=="c"){
    title("TORSO - world o.", line=-2,outer=T)
  }
  if (substr(names(Hd_nomis)[i],4,4)=="1"){
    title("RIGHT UPPER ARM - world o.", line=-1.5,outer=T)
  }
  if (substr(names(Hd_nomis)[i],4,4)=="2"){
    title("RIGHT FOREARM - world o.", line=-1.5,outer=T)
  }
  if (substr(names(Hd_nomis)[i],4,4)=="3"){
    title("LEFT UPPER ARM - world o.", line=-1.5,outer=T)
  }
  if (substr(names(Hd_nomis)[i],4,4)=="4"){
    title("LEFT FOREARM - world o.", line=-1.5,outer=T)
  }
}

for (i in seq(23,38,4)){
    par(mfrow=c(2,2))
  hist(Hd_nomis[[i]],main=names(Hd_nomis)[i],xlab="",freq=F,xlim=c(-1,1))
  hist(Hd_nomis[[i+1]],main=names(Hd_nomis)[i+1],xlab="",freq=F,xlim=c(-1,1))
  hist(Hd_nomis[[i+2]],main=names(Hd_nomis)[i+2],xlab="",freq=F,xlim=c(-1,1))
  hist(Hd_nomis[[i+3]],main=names(Hd_nomis)[i+3],xlab="",freq=F,xlim=c(-1,1))
  if (substr(names(Hd_nomis)[i],3,3)=="1"){
    title("RIGHT UPPER ARM - TORSO o.", line=-1.5,outer=T)
  }
  if (substr(names(Hd_nomis)[i],3,3)=="3"){
    title("LEFT UPPER ARM - TORSO o.", line=-1.5,outer=T)
  }
  if (substr(names(Hd_nomis)[i],4,4)=="2"){
    title("RIGHT FOREARM - RIGHT UPPER ARM o.", line=-1.5,outer=T)
  }
  if (substr(names(Hd_nomis)[i],4,4)=="4"){
    title("LEFT FOREARM - LEFT UPPER ARM o.", line=-1.5,outer=T)
  }
}
dev.off()

#http://www.tobynorris.com/work/prog/csharp/quatview/help/orientations_and_quaternions.htm
#Angle of quaternions:
#The first component of all the quaternions is the real component q_0 and we can obtain
#the rotation angle following the formula:
#\theta=2acos(q_0)

nrow(Hd_nomis[which(Hd_nomis$wQic_1>1),])
nrow(Hd_nomis[which(Hd_nomis$wQi1_1>1),])
nrow(Hd_nomis[which(Hd_nomis$wQi2_1>1),])
nrow(Hd_nomis[which(Hd_nomis$wQi3_1>1),])
nrow(Hd_nomis[which(Hd_nomis$wQi4_1>1),])
nrow(Hd_nomis[which(Hd_nomis$cq1_1>1 & Hd_nomis$cq1_1<1.001 ),])
nrow(Hd_nomis[which(Hd_nomis$cq3_1>1 & Hd_nomis$cq3_1<1.001),])
nrow(Hd_nomis[which(Hd_nomis$s1q2_1>1 & Hd_nomis$s1q2_1<1.001),])
nrow(Hd_nomis[which(Hd_nomis$s3q4_1>1 & Hd_nomis$s3q4_1<1.001),])

Hd_nomis$cq1_1[which(Hd_nomis$cq1_1>1 & Hd_nomis$cq1_1<1.001 )]<-1
Hd_nomis$cq3_1[which(Hd_nomis$cq3_1>1 & Hd_nomis$cq3_1<1.001 )]<-1
Hd_nomis$s1q2_1[which(Hd_nomis$s1q2_1>1 & Hd_nomis$s1q2_1<1.001 )]<-1
Hd_nomis$s3q4_1[which(Hd_nomis$s3q4_1>1 & Hd_nomis$s3q4_1<1.001 )]<-1

for (i in 1:ncol(Hd_nomis))
{
  j<-paste0(names(Hd_nomis)[i],"_ang",sep="")
  if(substr(names(Hd_nomis)[i],nchar(names(Hd_nomis)[i])-1,nchar(names(Hd_nomis)[i]))=="_1")
  {
        Hd_nomis[[j]]<-2*acos(Hd_nomis[[i]])
  }
}

for (i in seq(55,63,9))
{
  #if (i!=63){
    par(mfrow=c(3,3))
    hist(Hd_nomis[[i]],main=names(Hd_nomis)[i],xlab="",freq=F)
    hist(Hd_nomis[[i+1]],main=names(Hd_nomis)[i+1],xlab="",freq=F)
    hist(Hd_nomis[[i+2]],main=names(Hd_nomis)[i+2],xlab="",freq=F)
    hist(Hd_nomis[[i+3]],main=names(Hd_nomis)[i+3],xlab="",freq=F)
    #par(mfrow=c(1,1))
    hist(Hd_nomis[[i+4]],main=names(Hd_nomis)[i+4],xlab="",freq=F)
    hist(Hd_nomis[[i+5]],main=names(Hd_nomis)[i+5],xlab="",freq=F)
    hist(Hd_nomis[[i+6]],main=names(Hd_nomis)[i+6],xlab="",freq=F)
    hist(Hd_nomis[[i+7]],main=names(Hd_nomis)[i+7],xlab="",freq=F)
    hist(Hd_nomis[[i+8]],main=names(Hd_nomis)[i+8],xlab="",freq=F)
    #}

}
