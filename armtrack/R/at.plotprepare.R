#' at.plotprepare function
#'
#' This function prepares the Arm Tracker database to perform a 3D ggplot.
#' @param data Needed in order to know the dataframe file to prepare
#' @param freq The frequency (Hz) that we need on data. Default: 2Hz
#' @keywords plot prepare
#' @export
#' @examples
#' at.plotprepare()
#'

at.plotprepare<-function(data,freq=2){
  sec<-50/freq
  se<-seq(0,nrow(data),sec)
  data<-data[rownames(data) %in% se,]
  rm(se)
  
  
  LHand<-subset(data,select=c(Timestamp,LHandPosX,LHandPosY,LHandPosZ))
  names(LHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  LElb<-subset(data,select=c(Timestamp,LElbPosX,LElbPosY,LElbPosZ))
  names(LElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  LSh<-subset(data,select=c(Timestamp,LShPosX,LShPosY,LShPosZ))
  names(LSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  Neck<-subset(data,select=c(Timestamp,NeckPosX,NeckPosY,NeckPosZ))
  names(Neck)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  Head<-subset(data,select=c(Timestamp,HeadPosX,HeadPosY,HeadPosZ))
  names(Head)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  RSh<-subset(data,select=c(Timestamp,RShPosX,RShPosY,RShPosZ))
  names(RSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  RElb<-subset(data,select=c(Timestamp,RElbPosX,RElbPosY,RElbPosZ))
  names(RElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  RHand<-subset(data,select=c(Timestamp,RHandPosX,RHandPosY,RHandPosZ))
  names(RHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  
  torso<-subset(data,select=c(Timestamp,TorsoPosX,TorsoPosY,TorsoPosZ))
  names(torso)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  waist<-subset(data,select=c(Timestamp,WaistPosX,WaistPosY,WaistPosZ))
  names(waist)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  LThighPos<-subset(data,select=c(Timestamp,LThighPosX,LThighPosY,LThighPosZ))
  names(LThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  RThighPos<-subset(data,select=c(Timestamp,RThighPosX,RThighPosY,RThighPosZ))
  names(RThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  LKneePos<-subset(data,select=c(Timestamp,LKneePosX,LKneePosY,LKneePosZ))
  names(LKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  RKneePos<-subset(data,select=c(Timestamp,RKneePosX,RKneePosY,RKneePosZ))
  names(RKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  LAnklePos<-subset(data,select=c(Timestamp,LAnklePosX,LAnklePosY,LAnklePosZ))
  names(LAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  RAnklePos<-subset(data,select=c(Timestamp,RAnklePosX,RAnklePosY,RAnklePosZ))
  names(RAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
  
  dd<-rbind.data.frame(Head,Neck,LSh,LElb,LHand,LElb,LSh,Neck,RSh,RElb,RHand,RElb,RSh, Neck, torso, waist,
                       LThighPos,LKneePos,LAnklePos,LKneePos,LThighPos,waist,RThighPos,RKneePos,RAnklePos,
                       RKneePos,RThighPos,waist,torso,Neck)
  
  dd<-dd[order(dd$Timestamp),]
  return(dd)
}
