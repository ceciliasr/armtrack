#' at.read2 function
#'
#' This function allows you to read Arm Tracker Data in .mat format
#' @param data Needed in order to know the name (and path) of the mat data file you want to read
#' @keywords mat read
#' @export
#' @examples
#' at.read2()
#'
at.read2<-function(data)
{
  if (!require("rmatio"))
    {
      install.packages("rmatio")
      llibrary(rmatio)
  }
  dd<-read.mat(data)
  
  dd2<-data.frame(dd$Time)
  dd2<-data.frame(dd2,dd$Acc2,dd$Acc4,dd$AccC,dd$ang,dd$LElbAngle,dd$RElbAngle,dd$LSAngle,dd$RSAngle,dd$LHandPos[,1],dd$LHandPos[,2],dd$LHandPos[,3],
                  dd$LElbPos[,1],dd$LElbPos[,2],dd$LElbPos[,3],dd$LShPos[,1],dd$LShPos[,2],dd$LShPos[,3],
                  dd$RHandPos[,1],dd$RHandPos[,2],dd$RHandPos[,3],dd$RElbPos[,1],dd$RElbPos[,2],dd$RElbPos[,3],
                  dd$RShPos[,1],dd$RShPos[,2],dd$RShPos[,3])
  names(dd2)<-c("Timestamp","Acc2","Acc4","AccC","TorsoAngle","LElbAngle","RElbAngle","LSAngle","RSAngle","LHandPosX","LHandPosY","LHandPosZ",
                "LElbPosX","LElbPosY","LElbPosZ","LShPosX","LShPosY","LShPosZ","RHandPosX","RHandPosY","RHandPosZ",
                "RElbPosX","RElbPosY","RElbPosZ","RShPosX","RShPosY","RShPosZ")
  
  for(i in c(1:6,13:16))
  {
    for(j in 1:3)
    {
      n<-paste0("var_",i,"eje_",j,sep="")
      dd2[[n]]<-dd$Landmarks[j,i]
    }
  }
  names(dd2)[28:57]<-c("HeadPosX","HeadPosY","HeadPosZ","NeckPosX","NeckPosY","NeckPosZ","TorsoPosX","TorsoPosY","TorsoPosZ","WaistPosX","WaistPosY","WaistPosZ","RThighPosX","RThighPosY","RThighPosZ","LThighPosX","LThighPosY","LThighPosZ","RKneePosX","RKneePosY","RKneePosZ","LKneePosX","LKneePosY","LKneePosZ","RAnklePosX","RAnklePosY","RAnklePosZ","LAnklePosX","LAnklePosY","LAnklePosZ")
  return(dd2)
}

