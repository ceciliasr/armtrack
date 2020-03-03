#' at.read function
#'
#' This function allows you to read Arm Tracker Data in .mat format
#' @param data Needed in order to know the name (and path) of the mat data file you want to read
#' @keywords mat read
#' @export
#' @examples
#' at.read()
#'
at.read<-function(data)
{
  if (!require("rmatio")) install.packages("rmatio")
  dd<-read.mat(data)
  dd2<-data.frame(dd$TimeStamp)
  dd2<-data.frame(dd2,dd$wQic,dd$wQi1,dd$wQi2,dd$wQi3,dd$wQi4,dd$ang,dd$cq1,dd$cq3,dd$s1q2,dd$s3q4,dd$LElbAngle,
                  dd$RElbAngle,dd$LSAngle,dd$RSAngle,dd$RHandPos,dd$LHandPos,dd$RElbPos,dd$LElbPos)
  names(dd2)<-c("Timestamp","wQic_1","wQic_2","wQic_3","wQic_4","wQi1_1","wQi1_2","wQi1_3","wQi1_4","wQi2_1","wQi2_2",
                "wQi2_3","wQi2_4","wQi3_1","wQi3_2","wQi3_3","wQi3_4","wQi4_1","wQi4_2","wQi4_3","wQi4_4",
                "ang","cq1_1","cq1_2","cq1_3","cq1_4","cq3_1","cq3_2","cq3_3","cq3_4","s1q2_1","s1q2_2",
                "s1q2_3","s1q2_4","s3q4_1","s3q4_2","s3q4_3","s3q4_4","LElbAngle","RElbAngle","LSAngle",
                "RSAngle","RHandPos_1","RHandPos_2","RHandPos_3","RHandPos_4","LHandPos_1","LHandPos_2",
                "LHandPos_3","LHandPos_4","RElbPos_1","RElbPos_2","RElbPos_3","RElbPos_4","LElbPos_1","LElbPos_2",
                "LElbPos_3","LElbPos_4")
  
  dd2<-subset(dd2,select=-c(RHandPos_1))
  dd2<-subset(dd2,select=-c(LHandPos_1))
  dd2<-subset(dd2,select=-c(RElbPos_1))
  dd2<-subset(dd2,select=-c(LElbPos_1))
  return(dd2)
}
