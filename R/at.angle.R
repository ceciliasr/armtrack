#' at.angle function
#'
#' This function allows you to obtain the angle of a quaternion in radians and add this as a new
#' variable on the input dataset
#' @param data Input dataset where we can find the variable
#' @param var Variable of the dataset to treat. This variable must be the first component of the quaternion
#' because is the one obtaining the angle information.
#' @keywords quaternion angle
#' @export
#' @examples
#' at.read()
#'
at.angle<-function(data,var)
{
  data[which(data[,which(names(data)==var)]>1 & data[,which(names(data)==var)]<1.001),which(names(data)==var)]<-1
  j<-paste0(var,"_ang",sep="")
  data[[j]]<-2*acos(data[,which(names(data)==var)])
  return(data)
}
