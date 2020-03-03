#' at.skeletonplot function
#'
#' This function allows you to plot the skeleton made with Arm Tracker Data
#' @param data Needed in order to know the name of the data to plot (output of at.plotprepare)
#' @keywords mat read
#' @export
#' @examples
#' at.skeletonplot()
#'

at.skeletonplot<-function(data)
{
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
  canvas <- ggplot(data, aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
    theme_void()+ coord_equal()+ transition_time(Timestamp)+coord_fixed(ratio=1)
  
  
  p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = 60, phi = 0)  + 
    stat_3D(data = data,mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = 60, phi = 0)+
    labs(title = " Timestamp (in sec.): {round(as.integer(frame_time))}")
  p<-ifelse(round(as.integer(max(data$Timestamp)-min(data$Timestamp))*1.5)==0,2,round(as.integer(max(data$Timestamp)-min(data$Timestamp))*1.5))
  pT<-animate(p_with_canv,p)
  return(pT)
  #anim_save("c:/TFM/Results/HDT_347355_455355_1100.gif",pT)
}

