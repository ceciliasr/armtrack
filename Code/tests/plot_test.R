setwd("c:/TFM/Data")

if (!require("rmatio"))
{
  install.packages("rmatio")
  library(rmatio)
}
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
if (!require("matrixStats"))
{
  install.packages("matrixStats")
  library(matrixStats)
}

load("BMDPC1_plot.rda")
d1$Timestamp<-d1$Timestamp/1000
dd<-d1[1:60,] #Primera hora de datos
rm(d1)
# create a canvas 

theta<-90
phi<--20

canvas <- ggplot(dd, aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
  theme_void()+ coord_equal()+ transition_time(Timestamp)+coord_fixed(ratio=1.5)


p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = theta, phi = phi)  + 
  stat_3D(data = dd,mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = theta, phi = phi)+
  labs(title = " Timestamp (in sec.): {as.integer(frame_time)}")
animate(p_with_canv,2)
anim_save("c:/TFM/Results/BMDPC6_1aHora_500.gif")
