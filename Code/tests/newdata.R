#Installing package
install.packages("c:/TFM/Code/armtrack_0.0.0.9000.tar.gz", repos = NULL, type = "source")
library(armtrack)

load("c:/TFM/Data/HealthyD.rda")
load("c:/TFM/Data/BDistrophyD.rda")

if (!require("rmatio"))
{
  install.packages("rmatio")
  library(rmatio)
}
dd<-read.mat("C:/TFM/Data/ShoulderPositions.mat")
dd1<-read.mat("C:/TFM/Data/DadesVideoCecilia.mat")

