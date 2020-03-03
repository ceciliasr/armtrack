#######Author: Cecilia Siliato#######
####### Creating an R package #######

setwd("c:/Users/cecilia.siliato/Desktop/CECI/Altres coses/MESIO/Master Thesis")
rm(list=ls())

install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)

create("armtrack")

setwd("./Code/armtrack")
document()

