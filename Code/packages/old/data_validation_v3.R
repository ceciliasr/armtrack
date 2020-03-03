#Installing package
install.packages("c:/Users/cecilia.siliato/Desktop/CECI/Altres coses/MESIO/TFM/Code/armtrack_0.0.0.9000.tar.gz", repos = NULL, type = "source")
library(armtrack)
# 1. Reading Data
Hdata<-"c:/Users/cecilia.siliato/Desktop/CECI/Altres coses/MESIO/TFM/Data/HealthyData.mat"
Hdata<-at.read(Hdata)
Bdata<-"c:/Users/cecilia.siliato/Desktop/CECI/Altres coses/MESIO/TFM/Data/BMDData.mat"
Bdata<-at.read(Bdata)
# 2. Missing data
#Imputing missing values
Hd_nomis<-at.missing(Hdata)
Bd_nomis<-at.missing(Bdata)
Hd_nomis$Type<-"Healthy"
Bd_nomis$Type<-"BMDistrophy"
dd<-rbind.data.frame(Hd_nomis,Bd_nomis)
# 3. Univariant analysis: Validation of each variable
HD<-subset(Hd_nomis, select=c("Timestamp","ang","LElbAngle","RElbAngle","LSAngle","RSAngle","RHandPos_2","RHandPos_3","RHandPos_4","LHandPos_2","LHandPos_3","LHandPos_4","RElbPos_2","RElbPos_3","RElbPos_4","LElbPos_2","LElbPos_3","LElbPos_4","Type"))
BMD<-subset(Bd_nomis, select=c("Timestamp","ang","LElbAngle","RElbAngle","LSAngle","RSAngle","RHandPos_2","RHandPos_3","RHandPos_4","LHandPos_2","LHandPos_3","LHandPos_4","RElbPos_2","RElbPos_3","RElbPos_4","LElbPos_2","LElbPos_3","LElbPos_4","Type"))
TD<-subset(dd, select=c("Timestamp","ang","LElbAngle","RElbAngle","LSAngle","RSAngle","RHandPos_2","RHandPos_3","RHandPos_4","LHandPos_2","LHandPos_3","LHandPos_4","RElbPos_2","RElbPos_3","RElbPos_4","LElbPos_2","LElbPos_3","LElbPos_4","Type"))
if (!require("ggplot2"))
  {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("gridExtra"))
{
  install.packages("gridExtra")
  library(gridExtra)
}
if (!require("grid"))
{
  install.packages("grid")
  library(grid)
}
if(!require("plotly"))
{
  install.packages("plotly")
  library(plotly)
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

p1<-ggplotly(TD, aes(LElbAngle, color=Type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', fill = "white") + scale_x_continuous(name ="Left")+scale_y_continuous(name="")+theme(legend.position = "top")+ylim(0,0.035)
p11<-p1+theme(legend.position = "none")
p2<-ggplotly(TD, aes(RElbAngle, color=Type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', fill = "white") + scale_x_continuous(name ="Right")+scale_y_continuous(name="")+ theme(legend.position="none")+ylim(0,0.035)

legend<-get_legend(p1)
grid.arrange(p11, p2, legend, nrow=2, widths=c(2.3, 2.3),
             layout_matrix = rbind(c(1,2), c(3,3)),heights=c(2.1,0.5),
p1<-ggplotly(TD, aes(LSAngle, color=Type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', fill = "white") + scale_x_continuous(name ="Left")+scale_y_continuous(name="")+theme(legend.position = "top")+ylim(0,0.035)
p11<-p1+theme(legend.position = "none")
p2<-ggplotly(TD, aes(RSAngle, color=Type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', fill = "white") + scale_x_continuous(name ="Right")+scale_y_continuous(name="")+ theme(legend.position="none")+ylim(0,0.035)

legend<-get_legend(p1)
grid.arrange(p11, p2, legend, nrow=2, widths=c(2.3, 2.3),
             layout_matrix = rbind(c(1,2), c(3,3)),heights=c(2.1,0.5),
             top = textGrob("Upper arm and body angle",gp=gpar(fontsize=20,font=1)))

se<-seq(0,nrow(HD),10)
HD1<-HD[rownames(HD) %in% se,]
se<-seq(0,nrow(BMD),10)
BMD1<-BMD[rownames(BMD) %in% se,]
se<-seq(0,nrow(TD),10)
TD1<-TD[rownames(TD) %in% se,]
rm(se)

#Plano XY:
par(mfrow=c(2,3))
## Use densCols() output to get density at each point
x <- densCols(BMD1$LElbPos_2,BMD1$LElbPos_3, colramp=colorRampPalette(c("black", "white")))
BMD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
BMD1$col <- cols[BMD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(LElbPos_3~LElbPos_2, data=BMD1[order(BMD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="y", main="BMDistrophy: XY",xlim=c(-300,150),ylim=c(0,450))
#Plano YZ:
## Use densCols() output to get density at each point
x <- densCols(BMD1$LElbPos_3,BMD1$LElbPos_4, colramp=colorRampPalette(c("black", "white")))
BMD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
BMD1$col <- cols[BMD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(LElbPos_4~LElbPos_3, data=BMD1[order(BMD1$dens),], pch=20, col=col, cex=2, xlab="y", ylab="z", main="BMDistrophy: YZ",xlim=c(0,450),ylim=c(1000,1650))
#Plano XZ:
## Use densCols() output to get density at each point
x <- densCols(BMD1$LElbPos_2,BMD1$LElbPos_4, colramp=colorRampPalette(c("black", "white")))
BMD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
BMD1$col <- cols[BMD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(LElbPos_4~LElbPos_2, data=BMD1[order(BMD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="z", main="BMDistrophy: XZ",xlim=c(-300,150),ylim=c(1000,1650))
BMD1 <- subset(BMD1,select=-c(col,dens))

#Plano XY
## Use densCols() output to get density at each point
x <- densCols(HD1$LElbPos_2,HD1$LElbPos_3, colramp=colorRampPalette(c("black", "white")))
HD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
HD1$col <- cols[HD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(LElbPos_3~LElbPos_2, data=HD1[order(HD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="y", main="Healthy: XY",xlim=c(-300,150),ylim=c(0,450))
#Plano YZ:
## Use densCols() output to get density at each point
x <- densCols(HD1$LElbPos_3,HD1$LElbPos_4, colramp=colorRampPalette(c("black", "white")))
HD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
HD1$col <- cols[HD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(LElbPos_4~LElbPos_3, data=HD1[order(HD1$dens),], pch=20, col=col, cex=2, xlab="y", ylab="z", main="Healthy: YZ",xlim=c(0,450),ylim=c(1000,1650))
#Plano XZ:
## Use densCols() output to get density at each point
x <- densCols(HD1$LElbPos_2,HD1$LElbPos_4, colramp=colorRampPalette(c("black", "white")))
HD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
HD1$col <- cols[HD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(LElbPos_4~LElbPos_2, data=HD1[order(HD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="z", main="Healthy: XZ",xlim=c(-300,150),ylim=c(1000,1650))
HD1 <- subset(HD1,select=-c(dens,col))
title("Left Elbow position", line=-1.3,outer=T,cex.main=1.5)

#Plano XY:
par(mfrow=c(2,3))
## Use densCols() output to get density at each point
x <- densCols(BMD1$RElbPos_2,BMD1$RElbPos_3, colramp=colorRampPalette(c("black", "white")))
BMD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
BMD1$col <- cols[BMD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(RElbPos_3~RElbPos_2, data=BMD1[order(BMD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="y", main="BMDistrophy: XY",xlim=c(-400,200),ylim=c(-450,50))
#Plano YZ:
## Use densCols() output to get density at each point
x <- densCols(BMD1$RElbPos_3,BMD1$RElbPos_4, colramp=colorRampPalette(c("black", "white")))
BMD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
BMD1$col <- cols[BMD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(RElbPos_4~RElbPos_3, data=BMD1[order(BMD1$dens),], pch=20, col=col, cex=2, xlab="y", ylab="z", main="BMDistrophy: YZ",xlim=c(-400,70),ylim=c(1000,1650))
#Plano XZ:
## Use densCols() output to get density at each point
x <- densCols(BMD1$RElbPos_2,BMD1$RElbPos_4, colramp=colorRampPalette(c("black", "white")))
BMD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
BMD1$col <- cols[BMD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(RElbPos_4~RElbPos_2, data=BMD1[order(BMD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="z", main="BMDistrophy: XZ",xlim=c(-400,200),ylim=c(1000,1650))
BMD1 <- subset(BMD1,select=-c(col,dens))


## Use densCols() output to get density at each point
x <- densCols(HD1$RElbPos_2,HD1$RElbPos_3, colramp=colorRampPalette(c("black", "white")))
HD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
HD1$col <- cols[HD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(RElbPos_3~RElbPos_2, data=HD1[order(HD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="y", main="Healthy: XY",xlim=c(-400,200),ylim=c(-450,50))
#Plano YZ:
## Use densCols() output to get density at each point
x <- densCols(HD1$RElbPos_3,HD1$RElbPos_4, colramp=colorRampPalette(c("black", "white")))
HD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
HD1$col <- cols[HD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(RElbPos_4~RElbPos_3, data=HD1[order(HD1$dens),], pch=20, col=col, cex=2, xlab="y", ylab="z", main="Healthy: YZ",xlim=c(-400,70),ylim=c(1000,1650))
#Plano XZ:
## Use densCols() output to get density at each point
x <- densCols(HD1$RElbPos_2,HD1$RElbPos_4, colramp=colorRampPalette(c("black", "white")))
HD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
HD1$col <- cols[HD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(RElbPos_4~RElbPos_2, data=HD1[order(HD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="z", main="Healthy: XZ",xlim=c(-400,200),ylim=c(1000,1650))
HD1 <- subset(HD1,select=-c(dens,col))

title("Right Elbow position", line=-1.3,outer=T,cex.main=1.5)
######LEFT WRIST######
#Plano XY:
par(mfrow=c(2,3))
## Use densCols() output to get density at each point
x <- densCols(BMD1$LHandPos_2,BMD1$LHandPos_3, colramp=colorRampPalette(c("black", "white")))
BMD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
BMD1$col <- cols[BMD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(LHandPos_3~LHandPos_2, data=BMD1[order(BMD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="y", main="BMDistrophy: XY",xlim=c(-400,450),ylim=c(-250,650))
#Plano YZ:
## Use densCols() output to get density at each point
x <- densCols(BMD1$LHandPos_3,BMD1$LHandPos_4, colramp=colorRampPalette(c("black", "white")))
BMD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
BMD1$col <- cols[BMD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(LHandPos_4~LHandPos_3, data=BMD1[order(BMD1$dens),], pch=20, col=col, cex=2, xlab="y", ylab="z", main="BMDistrophy: YZ",xlim=c(-250,700),ylim=c(800,1750))
#Plano XZ:
## Use densCols() output to get density at each point
x <- densCols(BMD1$LHandPos_2,BMD1$LHandPos_4, colramp=colorRampPalette(c("black", "white")))
BMD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
BMD1$col <- cols[BMD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(LHandPos_4~LHandPos_2, data=BMD1[order(BMD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="z", main="BMDistrophy: XZ",xlim=c(-400,450),ylim=c(800,1750))
BMD1 <- subset(BMD1,select=-c(col,dens))

#Plano XY
## Use densCols() output to get density at each point
x <- densCols(HD1$LHandPos_2,HD1$LHandPos_3, colramp=colorRampPalette(c("black", "white")))
HD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
HD1$col <- cols[HD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(LHandPos_3~LHandPos_2, data=HD1[order(HD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="y", main="Healthy: XY",xlim=c(-400,450),ylim=c(-250,650))
#Plano YZ:
## Use densCols() output to get density at each point
x <- densCols(HD1$LHandPos_3,HD1$LHandPos_4, colramp=colorRampPalette(c("black", "white")))
HD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
HD1$col <- cols[HD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(LHandPos_4~LHandPos_3, data=HD1[order(HD1$dens),], pch=20, col=col, cex=2, xlab="y", ylab="z", main="Healthy: YZ",xlim=c(-250,700),ylim=c(800,1750))
#Plano XZ:
## Use densCols() output to get density at each point
x <- densCols(HD1$LHandPos_2,HD1$LHandPos_4, colramp=colorRampPalette(c("black", "white")))
HD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
HD1$col <- cols[HD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(LHandPos_4~LHandPos_2, data=HD1[order(HD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="z", main="Healthy: XZ",xlim=c(-400,450),ylim=c(800,1750))
HD1 <- subset(HD1,select=-c(dens,col))

title("Left Wrist position", line=-1.3,outer=T,cex.main=1.5)


######RIGHT WRIST######

#Plano XY:
par(mfrow=c(2,3))
## Use densCols() output to get density at each point
x <- densCols(BMD1$RHandPos_2,BMD1$RHandPos_3, colramp=colorRampPalette(c("black", "white")))
BMD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
BMD1$col <- cols[BMD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(RHandPos_3~RHandPos_2, data=BMD1[order(BMD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="y", main="BMDistrophy: XY",xlim=c(-650,410),ylim=c(-700,250))
#Plano YZ:
## Use densCols() output to get density at each point
x <- densCols(BMD1$RHandPos_3,BMD1$RHandPos_4, colramp=colorRampPalette(c("black", "white")))
BMD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
BMD1$col <- cols[BMD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(RHandPos_4~RHandPos_3, data=BMD1[order(BMD1$dens),], pch=20, col=col, cex=2, xlab="y", ylab="z", main="BMDistrophy: YZ",xlim=c(-700,250),ylim=c(800,1850))
#Plano XZ:
## Use densCols() output to get density at each point
x <- densCols(BMD1$RHandPos_2,BMD1$RHandPos_4, colramp=colorRampPalette(c("black", "white")))
BMD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
BMD1$col <- cols[BMD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(RHandPos_4~RHandPos_2, data=BMD1[order(BMD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="z", main="BMDistrophy: XZ",xlim=c(-650,410),ylim=c(800,1850))
BMD1 <- subset(BMD1,select=-c(col,dens))


#Plano XY
## Use densCols() output to get density at each point
x <- densCols(HD1$RHandPos_2,HD1$RHandPos_3, colramp=colorRampPalette(c("black", "white")))
HD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
HD1$col <- cols[HD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(RHandPos_3~RHandPos_2, data=HD1[order(HD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="y", main="Healthy: XY",xlim=c(-650,410),ylim=c(-700,250))
#Plano YZ:
## Use densCols() output to get density at each point
x <- densCols(HD1$RHandPos_3,HD1$RHandPos_4, colramp=colorRampPalette(c("black", "white")))
HD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
HD1$col <- cols[HD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(RHandPos_4~RHandPos_3, data=HD1[order(HD1$dens),], pch=20, col=col, cex=2, xlab="y", ylab="z", main="Healthy: YZ",xlim=c(-700,250),ylim=c(800,1850))
#Plano XZ:
## Use densCols() output to get density at each point
x <- densCols(HD1$RHandPos_2,HD1$RHandPos_4, colramp=colorRampPalette(c("black", "white")))
HD1$dens <- col2rgb(x)[1,] + 1L
## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
HD1$col <- cols[HD1$dens]
## Plot it, reordering rows so that densest points are plotted on top
plot(RHandPos_4~RHandPos_2, data=HD1[order(HD1$dens),], pch=20, col=col, cex=2, xlab="x", ylab="z", main="Healthy: XZ",xlim=c(-650,410),ylim=c(800,1850))
HD1 <- subset(HD1,select=-c(dens,col))

title("Right Wrist position", line=-1.3,outer=T,cex.main=1.5)
if (!require("corrplot"))
    {
      install.packages("corrplot")
      library(corrplot)
      install.packages("Hmisc")
      library(Hmisc)
  }
#Balls plot:
mat<-cor(as.matrix(BMD[,1:ncol(BMD)-1]))
diag(mat)<-0
corrplot(mat[2:nrow(mat),2:ncol(mat)],method="circle",is.corr=FALSE,tl.cex=0.7,main="BMDistrophy",mar=c(0,0,1,0))
mat<-cor(as.matrix(HD[,1:ncol(HD)-1]))
diag(mat)<-0
corrplot(mat[2:nrow(mat),2:ncol(mat)],method="circle",is.corr=FALSE,tl.cex=0.7, main="Healthy",mar=c(0,0,1,0))

mat<-cor(as.matrix(TD[,1:ncol(TD)-1]))
diag(mat)<-0
corrplot(mat[2:nrow(mat),2:ncol(mat)],method="circle",is.corr=FALSE,tl.cex=0.7,main="Total",mar=c(0,0,1,0))

# 5. Multivariant analysis: Principal Component Analysis

pca<-prcomp(BMD1[,2:(ncol(BMD1)-1)],center=TRUE,scale=T)
print("BMDistrophy Data")
summary(pca)

pca1<-prcomp(HD1[,2:(ncol(HD1)-1)],center=TRUE,scale=T)
print("Healthy Data")
summary(pca1)

plot(1:10, c(pca$sdev^2)[1:10]/(sum(pca$sdev^2))*100, type="b",
     main="SCREE GRAPH",xlab="Components", ylab="% Variance explained",
     ylim=c(0,max(pca$sdev^2/(sum(pca$sdev^2)),pca1$sdev^2/(sum(pca1$sdev^2)))*100+10),col="red")
lines(1:10, c(pca1$sdev^2)[1:10], col="blue",type="b")
legend(7.5, 50, legend=c("BMDistrophy", "Healthy"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

#Barplot of contribution of each variable to the two firsts PC:
if (!require("factoextra"))
{
  install.packages("factoextra")
  library(factoextra)
}
fviz_contrib(pca, choice = "var", axes = 1, top = 20,title="BMDistrophy - PC1")
fviz_contrib(pca, choice = "var", axes = 2, top = 20,title="BMDistrophy - PC2")

fviz_contrib(pca1, choice = "var", axes = 1, top = 20,title="Healthy - PC1")
fviz_contrib(pca1, choice = "var", axes = 2, top = 20,title="Healthy - PC2")

corvar <- pca$rotation %*% diag(pca$sdev)
plot(-1:1, -1:1, type='n', asp=1, xlab='PC1', ylab='PC2',main="BMDistrophy")

abline(h=0, v=0, lty=2, col=8)

## Dibuja un círculo de centro (0,0) y radio 1
symbols(0, 0, 1, inches=F, add=T)
symbols(0, 0, sqrt(.5), inches=F, add=T)

## Dibuja los vectores y coloca los nombres
arrows(0, 0, corvar[,1], corvar[,2], length=.1)
text(corvar[,1], corvar[,2], colnames(BMD1)[2:ncol(BMD1)-1], pos=4, offset=.6, col=2, font=2)

corvar1 <- pca1$rotation %*% diag(pca1$sdev)
plot(-1:1, -1:1, type='n', asp=1, xlab='PC1', ylab='PC2',main="Healthy")

abline(h=0, v=0, lty=2, col=8)

## Dibuja un círculo de centro (0,0) y radio 1
symbols(0, 0, 1, inches=F, add=T)
symbols(0, 0, sqrt(.5), inches=F, add=T)

## Dibuja los vectores y coloca los nombres
arrows(0, 0, corvar1[,1], corvar1[,2], length=.1)
text(corvar1[,1], corvar1[,2], colnames(HD1)[2:ncol(HD1)-1], pos=4, offset=.6, col=2, font=2)

corvar <- pca$rotation %*% diag(pca$sdev)
plot(-1:1, -1:1, type='n', asp=1, xlab='PC3', ylab='PC4',main="BMDistrophy")

abline(h=0, v=0, lty=2, col=8)

## Dibuja un círculo de centro (0,0) y radio 1
symbols(0, 0, 1, inches=F, add=T)
symbols(0, 0, sqrt(.5), inches=F, add=T)

## Dibuja los vectores y coloca los nombres
arrows(0, 0, corvar[,3], corvar[,4], length=.1)
text(corvar[,3], corvar[,4], colnames(BMD1)[2:ncol(BMD1)-1], pos=4, offset=.6, col=2, font=2)

corvar1 <- pca1$rotation %*% diag(pca1$sdev)
plot(-1:1, -1:1, type='n', asp=1, xlab='PC3', ylab='PC4',main="Healthy")

abline(h=0, v=0, lty=2, col=8)

## Dibuja un círculo de centro (0,0) y radio 1
symbols(0, 0, 1, inches=F, add=T)
symbols(0, 0, sqrt(.5), inches=F, add=T)

## Dibuja los vectores y coloca los nombres
arrows(0, 0, corvar1[,3], corvar1[,4], length=.1)
text(corvar1[,3], corvar1[,4], colnames(HD1)[2:ncol(HD1)-1], pos=4, offset=.6, col=2, font=2)

pca<-prcomp(TD1[,2:ncol(TD1)-1],center=FALSE,scale=T)
print("Total Data")
summary(pca)
corvar <- pca$rotation %*% diag(pca$sdev)
plot(-1:1, -1:1, type='n', asp=1, xlab='PC1', ylab='PC2',main="Total")

abline(h=0, v=0, lty=2, col=8)

## Dibuja un círculo de centro (0,0) y radio 1
symbols(0, 0, 1, inches=F, add=T)
symbols(0, 0, sqrt(.5), inches=F, add=T)

## Dibuja los vectores y coloca los nombres
arrows(0, 0, corvar[,1], corvar[,2], length=.1)
text(corvar[,1], corvar[,2], colnames(TD1)[2:ncol(TD1)-1], pos=4, offset=.6, col=2, font=2)
