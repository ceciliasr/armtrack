*2. Angles*

In order to validate the other angles that are present on the dataset, we can see the histogram of them:
    
```{r, warning=FALSE, message=FALSE,echo=FALSE}
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
if (!require("plotly"))
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

p1<-ggplot(TD, aes(LElbAngle, color=Type)) +  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', fill = "white", binwidth=1) + scale_x_continuous(name ="Left",breaks=seq(0,150,10),limits=c(0,150),labels=c("0","","","30","","","60","","","90","","","120","","","150"))+theme(legend.position = "none")+ scale_y_continuous(name="",breaks=seq(0,0.0425,0.0025),limits=c(0,0.0425))

p2<-ggplot(TD, aes(RElbAngle, color=Type)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', fill = "white", binwidth=1) +scale_x_continuous(name ="Right",breaks=seq(0,150,10),limits=c(0,150),labels=c("0","","","30","","","60","","","90","","","120","","","150"))+theme(legend.position = "none")+ scale_y_continuous(name="",breaks=seq(0,0.0425,0.0025),limits=c(0,0.0425),  labels=rep("",18))

#legend<-get_legend(p1)
p1<-ggplotly(p1)
p2<-ggplotly(p2)
subplot(p1, p2)%>%
layout(title = list(text = "Elbow angle", y = 0.995))
```
    
```{r, warning=FALSE, message=FALSE,echo=FALSE}
p1<-ggplot(TD, aes(LSAngle, color=Type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', fill = "white", binwidth=1) + scale_x_continuous(name ="Left",breaks=seq(0,150,10),limits=c(0,150),labels=c("0","","","30","","","60","","","90","","","120","","","150"))+ scale_y_continuous(name="",breaks=seq(0,0.036,0.002),limits=c(0,0.035))+theme(legend.position = "top")
p11<-p1+theme(legend.position = "none")
p2<-ggplot(TD, aes(RSAngle, color=Type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', fill = "white", binwidth=1) + scale_x_continuous(name ="Right",breaks=seq(0,150,10),limits=c(0,150),labels=c("0","","","30","","","60","","","90","","","120","","","150"))+ scale_y_continuous(name="", breaks=seq(0,0.036,0.002),limits=c(0,0.035), labels=rep("",19))+ theme(legend.position="none")

p11<-ggplotly(p11)
p2<-ggplotly(p2)
subplot(p11, p2)%>%
layout(title = list(text = "Upper arm and body angle", y = 0.995))
```
    
Note that these angles are directly expressed in degrees, not radians. Neither the Elbow angle nor the upper arm and body angle arrive never to 180º. This is because the guy who was using the T-Shirt that collect the data was only working on the office and it would be strange to arrive to these degrees.

If we compare Healthy and Becker Distrophy Data, we can see more frequency and less dispersion in Becker Distrophy data. This makes sense regarding to the fact that people with Becker Distrophy has less mobility.

Finally we have compared the torso inclination angle between Healthy and Becker Distrophy data:

```{r}
p1<-ggplot(TD, aes(ang, color=Type)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', fill = "white", binwidth=1) + scale_x_continuous(name ="Torso angle",breaks=seq(0,180,10),limits=c(0,180),labels=c("0","","","30","","","60","","","90","","","120","","","150","","","180"))+scale_y_continuous(name="",breaks=seq(0,0.036,0.002),limits=c(0,0.035))+theme(legend.position = "none")
p1<-ggplotly(p1)
p1
```

People with Becker muscular Distrophy  has less movement on their extremities but more movement on the torso and this histogram shows us exactly this detail. We can see more frequency and less variance in healthy data than in BMD data. This is one of the few cases where we are going to see this situation.



*3. Position variables*

There are 4 position data reffering to (right and left) elbow position and (right and left) hand position. On the '.mat' database, these variables are represented as quaternions but on the preprocess performed, we realised that there is allways a component of these quaternions that was with value equal (or very similar) to 0. So, we have converted each position quaternion in 3 numeric variables corresponding to the 3 axes x, y and z. Therefore, we have finally $3 \cdot 4=12$ variables reffering to 4 different positions.

We are going to see density plots of the planes XY, YZ and XZ in order to see where are concentrated the biggest quantity of movements. But first, we reduce the data frequency (from 50 records per second to 5 records per second) to optimize the computation time:

```{r, warning=FALSE, message=FALSE,echo=FALSE}
se<-seq(0,nrow(HD),10)
HD1<-HD[rownames(HD) %in% se,]
se<-seq(0,nrow(BMD),10)
BMD1<-BMD[rownames(BMD) %in% se,]
se<-seq(0,nrow(TD),10)
TD1<-TD[rownames(TD) %in% se,]
rm(se)
```


```{r, warning=FALSE, message=FALSE,echo=FALSE}
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
```

As we expected, we can see more mobility on the healthy data elbows. Becker data has more red points (the positions are more frequents) and the movements are concentrated in a smaller space.

On the other hand, mainly in the plots in which the z axis appears, we can observe the rotation movement.

```{r, echo=F, warning=F, message=F}
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
```

The comments here are very similar to the ones before. More variability in healthy data and there is (almost allways) more than one red point because of the bigger mobility of healthy people.


### 4. Bivariant analysis: Correlogram

The next step is to see if there is a relationship between any of the variables. We have plotted the *correlation* between all the variables. First on Becker muscular distrophy data and then in healthy data:

```{r,echo=F, warning=F, message=F, fig.width = 9, fig.height = 9}
  if (!require("corrplot"))
    {
      install.packages("corrplot")
      library(corrplot)
 #     install.packages("Hmisc")
#      library(Hmisc)
  }
#Balls plot:
mat<-cor(as.matrix(BMD[,1:ncol(BMD)-1]))
diag(mat)<-0
corrplot(mat[2:nrow(mat),2:ncol(mat)],method="circle",is.corr=FALSE,tl.cex=0.7,main="BMDistrophy",mar=c(0,0,1,0))
```

Note that the diagonal is 0. We did this change in order to see better the real correlation between all variables (otherwise, the diagonal is 1 and the other 'balls' could be shorter).

We can see in general a small correlation in all the variables. The positive correlation between elbow position and the upper-forearm angle (in both sides) could be stand out.

```{r,echo=F, warning=F, message=F, fig.width = 9, fig.height = 9}
mat<-cor(as.matrix(HD[,1:ncol(HD)-1]))
diag(mat)<-0
corrplot(mat[2:nrow(mat),2:ncol(mat)],method="circle",is.corr=FALSE,tl.cex=0.7, main="Healthy",mar=c(0,0,1,0))
```

The main difference we can observe between BMD data and healthy data is that in healthy data there is a bigger correlation in (almost) all the variables.

Now we do the same plot for the total data without making any difference between BMDistrophy and Healthy data:

```{r,echo=F, warning=F, message=F, fig.width = 9, fig.height = 9}
mat<-cor(as.matrix(TD[,1:ncol(TD)-1]))
diag(mat)<-0
corrplot(mat[2:nrow(mat),2:ncol(mat)],method="circle",is.corr=FALSE,tl.cex=0.7,main="Total",mar=c(0,0,1,0))
```

This last plot does not give us much more information.

We will use these correlations if we need to reduce the quantity of variables in order to do deeper studies later.


