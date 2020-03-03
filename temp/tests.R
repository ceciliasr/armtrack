#Installing package
install.packages("c:/TFM/Code/armtrack_0.0.0.9000.tar.gz", repos = NULL, type = "source")
library(armtrack)

Hdata<-"c:/TFM/Data/HealthyData.mat"
Hdata<-at.read(Hdata)

Bdata<-"c:/TFM/Data/BMDData.mat"
Bdata<-at.read(Bdata)

#Imputing missing values
Hd_nomis<-at.missing(Hdata)
Bd_nomis<-at.missing(Bdata)

Hd_nomis$Type<-"Healthy"
Bd_nomis$Type<-"BMDistrophy"
dd<-rbind.data.frame(Hd_nomis,Bd_nomis)

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
rm(Hdata,Bdata,Bd_nomis,Hd_nomis,dd)

save(HD, file="c:/TFM/Data/HeathyD.rda") 
save(BMD, file="c:/TFM/Data/BDistrophyD.rda") 
save(TD, file="c:/TFM/Data/TotalD.rda") 

install.packages("signal")
install.packages("oce")
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(oce, warn.conflicts = F, quietly = T) # image plotting functions and nice color maps
se<-seq(0,nrow(HD),50) #freq=1seg
HD1<-HD[rownames(HD) %in% se,]

par(mfrow=c(2,2))

# number of points to use for the fft
nfft=1024
# window size (in points)
window=256
# overlap (in points)
overlap=128
# create spectrogram
spec = specgram(x = HD1$LElbAngle,n = nfft,Fs = 1,window = window,overlap = overlap)
# discard phase information
P = abs(spec$S)
# normalize
P = P/max(P)
# convert to dB
P = 10*log10(P)
# config time axis
t = spec$t
# plot spectrogram
imagep(x = t,y = spec$f,z = t(P),col = oce.colorsViridis,ylab = 'Frequency [Hz]',xlab = 'Time [s]',drawPalette = T,decimate = F,
       main="Aprox. 6h")

HD2<-HD[1:579195,]#3h
se<-seq(0,nrow(HD2),50) #freq=1seg
HD2<-HD2[rownames(HD2) %in% se,]

# create spectrogram
spec = specgram(x = HD2$LElbAngle,n = nfft,Fs = 1,window = window,overlap = overlap)
# discard phase information
P = abs(spec$S)
# normalize
P = P/max(P)
# convert to dB
P = 10*log10(P)
# config time axis
t = spec$t
# plot spectrogram
imagep(x = t,y = spec$f,z = t(P),col = oce.colorsViridis,ylab = 'Frequency [Hz]',xlab = 'Time [s]',drawPalette = T,decimate = F,
       main="Aprox. 3h")

HD3<-HD[1:289597,]#1.5h
se<-seq(0,nrow(HD3),50) #freq=1seg
HD3<-HD3[rownames(HD3) %in% se,]

# create spectrogram
spec = specgram(x = HD3$LElbAngle,n = nfft,Fs = 1,window = window,overlap = overlap)
# discard phase information
P = abs(spec$S)
# normalize
P = P/max(P)
# convert to dB
P = 10*log10(P)
# config time axis
t = spec$t
# plot spectrogram
imagep(x = t,y = spec$f,z = t(P),col = oce.colorsViridis,ylab = 'Frequency [Hz]',xlab = 'Time [s]',drawPalette = T,decimate = F,
       main="Aprox. 1h30min")

HD4<-HD[1:144798,]#45min
se<-seq(0,nrow(HD4),50) #freq=1seg
HD4<-HD4[rownames(HD4) %in% se,]

# create spectrogram
spec = specgram(x = HD4$LElbAngle,n = nfft,Fs = 1,window = window,overlap = overlap)
# discard phase information
P = abs(spec$S)
# normalize
P = P/max(P)
# convert to dB
P = 10*log10(P)
# config time axis
t = spec$t
# plot spectrogram
imagep(x = t,y = spec$f,z = t(P),col = oce.colorsViridis,ylab = 'Frequency [Hz]',xlab = 'Time [s]',drawPalette = T,decimate = F,
       main="Aprox. 45min")

title('Left Elb Ang. Healthy Data', outer = TRUE,line=-1)





par(mfrow=c(2,2))
del<-1 # sampling interval
x.spec <- spectrum(x=HD1$LElbAngle,log="no",span=10,plot=FALSE)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")

del<-1 # sampling interval
x.spec <- spectrum(x=BMD1$LElbAngle,log="no",span=10,plot=FALSE)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")

del<-100 # sampling interval
x.spec <- spectrum(x=HD1$LElbAngle,log="no",span=10,plot=FALSE)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")

del<-1000 # sampling interval
x.spec <- spectrum(x=HD1$LElbAngle,log="no",span=10,plot=FALSE)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")







del<-1 # sampling interval
x.spec <- spectrum(x=HD2$LElbAngle,log="no",span=10,plot=FALSE)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")

del<-1 # sampling interval
x.spec <- spectrum(x=HD3$LElbAngle,log="no",span=10,plot=FALSE)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")

del<-1 # sampling interval
x.spec <- spectrum(x=HD4$LElbAngle,log="no",span=10,plot=FALSE)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")





##########   EXAMPLE   ##########

# create data generating signals
par(mfrow=c(1,2))
n <- 50                            # 50 Hz signal
HD4<-HD[1:3217,]#1min

eegpsd(HD4$LElbAngle, Fs = 50, upper = 20, t = "b")

BMD4<-BMD[1:3217,]#1min

# plot psd (single channel)
eegpsd(BMD4$LElbAngle, Fs = 50, upper = 20, t = "b")



data<-BMD
var<-"Timestamp"
interval<-5


winpca<-function(data, var, interval)
{
  if (!require("factoextra"))
  {
    install.packages("factoextra")
    library(factoextra)
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
  dd <- data[order(data[[var]]),]
  len<- round(nrow(dd)/interval)
  pca_tot<-data.frame(pca=double(),type=character(),y=double())
  for (i in 1:interval){
    if (i==interval){
      dd1<-dd[c(((i-1)*len):nrow(dd)),which(names(dd)!=var)]
    }else{
      dd1<-dd[c(((i-1)*len):((i*len)-1)),which(names(dd)!=var)] 
    }
    assign(paste("ddat",i,sep="_"),dd1)
    nums <- unlist(lapply(dd1, is.numeric))  
    dd1<-dd1[ , nums]
    pca<-prcomp(dd1,center=TRUE,scale=T)
    assign(paste("pca",i,sep="_"),pca)
    pca_df<-as.data.frame(pca$sdev)
    names(pca_df)<-"pca"
    pca_df$Type<-paste("Dataset",i,sep=" ")
    pca_df$y<-c(pca_df$pca^2)/(sum(pca_df$pca^2))*100
    pca_tot<-rbind.data.frame(pca_tot,pca_df)
    p1<-fviz_contrib(pca, choice = "var", axes = 1, top = 10,title=F)
    p2<-fviz_contrib(pca, choice = "var", axes = 1, top = 10,title=F)
    p3<-fviz_contrib(pca, choice = "var", axes = 1, top = 10,title=F)
    p4<-fviz_contrib(pca, choice = "var", axes = 1, top = 10,title=F)
    p1<-plotly_build(p1)
    p2<-plotly_build(p2)
    p3<-plotly_build(p3)
    p4<-plotly_build(p4)
    p <- subplot(p1,p2,p3,p4,nrows=2,titleX=T, titleY=T,margin=0.15)
    p<-p %>% layout(annotations = list( 
      list(x = 0.5 , y = 1.07, text = paste("Dataset",i,sep=" "), showarrow = F, xref='paper', yref='paper'),
      list(x = 0.2 , y = 1.05, text = "PC1", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.8 , y = 1.05, text = "PC2", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.2 , y = 0.4, text = "PC3", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.8 , y = 0.4, text = "PC4", showarrow = F, xref='paper', yref='paper')))
    print(p)
  }
  pca_tot$x<-rep(c(1:ncol(dd1)),interval)
  scpl<-pca_tot %>% ggplot(aes(pca_tot$x, pca_tot$y,color = Type)) +
    geom_line(data=pca_tot, aes(pca_tot$x, pca_tot$y, color=Type))+geom_point()+
    scale_x_discrete(name ="ScatterPlot")+
    scale_y_continuous(name="",breaks=seq(0,max(pca_tot$y),5),
                       labels=seq(0,max(pca_tot$y),5),limits=c(0,max(pca_tot$y)))
  
  scpl <- plotly_build(scpl)
  print(scpl)
}
winpca(data,var,interval)

