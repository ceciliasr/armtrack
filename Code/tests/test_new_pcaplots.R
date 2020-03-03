source("C:/TFM/Code/packages.R")
data<-"c:/TFM/Data/DataCecilia_TFMCecilia_05022020.mat"
dd<-at.read2(data)
#Let's study the first PC. 
data<-dd[,c(3:12,16:21)]
#Timestamp and data type are not relevant variables for PCA
x<-as.matrix(data)
#Performing PCA

for(i in 1:5)
{

  pca<-prcomp(x,center=T,scale=T)
  eigenvectors<-as.matrix(pca$rotation) #PCs as columns, Initial variables as rows
  #But we are going to change s values in order to understand PC1 meaning
  a<-100
  s_new<-seq(-a,a,1)
  mu<-unname(pca$center)
  mu<-matrix(mu,nrow=length(s_new),ncol=ncol(x),byrow=TRUE)
  sigma<-unname(apply(data, 2, sd, na.rm = TRUE))
  sigma<-matrix(sigma,nrow=length(s_new),ncol=ncol(x),byrow=TRUE)
  eigenvectors<-eigenvectors[,i]
  res<-((s_new%*%t(eigenvectors))*sigma)+mu
  dd1<-data.frame(data.frame(s_new),res)
  names(dd1)[1]<-"Timestamp"
  rm(eigenvectors,s_new,res,pca)
  
  constant<-dd[1:nrow(dd1),c(13:15,22:ncol(dd))]
  df<-cbind.data.frame(dd1,constant[,1:ncol(constant)])  
  plot<-at.plotprepare(df,freq=2)
  for(phi in c(0,45,90,135,180))
  {
    theta<-90
    canvas <- ggplot(plot, aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
      theme_void()+ coord_equal()+ transition_time(Timestamp)+coord_fixed(ratio=1)
    
    
    p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = theta, phi = phi)  + 
      stat_3D(data = plot,mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = theta, phi = phi)+
      labs(title = " A value: {round(as.integer(frame_time))}")
    file=paste0("c:/TFM/Results/CeciD_20200205/GIFs/PCLearning/PC",i,"_",theta,"_",phi,".gif")
    animate(p_with_canv,length(unique(plot$Timestamp)))
    anim_save(file=file)
  }
  for(theta in c(0,45,135,180))
  {
    phi<-0
    canvas <- ggplot(plot, aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
      theme_void()+ coord_equal()+ transition_time(Timestamp)+coord_fixed(ratio=1)
    
    
    p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = theta, phi = phi)  + 
      stat_3D(data = plot,mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = theta, phi = phi)+
      labs(title = " A value: {round(as.integer(frame_time))}")
    file=paste0("c:/TFM/Results/CeciD_20200205/GIFs/PCLearning/PC",i,"_",theta,"_",phi,".gif")
    animate(p_with_canv,length(unique(plot$Timestamp)))
    anim_save(file=file)
  }
}
data<-"c:/TFM/Data/DataVic_TFMCecilia_05022020.mat"
dd<-at.read2(data)
#Let's study the first PC. 
data<-dd[,c(3:12,16:21)]
#Timestamp and data type are not relevant variables for PCA
x<-as.matrix(data)
#Performing PCA

for(i in 1:5)
{
  
  pca<-prcomp(x,center=T,scale=T)
  eigenvectors<-as.matrix(pca$rotation) #PCs as columns, Initial variables as rows
  #But we are going to change s values in order to understand PC1 meaning
  a<-5000
  s_new<-seq(-a,a,1)
  mu<-unname(pca$center)
  mu<-matrix(mu,nrow=length(s_new),ncol=ncol(x),byrow=TRUE)
  sigma<-unname(apply(data, 2, sd, na.rm = TRUE))
  sigma<-matrix(sigma,nrow=length(s_new),ncol=ncol(x),byrow=TRUE)
  eigenvectors<-eigenvectors[,i]
  res<-((s_new%*%t(eigenvectors))*sigma)+mu
  dd1<-data.frame(data.frame(s_new),res)
  names(dd1)[1]<-"Timestamp"
  rm(eigenvectors,s_new,res,pca)
  
  constant<-dd[1:nrow(dd1),c(13:15,22:ncol(dd))]
  df<-cbind.data.frame(dd1,constant[,1:ncol(constant)])  
  plot<-at.plotprepare(df,freq=2)
  for(phi in c(0,45,90,135,180))
  {
    theta<-90
    canvas <- ggplot(plot, aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
      theme_void()+ coord_equal()+ transition_time(Timestamp)+coord_fixed(ratio=1)
    
    
    p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = theta, phi = phi)  + 
      stat_3D(data = plot,mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = theta, phi = phi)+
      labs(title = " A value: {round(as.integer(frame_time))}")
    file=paste0("c:/TFM/Results/VictorD_20200205/GIFs/PCLearning/PC",i,"_",theta,"_",phi,".gif")
    animate(p_with_canv,length(unique(plot$Timestamp)))
    anim_save(file=file)
  }
  for(theta in c(0,45,135,180))
  {
    phi<-0
    canvas <- ggplot(plot, aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
      theme_void()+ coord_equal()+ transition_time(Timestamp)+coord_fixed(ratio=1)
    
    
    p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = theta, phi = phi)  + 
      stat_3D(data = plot,mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = theta, phi = phi)+
      labs(title = " A value: {round(as.integer(frame_time))}")
    file=paste0("c:/TFM/Results/VictorD_20200205/GIFs/PCLearning/PC",i,"_",theta,"_",phi,".gif")
    animate(p_with_canv,length(unique(plot$Timestamp)))
    anim_save(file=file)
  }
}


shell("c:/TFM/Code/ConvertVideo.bat")
#p<-at.skeletonplot(plot)
#assign(paste0("p",i),p)
