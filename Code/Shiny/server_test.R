if (!require("shiny"))
{
  install.packages("shiny")
  library(shiny)
}

setwd("c:/TFM/Code/Shiny")
shinyServer(
  function(input,output){
    output$skeleton <- renderGvis({
      source("c:/TFM/Code/Shiny/packages.R")
      if((input$Subject=="Victor")==T){
        data<-"c:/TFM/Code/Shiny/Data/DataVic_TFMCecilia_05022020.mat"  
      }
      else{
        data<-"c:/TFM/Code/Shiny/Data/DataCecilia_TFMCecilia_05022020.mat"  
      }
      
      dd<-at.read2(data)
      data<-dd[,c(3:12,16:21)]
      #Timestamp and data type are not relevant variables for PCA
      x<-as.matrix(data)
      #Performing PCA
      pca<-prcomp(x,center=T,scale=T)
      eigenvectors<-as.matrix(pca$rotation) #PCs as columns, Initial variables as rows
      if ((input$range[1]==input$range[2])==T)
      {
        #But we are going to change s values in order to understand PC1 meaning
        mu<-unname(pca$center)
        mu<-matrix(mu,nrow=1,ncol=ncol(x),byrow=TRUE)
        sigma<-unname(apply(data, 2, sd, na.rm = TRUE))
        sigma<-matrix(sigma,nrow=1,ncol=ncol(x),byrow=TRUE)
        eigenvectors<-eigenvectors[,input$PC]
        res<-((input$range[1]%*%t(eigenvectors))*sigma)+mu
        dd1<-data.frame(input$range[1],res)
        names(dd1)[1]<-"Timestamp"
        rm(res,pca)
        
        constant<-dd[1:nrow(dd1),c(13:15,22:ncol(dd))]
        df<-cbind.data.frame(dd1,constant[,1:ncol(constant)])  
        LHand<-subset(df,select=c(Timestamp,LHandPosX,LHandPosY,LHandPosZ))
        names(LHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        LElb<-subset(df,select=c(Timestamp,LElbPosX,LElbPosY,LElbPosZ))
        names(LElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        LSh<-subset(df,select=c(Timestamp,LShPosX,LShPosY,LShPosZ))
        names(LSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        Neck<-subset(df,select=c(Timestamp,NeckPosX,NeckPosY,NeckPosZ))
        names(Neck)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        Head<-subset(df,select=c(Timestamp,HeadPosX,HeadPosY,HeadPosZ))
        names(Head)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        RSh<-subset(df,select=c(Timestamp,RShPosX,RShPosY,RShPosZ))
        names(RSh)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        RElb<-subset(df,select=c(Timestamp,RElbPosX,RElbPosY,RElbPosZ))
        names(RElb)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        RHand<-subset(df,select=c(Timestamp,RHandPosX,RHandPosY,RHandPosZ))
        names(RHand)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        
        torso<-subset(df,select=c(Timestamp,TorsoPosX,TorsoPosY,TorsoPosZ))
        names(torso)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        waist<-subset(df,select=c(Timestamp,WaistPosX,WaistPosY,WaistPosZ))
        names(waist)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        LThighPos<-subset(df,select=c(Timestamp,LThighPosX,LThighPosY,LThighPosZ))
        names(LThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        RThighPos<-subset(df,select=c(Timestamp,RThighPosX,RThighPosY,RThighPosZ))
        names(RThighPos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        LKneePos<-subset(df,select=c(Timestamp,LKneePosX,LKneePosY,LKneePosZ))
        names(LKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        RKneePos<-subset(df,select=c(Timestamp,RKneePosX,RKneePosY,RKneePosZ))
        names(RKneePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        LAnklePos<-subset(df,select=c(Timestamp,LAnklePosX,LAnklePosY,LAnklePosZ))
        names(LAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        RAnklePos<-subset(df,select=c(Timestamp,RAnklePosX,RAnklePosY,RAnklePosZ))
        names(RAnklePos)<-c("Timestamp","Pos_x","Pos_y","Pos_z")
        
        dd<-rbind.data.frame(Head,Neck,LSh,LElb,LHand,LElb,LSh,Neck,RSh,RElb,RHand,RElb,RSh, Neck, torso, waist,
                             LThighPos,LKneePos,LAnklePos,LKneePos,LThighPos,waist,RThighPos,RKneePos,RAnklePos,
                             RKneePos,RThighPos,waist,torso,Neck)
        
        dd<-dd[order(dd$Timestamp),]
        #plot<-at.plotprepare(df,freq=2)
        canvas <- ggplot(dd, aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
          theme_void()+ coord_equal()
        
        
        p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = input$Theta, phi = input$Phi)  + 
          stat_3D(data = dd,mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = input$Theta, phi = input$Phi)
        p_with_canv
      }
      else 
      {
        s_new<-seq(input$range[1],input$range,1)
        mu<-unname(pca$center)
        mu<-matrix(mu,nrow=length(s_new),ncol=ncol(x),byrow=TRUE)
        sigma<-unname(apply(data, 2, sd, na.rm = TRUE))
        sigma<-matrix(sigma,nrow=length(s_new),ncol=ncol(x),byrow=TRUE)
        eigenvectors<-eigenvectors[,input$PC]
        res<-((s_new%*%t(eigenvectors))*sigma)+mu
        dd1<-data.frame(data.frame(s_new),res)
        names(dd1)[1]<-"Timestamp"
        rm(s_new,res,pca)
        
        constant<-dd[1:nrow(dd1),c(13:15,22:ncol(dd))]
        df<-cbind.data.frame(dd1,constant[,1:ncol(constant)])  
        plot<-at.plotprepare(df,freq=1)
        canvas <- ggplot(plot, aes(x = Pos_x, y = Pos_y, z = Pos_z)) +
          theme_void()+ coord_equal()+ transition_time(Timestamp)+coord_fixed(ratio=1)
        
        
        p_with_canv <-canvas+ stat_3D(geom = "point",size=8,theta = input$Theta, phi = input$Phi)  + 
          stat_3D(data = plot,mapping=aes(x = Pos_x, y = Pos_y, z = Pos_z), geom = "path", size = 2, theta = input$Theta, phi = input$Phi)+
          labs(title = " A value: {round(as.integer(frame_time))}")
        animate(p_with_canv,length(unique(plot$Timestamp))) 
      }
      
    })
  }
)