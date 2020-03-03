#' winpca
#'
#' This function allows you to perform a pca in a database which is like a time series.
#' You indicate the quantity of 'windows' or intervals of time that you want and it 
#' will perform a pca for each interval.
#' @param data Dataset to do the PCA
#' @param var Variable of dataframe that indicates the time
#' @param interval Number of interval in which you would like to devide the dataset
#' @keywords pca
#' @export
#' @examples
#' winpca()
#'
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
