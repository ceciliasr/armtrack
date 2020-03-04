#' at.pca function
#'
#' This function allows you to perform a pca to a DB and obtain a list with the reconstructed database using
#' the first n components until obtain more than a value (given) of cumulative proportion.
#' @param data Needed in order to know the dataframe file to perform PCA
#' @param val Threshold cumulative proportion value to take into account (Default: 0.9)
#' @keywords pca reconstruct
#' @export
#' @examples
#' at.pca()
#'

at.pca<-function(data,val=0.9)
{
  if (!require("matrixStats"))
  {
    install.packages("matrixStats")
    library(matrixStats)
  }
  #Timestamp and data type are not relevant variables for PCA
  x<-as.matrix(subset(data))
  #Performing PCA
  pca<-prcomp(x,center=T,scale=T)
  eigenvectors<-as.matrix(pca$rotation) #PCs as columns, Initial variables as rows
  mu<-unname(pca$center)
  mu<-matrix(mu,nrow=nrow(x),ncol=ncol(x),byrow=TRUE)
  sigma<-unname(apply(data, 2, sd, na.rm = TRUE))
  sigma<-matrix(sigma,nrow=nrow(x),ncol=ncol(x),byrow=TRUE)
  indiv_components<-as.matrix(pca$x) #PCs as columns, Individuals as rows
  
  #We take the first PC that have the "Cumulative Proportion" value higher than 0.9:
  sum<-0
  i<-0
  while(sum<val)
  {
    i<-i+1
    j<-i*3
    sum<-summary(pca)$importance[j]
  }
  j<-i
  dataframes<-list()
  for(i in 1:j)
  {
    res<-indiv_components[,i]%*%t(eigenvectors[,i])
    dd<-data.frame((res*sigma)+mu) #Reconstructed database using only PC1
    name <- paste("dd",i,sep="")
    dataframes[[name]]<-dd
  }
  x <- readline(cat("The first Principal Component that have the cumulative propotion value higher than 0.9 is ",i,"\n(press enter to continue)",sep=""))
  return(dataframes)
}