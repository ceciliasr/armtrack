#' at.pca3 function
#'
#' This function allows you to perform a PCA on a dataframe with 240 cols (Temp. Window)
#' @param data Needed in order to know the dataframe on which perform PCA
#' @keywords window 
#' @export
#' @examples
#' at.pca3()
#'

at.pca3<-function(data) #By default window of 8s
{
#Convert dataframe to real number
for (i in 1:240)
{
  data[[i]]<-as.numeric(as.character(data[[i]]))
}

pca<-prcomp(data[,1:240],center=T,scale=T)

#We take 28 PCs because normally explain more than an 80% of data variability
dd1<-cbind.data.frame(pca$x[,1:28],data$action,data$id)
rm(data)
names(dd1)[29]<-"action"
names(dd1)[30]<-"id"
dd1$action<-factor(dd1$action)
dd1$id<-factor(dd1$id)
rm(pca)
for (i in 1:28)
{
  dd1[[i]]<-as.numeric(as.character(dd1[[i]]))
}
rm(i)

return(dd1)

}