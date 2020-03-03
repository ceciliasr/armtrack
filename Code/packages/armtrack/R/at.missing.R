#' at.missing function
#'
#' This function allows you to imput the missing data with the mean value of each variable when the % of missing data
#' is less than a value (default 5%).
#' @param data Dataset to do the missing treatment
#' @keywords missing
#' @export
#' @examples
#' at.missing()
#'
at.missing<-function(data, perc=5)
{
  max<-max(data$Timestamp)
  d1<-data.frame(c(seq(20,max,20)))
  names(d1)<-"Timestamp"
  d2<-merge(d1,data,by="Timestamp",all.x=T)

  k<-0
  if ((sum(is.na(d2)))/(nrow(d2)*(ncol(d2)-1))*100<perc) {
    for (j in 1:ncol(d2)){
      if (sum(is.na(d2[[j]]))/nrow(d2)>perc){
        cat("The variable",names(d2)[j], "has",sum(is.na(d2[[j]])),"missings. Is it normal?",fill=T)
        k<-k+1
      }
    }
    if (k==0){
    cat("The ",round((sum(is.na(d2)))/(nrow(d2)*(ncol(d2)-1))*100,2),"% of cells are missings. We will imput the next non-missing value of each variable on the missing values.",sep="",fill=T)
    for(i in 1:ncol(d2)){
      if(is.na(d2[1,i])==T){
        d2[1,i]<-d2[2,i]
      }
      while(length(d2[is.na(d2[,i]), i])>0){
        d2[is.na(d2[,i]), i]<-d2[c(as.integer(row.names(d2[is.na(d2[,i]),])))+rep(-1,length(row.names(d2[is.na(d2[,i]),]))),i]
      }
    }
    }
  } else {
    cat("The ",round((sum(is.na(d2)))/(nrow(d2)*(ncol(d2)-1))*100,2),"% of cells are missings. You should perform a deeper study.",sep="",fill=T)
  }
  return(d2)
}

