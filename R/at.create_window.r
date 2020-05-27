#' at.create_window function
#'
#' This function allows you to create a temporal window of your data
#' @param data Needed in order to know the dataframe on which create temporal window
#' @param lookback How many timesteps on past the window should take into account. Default 40 (8s)
#' @param delay How many timesteps on future the window should take into account. Default 40 (8s)
#' @keywords window 
#' @export
#' @examples
#' at.create_window()
#'

at.create_window<-function(data,lookback=40,delay=40) #By default window of 8s
{


generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                (dim(data)[[-1]])-2))
    targets <- array(0, dim = c(length(rows)))
    id <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,1:7]
      targets[[j]] <- data[rows[[j]] + delay,8]
      id[[j]] <- data[rows[[j]] + delay,9]
    }           
    list(samples, targets,id)
  }
}

#lookback: Cuantos timesteps en el pasado deberia ir el dataset de input (8seg.)
step <- 1 #El periodo, en timesteps, en el que se muestrean los datos (1 xq ya estan resamp.)
#delay: Cuantos timesteps en el futuro deberia ir el target. (2seg.)
batch_size <- 100 #Cada cuántos timesteps se deben actualizar los parámetros del modelo (2 seg)
#epochs: Cuántas veces el modelo tiene que recorrer los datos completos
#max_index and min_index sirven para crear los datasets de train, valid y test
gen2 <- generator(data,lookback = lookback,delay = delay,min_index = 1,
                        max_index = nrow(data),shuffle = F,step = step, batch_size = batch_size)


dd<-as.matrix(gen2())
dd<-cbind.data.frame(as.data.frame(dd[[1]][,,1]),as.data.frame(dd[[1]][,,2]),as.data.frame(dd[[1]][,,3]),
                        as.data.frame(dd[[1]][,,4]),as.data.frame(dd[[1]][,,5]),as.data.frame(dd[[1]][,,6])
                     ,as.data.frame(dd[[2]]),as.data.frame(dd[[3]]))
names(dd)<-c("PC1T1","PC1T2","PC1T3","PC1T4","PC1T5","PC1T6","PC1T7","PC1T8","PC1T9","PC1T10","PC1T11","PC1T12","PC1T13","PC1T14","PC1T15","PC1T16","PC1T17","PC1T18","PC1T19","PC1T20",
             "PC1T21","PC1T22","PC1T23","PC1T24","PC1T25","PC1T26","PC1T27","PC1T28","PC1T29","PC1T30","PC1T31","PC1T32","PC1T33","PC1T34","PC1T35","PC1T36","PC1T37","PC1T38","PC1T39","PC1T40",
             "PC2T1","PC2T2","PC2T3","PC2T4","PC2T5","PC2T6","PC2T7","PC2T8","PC2T9","PC2T10","PC2T11","PC2T12","PC2T13","PC2T14","PC2T15","PC2T16","PC2T17","PC2T18","PC2T19","PC2T20",
             "PC2T21","PC2T22","PC2T23","PC2T24","PC2T25","PC2T26","PC2T27","PC2T28","PC2T29","PC2T30","PC2T31","PC2T32","PC2T33","PC2T34","PC2T35","PC2T36","PC2T37","PC2T38","PC2T39","PC2T40",
             "PC3T1","PC3T2","PC3T3","PC3T4","PC3T5","PC3T6","PC3T7","PC3T8","PC3T9","PC3T10","PC3T11","PC3T12","PC3T13","PC3T14","PC3T15","PC3T16","PC3T17","PC3T18","PC3T19","PC3T20",
             "PC3T21","PC3T22","PC3T23","PC3T24","PC3T25","PC3T26","PC3T27","PC3T28","PC3T29","PC3T30","PC3T31","PC3T32","PC3T33","PC3T34","PC3T35","PC3T36","PC3T37","PC3T38","PC3T39","PC3T40",
             "PC4T1","PC4T2","PC4T3","PC4T4","PC4T5","PC4T6","PC4T7","PC4T8","PC4T9","PC4T10","PC4T11","PC4T12","PC4T13","PC4T14","PC4T15","PC4T16","PC4T17","PC4T18","PC4T19","PC4T20",
             "PC4T21","PC4T22","PC4T23","PC4T24","PC4T25","PC4T26","PC4T27","PC4T28","PC4T29","PC4T30","PC4T31","PC4T32","PC4T33","PC4T34","PC4T35","PC4T36","PC4T37","PC4T38","PC4T39","PC4T40",
             "PC5T1","PC5T2","PC5T3","PC5T4","PC5T5","PC5T6","PC5T7","PC5T8","PC5T9","PC5T10","PC5T11","PC5T12","PC5T13","PC5T14","PC5T15","PC5T16","PC5T17","PC5T18","PC5T19","PC5T20",
             "PC5T21","PC5T22","PC5T23","PC5T24","PC5T25","PC5T26","PC5T27","PC5T28","PC5T29","PC5T30","PC5T31","PC5T32","PC5T33","PC5T34","PC5T35","PC5T36","PC5T37","PC5T38","PC5T39","PC5T40",
             "PC6T1","PC6T2","PC6T3","PC6T4","PC6T5","PC6T6","PC6T7","PC6T8","PC6T9","PC6T10","PC6T11","PC6T12","PC6T13","PC6T14","PC6T15","PC6T16","PC6T17","PC6T18","PC6T19","PC6T20",
             "PC6T21","PC6T22","PC6T23","PC6T24","PC6T25","PC6T26","PC6T27","PC6T28","PC6T29","PC6T30","PC6T31","PC6T32","PC6T33","PC6T34","PC6T35","PC6T36","PC6T37","PC6T38","PC6T39","PC6T40",
			 "action","id")
n<-trunc((nrow(data)-delay-batch_size)/batch_size)
for (i in 1:n)
{
  mat_new<-as.matrix(gen2())
  mat_new<-cbind.data.frame(as.data.frame(mat_new[[1]][,,1]),as.data.frame(mat_new[[1]][,,2]),as.data.frame(mat_new[[1]][,,3]),
                       as.data.frame(mat_new[[1]][,,4]),as.data.frame(mat_new[[1]][,,5]),as.data.frame(mat_new[[1]][,,6]),
                       as.data.frame(mat_new[[2]]),as.data.frame(mat_new[[3]]))
  names(mat_new)<-c("PC1T1","PC1T2","PC1T3","PC1T4","PC1T5","PC1T6","PC1T7","PC1T8","PC1T9","PC1T10","PC1T11","PC1T12","PC1T13","PC1T14","PC1T15","PC1T16","PC1T17","PC1T18","PC1T19","PC1T20",
               "PC1T21","PC1T22","PC1T23","PC1T24","PC1T25","PC1T26","PC1T27","PC1T28","PC1T29","PC1T30","PC1T31","PC1T32","PC1T33","PC1T34","PC1T35","PC1T36","PC1T37","PC1T38","PC1T39","PC1T40",
               "PC2T1","PC2T2","PC2T3","PC2T4","PC2T5","PC2T6","PC2T7","PC2T8","PC2T9","PC2T10","PC2T11","PC2T12","PC2T13","PC2T14","PC2T15","PC2T16","PC2T17","PC2T18","PC2T19","PC2T20",
               "PC2T21","PC2T22","PC2T23","PC2T24","PC2T25","PC2T26","PC2T27","PC2T28","PC2T29","PC2T30","PC2T31","PC2T32","PC2T33","PC2T34","PC2T35","PC2T36","PC2T37","PC2T38","PC2T39","PC2T40",
               "PC3T1","PC3T2","PC3T3","PC3T4","PC3T5","PC3T6","PC3T7","PC3T8","PC3T9","PC3T10","PC3T11","PC3T12","PC3T13","PC3T14","PC3T15","PC3T16","PC3T17","PC3T18","PC3T19","PC3T20",
               "PC3T21","PC3T22","PC3T23","PC3T24","PC3T25","PC3T26","PC3T27","PC3T28","PC3T29","PC3T30","PC3T31","PC3T32","PC3T33","PC3T34","PC3T35","PC3T36","PC3T37","PC3T38","PC3T39","PC3T40",
               "PC4T1","PC4T2","PC4T3","PC4T4","PC4T5","PC4T6","PC4T7","PC4T8","PC4T9","PC4T10","PC4T11","PC4T12","PC4T13","PC4T14","PC4T15","PC4T16","PC4T17","PC4T18","PC4T19","PC4T20",
               "PC4T21","PC4T22","PC4T23","PC4T24","PC4T25","PC4T26","PC4T27","PC4T28","PC4T29","PC4T30","PC4T31","PC4T32","PC4T33","PC4T34","PC4T35","PC4T36","PC4T37","PC4T38","PC4T39","PC4T40",
               "PC5T1","PC5T2","PC5T3","PC5T4","PC5T5","PC5T6","PC5T7","PC5T8","PC5T9","PC5T10","PC5T11","PC5T12","PC5T13","PC5T14","PC5T15","PC5T16","PC5T17","PC5T18","PC5T19","PC5T20",
               "PC5T21","PC5T22","PC5T23","PC5T24","PC5T25","PC5T26","PC5T27","PC5T28","PC5T29","PC5T30","PC5T31","PC5T32","PC5T33","PC5T34","PC5T35","PC5T36","PC5T37","PC5T38","PC5T39","PC5T40",
               "PC6T1","PC6T2","PC6T3","PC6T4","PC6T5","PC6T6","PC6T7","PC6T8","PC6T9","PC6T10","PC6T11","PC6T12","PC6T13","PC6T14","PC6T15","PC6T16","PC6T17","PC6T18","PC6T19","PC6T20",
               "PC6T21","PC6T22","PC6T23","PC6T24","PC6T25","PC6T26","PC6T27","PC6T28","PC6T29","PC6T30","PC6T31","PC6T32","PC6T33","PC6T34","PC6T35","PC6T36","PC6T37","PC6T38","PC6T39","PC6T40","action","id")
  dd<-rbind.data.frame(dd, mat_new)
  if(i%%50 == 0){
    cat("Iteracion: ", i, ". BBDD con ", nrow(dd)," registros. \n", sep="")
  }
}
return(dd)
}