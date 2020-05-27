#' at.generator function
#'
#' This function allows you to create a temporal window with a generator. It is useful
#' for example in order to apply then deep learning models such as GRU or LSTM.
#' @param data Matrix of data where the last column is the target variable
#' @param lookback Time steps that the window should look before
#' @param Delay Time steps that the window should after before in order to put the correct action
#' @param min_index, max_index Data points earlier than min_index or later than max_index will not be used in the output sequences. This is useful to reserve part of the data for test or validation.
#' @param shuffle Data should be shuffled or not? (FALSE by default)
#' @param batch_size Quantity of rows for each generator iteration
#' @param step Period between successive individual timesteps within sequences. For rate r, timesteps data[i],
#' data[i-r], ... data[i - length] are used for create a sample sequence.
#' @keywords pca reconstruct
#' @export
#' @examples
#' at.generator()
#'
at.generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size, step = 1) {
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
                                (dim(data)[[-1]])-1))
    targets <- array(0, dim = c(length(rows),6))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
                     length.out = dim(samples)[[2]])
      samples[j,,] <- as.numeric(as.character(data[indices,-ncol(data)]))
      a<-data[rows[[j]] + delay,ncol(data)]
      targets[j,(as.numeric(a)+1)] <- 1
    }           
    lista<-list(samples, targets)
		return(lista)
  }
}
