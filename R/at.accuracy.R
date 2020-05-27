at.accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

#' at.accuracy function
#'
#' This function computes the accuracy of a confussion matrix
#' @param x Table where perform accuracy computation
#' @keywords accuracy
#' @export
#' @examples
#' at.accuracy()
#'

at.accuracy<-function(x)
{
 sum(diag(x)/(sum(rowSums(x)))) * 100
}