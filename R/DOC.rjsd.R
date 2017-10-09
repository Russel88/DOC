#' root Jensen-Shannon divergence
#'
#' root Jensen-Shannon divergence between two numeric vectors
#' @param x,y Numeric vectors
#' @keywords rJSD divergence dissimilarity
#' @return A numeric
#' @export

DOC.rjsd <- function(x,y){
  z <- 0.5 * (x + y) 
  rJS <- sqrt(0.5 * (sum(x * log(x / z), na.rm=T) + sum(y * log(y / z), na.rm=T)))
  return(rJS)
}
