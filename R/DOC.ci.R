#' Get bootstrap Confidence Intervals
#'
#' @param boots Output from \code{DOC.boot} function
#' @param ci Vector with quantiles
#' @return A dataframe with quantiles of bootstraped loess
#' @export
DOC.ci <- function(boots,ci=c(0.025,0.5,0.975)){
  
  lowp <- boots[[1]]
  rjsd <- lowp[,2:ncol(lowp)]
  
  cis <- t(apply(rjsd,1,function(x) quantile(x,ci,na.rm=T)))
  
  LCI <- as.data.frame(cbind(boots[[1]][,1],cis))
  LCI <- na.omit(LCI)
  colnames(LCI) <- c("Overlap",colnames(cis))
  return(LCI)
}