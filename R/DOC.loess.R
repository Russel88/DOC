#' Calculate loess on the real data
#'
#' @param do Output from DO function
#' @param span Span of loess smoothing
#' @param degree Degree of loess smoothing (If 1 linear, >1 polynomial)
#' @param family "gaussian" is least-squares fitting, "symmetric" is robust fitting
#' @param iterations Number of iterations for robust fitting
#' @param surface "direct" estimation (slow exact) or "interpolate" estimation (fast approximate)
#' @return A dataframe with loess results from the non-bootstrapped data
#' @export
DOC.loess <- function(do,span=0.2,degree=1,family="symmetric",iterations=4,surface="interpolate"){
  
  # Subset
  OL <- do[[1]]
  DIS <- do[[2]]
  
  # Overlap values for loess prediction
  xs <- seq(0,1,by=0.001)
    
  # Vectorize
  OL.tri <- OL[upper.tri(OL)]
  DIS.tri <- DIS[upper.tri(DIS)]

  OL.tri <- OL.tri[!is.na(OL.tri)]
  DIS.tri <- DIS.tri[!is.na(DIS.tri)]
  
  # To data frame
  DF.l <- data.frame(y=c(DIS.tri),x=c(OL.tri)) 
  
  # Lowess
  LOW <- loess(y~x,data=DF.l,span=span,degree=degree,family=family,iterations=iterations,surface=surface)
  LOW.P <- data.frame(xs,predict(LOW,xs))
  LOW.P <- na.omit(LOW.P)
  colnames(LOW.P) <- c("Overlap","LOWESS")
  
  return(LOW.P)
}