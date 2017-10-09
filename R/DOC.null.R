#' Wrapper to run a DOC analysis with null models
#'
#' @param otu An OTU-table (taxa as rows)
#' @param N Number of null models. Is 1 by default as in the orignal paper.
#' @param non.zero Shuffle only non-zero (TRUE) or all values (FALSE)
#' @param ... Arguments for the \code{DOC} function
#' @return A list with results from \code{DOC} function. One element for each N
#' @export
DOC.null <- function(otu,N=1,non.zero=TRUE,...){
  
  docs <- foreach(i = 1:N,.options.snow = opts) %do% {
    
    # Make NULL
    otunull <- as.data.frame(DOC.otunull(otu,non.zero=non.zero))
    
    # Run DOC
    doc <- DOC(outnull, ...)
    
    return(doc)
  }
  
  return(docs)
} 
