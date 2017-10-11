#' Wrapper to run a DOC analysis with null models
#'
#' @param otu An OTU-table (taxa as rows)
#' @param N Number of null models. Is 1 by default as in the orignal Nature paper.
#' @param non.zero Shuffle only non-zero (TRUE) or all values (FALSE)
#' @param ... Arguments for the \code{DOC} function
#' @return A list with results from \code{DOC} function. One element for each N
#' @import foreach
#' @export
DOC.null <- function(otu,N=1,non.zero=TRUE,...){
  
  docs <- foreach(i = 1:N) %do% {
    
    # Make NULL
    otunull <- as.data.frame(DOC.otunull(otu,non.zero=non.zero))
    
    # Run DOC
    doc <- DOC(otunull, R=3,cores=3)
    
    return(doc)
  }
  
  names(docs) <- paste0("NULL.",1:N)

  doc.final <- DOC.merge(docs)
  
  return(doc.final)
} 

