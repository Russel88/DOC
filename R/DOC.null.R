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
    doc <- DOC(otunull, ...)
    
    return(doc)
  }
  
  names(docs) <- paste0("Null.",1:N)

  new.x <- list()
  for(i in 1:length(docs)){
    sub <- docs[[i]]
    subs <- list()
    for(j in 1:length(sub)){
      sub.sub <- sub[[j]]
      sub.sub$Data <- names(docs)[i]
      subs[[j]] <- sub.sub
    }
    names(subs) <- c("DO","LME","LOWESS","NEG","FNS","BOOT","CI")
    new.x[[i]] <- subs
  }
  
  DOs <- do.call(rbind,lapply(new.x, function(k) k$DO))
  LMEs <- do.call(rbind,lapply(new.x, function(k) k$LME))
  LOWESSs <- do.call(rbind,lapply(new.x, function(k) k$LOWESS))
  NEGs <- do.call(rbind,lapply(new.x, function(k) k$NEG))
  FNSs <- do.call(rbind,lapply(new.x, function(k) k$FNS))
  BOOTs <- do.call(rbind,lapply(new.x, function(k) k$BOOT))
  CIs <- do.call(rbind,lapply(new.x, function(k) k$CI))
  
  final <- list(DO = DOs,
                LME = LMEs,
                LOWESS = LOWESSs,
                NEG = NEGs,
                FNS = FNSs,
                BOOT = BOOTs,
                CI = CIs)
  
  class(final) <- "DOC"
  
  return(final)
} 

