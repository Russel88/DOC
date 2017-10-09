#' Merge DOC objects and add a Name column, for example for plotting multiple DOCs
#'
#' @param x A named list with outputs from \code{DOC} function.
#' @return A DOC object
#' @export
DOC.merge <- function(x){
  
  new.x <- list()
  for(i in 1:length(x)){
    sub <- x[[i]]
    sub <- lapply(sub, function(k) data.frame(k, Name = names(x)[i])) 
    new.x[[i]] <- sub
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