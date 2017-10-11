#' A wrapper to run the whole DOC analysis
#'
#' @param otu An OTU-table (taxa as rows)
#' @param R Number of bootstraps
#' @param subr If NULL will do bootstrap, alternatively an integer denoting size of subsample
#' @param pair A vector of length two with names of pairs mathcing colnames in \code{otu}
#' @param mov.avg Moving average window to use for estimating where negative slope starts
#' @param ci Vector with quantiles for confidence intervals
#' @param span Span of loess smoothing
#' @param degree Degree of loess smoothing (If 1 linear, >1 polynomial)
#' @param family "gaussian" is least-squares fitting, "symmetric" is robust fitting
#' @param iterations Number of iterations for robust fitting
#' @param surface "direct" estimation (slow exact) or "interpolate" estimation (fast approximate)
#' @param cores Number of cores to use
#' @return Output: A list with class DOC:
#' \itemize{
#'   \item DO: A dataframe with Overlap and Dissimilarity (rJSD)
#'   \item LME: A dataframe with estimates of negative slope
#'   \item LOWESS: A dataframe with loess(lowess) results of non-bootstraped data
#'   \item NEG: A dataframe with Overlap values where negative slope starts
#'   \item FNS: A dataframe with Fns values
#'   \item BOOT: A dataframe with loess results for each bootstrap
#'   \item CI: A dataframe with quantiles for the BOOT dataframe
#' }
#' @import foreach snow doSNOW
#' @export
DOC <- function(otu,R=100,subr=NULL,pair=NULL,mov.avg=5,ci=c(0.025,0.5,0.975),span=0.2,degree=1,family="symmetric",iterations=4,surface="interpolate",cores=1){
  
  if(!is.null(pair)){
    if(length(pair) != 2) stop("There should only be two names in pair!")
  }
  
  # Normalize OTU-table
  otun <- apply(otu,2,function(x) as.numeric(x)/sum(as.numeric(x)))
  
  # Dissimilarity and Overlap
  if(!is.null(pair)){ 
    Dis.Over <- DOC.do(otun,pair=pair) 
    } else {
    Dis.Over <- DOC.do(otun)
  }
  
  # Bootstrap lowess and lme
  Bootstrap <- DOC.boot(Dis.Over,R=R,subr=subr,pair=pair,mov.avg=mov.avg,span=span,degree=degree,family=family,iterations=iterations,surface=surface,cores=cores)
  
  # LOWESS CI
  LCIS <- DOC.ci(Bootstrap,ci=ci)
  
  # LOWESS no bootstrap
  LOWESS <- DOC.loess(Dis.Over,pair=pair,span=span,degree=degree,family=family,iterations=iterations,surface=surface)
  
  # LME
  LME <- as.data.frame(Bootstrap[[2]])
  colnames(LME) <- "Slope"
  
  # Negative slope
  NEG <- as.data.frame(Bootstrap[[3]])
  colnames(NEG) <- "Neg.Slope"
  
  # Fns
  FNS <- as.data.frame(Bootstrap[[4]])
  colnames(FNS) <- "Fns"
  
  # Final
  Final <- list(DO=Dis.Over[[3]],LME=LME,LOWESS=LOWESS,NEG=NEG,FNS=FNS,BOOT=Bootstrap[[1]],CI=LCIS)
  Final <- lapply(Final, function(k) data.frame(k, Data = "Real")) 
  class(Final) <- "DOC"
  return(Final)
  
}