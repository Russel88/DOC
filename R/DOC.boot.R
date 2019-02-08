#' Bootstraps for loess, negative slope and fns
#'
#' @param do Output from \code{DOC.do} function
#' @param R Number of bootstraps
#' @param subr If NULL will do bootstrap, alternatively an integer denoting size of subsample
#' @param pair A vector of length two with names of pairs
#' @param mov.avg Moving average window to use for estimating where negative slope starts
#' @param span Span of loess smoothing
#' @param degree Degree of loess smoothing (If 1 linear, >1 polynomial)
#' @param family "gaussian" is least-squares fitting, "symmetric" is robust fitting
#' @param iterations Number of iterations for robust fitting
#' @param surface "direct" estimation (slow exact) or "interpolate" estimation (fast approximate)
#' @param cores Number of cores to use
#' @return Output: A list with:
#' \itemize{
#'   \item A dataframe of loess for each bootstrap,
#'   \item a numeric with estimated slope from lme on data subsetted to Overlap values above median for each bootstrap,
#'   \item a numeric with estimated point of where negative slope starts (moving average of loess used) for each bootstrap,
#'   \item and a numeric with estimated Fns value for each bootstrap
#' }
#' @import foreach snow doSNOW lme4
#' @export

DOC.boot <- function(do,R=100,subr=NULL,pair=NULL,mov.avg=5,span=0.2,degree=1,family="symmetric",iterations=4,surface="interpolate",cores=1){
  
  # Subset and margin names
  OL <- do[[1]]
  DIS <- do[[2]]
  
  rownames(OL) <- 1:nrow(OL)
  rownames(DIS) <- 1:nrow(DIS)
  colnames(OL) <- 1:ncol(OL)
  colnames(DIS) <- 1:ncol(DIS)
  
  # Overlap values for loess prediction
  xs <- seq(0,1,by=0.001)
  
  # Start parallel
  if(cores == 1){
    registerDoSEQ()
  } else {
    cl <- makeCluster(cores)
    registerDoSNOW(cl)
  }
  
  message("Running bootstraps")
  
  # Progress bar
  pb <- txtProgressBar(max = R, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  i <- NULL
  llboot <- foreach(i = 1:R,.options.snow = opts, .export = "mov.avg") %dopar% {
    
    if(is.null(pair)){
      
      # Sample subjects
      if(is.null(subr)){
        Samp <- sample(rownames(OL),replace=TRUE)
      } else {
        Samp <- sample(rownames(OL),subr,replace=FALSE)
      }
      
      # Subset
      OL.sub <- OL[Samp,Samp] 
      DIS.sub <- DIS[Samp,Samp]
      
      # Vectorize
      OL.tri <- OL.sub[upper.tri(OL.sub)]
      DIS.tri <- DIS.sub[upper.tri(DIS.sub)]
      
    } else {
      
      # Sample subjects
      if(is.null(subr)){
        Sampr <- sample(rownames(OL),replace=TRUE)
        Sampc <- sample(colnames(OL),replace=TRUE)
      } else {
        Sampr <- sample(rownames(OL),subr,replace=FALSE)
        Sampc <- sample(colnames(OL),subr,replace=FALSE)
      }
      
      
      # Subset
      OL.sub <- OL[Sampr,Sampc] 
      DIS.sub <- DIS[Sampr,Sampc]
      
      # Vectorize
      OL.tri <- as.numeric(OL.sub)
      DIS.tri <- as.numeric(DIS.sub)
      
    }
    
    # To data frame
    DF.l <- data.frame(y=c(DIS.tri),x=c(OL.tri)) 
    
    # Lowess
    LOW <- loess(y~x,data=DF.l,span=span,degree=degree,family=family,iterations=iterations,surface=surface)
    LOW.P <- data.frame(predict(LOW,xs))
    colnames(LOW.P) <- paste("rJSD Boot",i)
    
    # Data frame for lme (slope)
    rowCol <- expand.grid(rownames(OL.sub), colnames(OL.sub))
    labs <- rowCol[as.vector(upper.tri(OL.sub,diag=F)),]
    tris <- cbind(labs, OL.sub[upper.tri(OL.sub,diag=F)],DIS.sub[upper.tri(DIS.sub,diag=F)])
    colnames(tris) <- c("Row","Col","OL","DIS")
    Tris <- as.data.frame(na.omit(tris))
   
    # Remove data with Overlap below median
    Tris.sub <- Tris[Tris$OL >= median(OL.sub,na.rm=T),]
    
    # LME
    fit <- lmer(DIS~OL+(1|Row),data=Tris.sub)
    Est <- coef(summary(fit))[ , "Estimate"][2]

    ## Detect negative slope
    # Smooth prediction
    ma <- function(x,n=mov.avg){filter(x,rep(1/n,n), sides=2)}
    low.ma <- ma(LOW.P[,1])
    slope <- diff(low.ma)/diff(xs)
    point <- which(slope>0)
    neg.slope <- xs[point[length(point)]]
    
    # Fns
    Fns <- sum(OL.tri>neg.slope,na.rm=T)/length(OL.tri)
    
    Final <- list(LOW.P,Est,neg.slope,Fns)
    
    return(Final)
  }
  
  if(cores > 1){
    stopCluster(cl)
    registerDoSEQ()
  }
  
  
  # Extract and bind lowess, lme, negative slope and Fns seperately
  LOWES <- do.call(cbind,lapply(llboot, function(x) x[[1]]))
  LME <- do.call(cbind,lapply(llboot, function(x) x[[2]]))
  NEG <- do.call(cbind,lapply(llboot, function(x) x[[3]]))
  FNS <- do.call(cbind,lapply(llboot, function(x) x[[4]]))
  
  LOWES <- cbind(xs,LOWES)
  colnames(LOWES)[1] <- "Overlap"
  
  Final <- list(LOWES,as.numeric(LME),as.numeric(NEG),as.numeric(FNS))
  
  return(Final)
  
}
