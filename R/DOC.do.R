#' Dissimilarity and Overlap
#'
#' @param otu Normalized otu-table (taxa as rows and samples sum to 1)
#' @param pair A vector of length two with names of pairs mathcing colnames in \code{otu}
#' @return A list with; a matrix of Overlap, a matrix of rJSD (Dissimilarities), and a dataframe with Overlap and rJSD as vectors
#' @export

DOC.do <- function(otu, pair = NULL){
  
  Samples <- ncol(otu)
  
  Mat.Overlap <- matrix(NA,ncol=Samples,nrow=Samples)
  Mat.rJSD <- matrix(NA,ncol=Samples,nrow=Samples)
  
  for(i in 1:(Samples-1)){
    for(j in (i+1):Samples){
      
      A <- otu[,c(i,j)]
      
      # Shared species
      Shared <- which(A[,1] > 0 & A[,2] > 0)
      
      # Overlap
      Overlap <- sum(0.5*(A[Shared,1]+A[Shared,2]))
      
      # Renormalize
      Renorm.i <- A[Shared,1]/sum(A[Shared,1])
      Renorm.j <- A[Shared,2]/sum(A[Shared,2])
      
      # rJSD
      RootJSD <- DOC.rjsd(Renorm.i,Renorm.j)
      
      # Insert in Matrices
      Mat.Overlap[i,j] <- Overlap
      Mat.rJSD[i,j] <- RootJSD
      
    }
  }
  
  if(!is.null(pair)) {
    
    pairv <- colnames(otu)
    pairv <- gsub(paste0(".*",pair[1],".*"), pair[1], pairv)
    pairv <- gsub(paste0(".*",pair[2],".*"), pair[2], pairv)
    if(length(unique(pairv)) != 2) {
      stop("Names of pairs do not match column names")
    }
    
    rownames(Mat.Overlap) <- pairv
    colnames(Mat.Overlap) <- pairv
    Mat.Overlap <- Mat.Overlap[rownames(Mat.Overlap) %in% pair[1],colnames(Mat.Overlap) %in% pair[2]]
    
    rownames(Mat.rJSD) <- pairv
    colnames(Mat.rJSD) <- pairv
    Mat.rJSD <- Mat.rJSD[rownames(Mat.rJSD) %in% pair[1],colnames(Mat.rJSD) %in% pair[2]]
    
    DF.list <- list(Mat.Overlap,Mat.rJSD)
    DF <- data.frame(Overlap=c(DF.list[[1]]),rJSD=c(DF.list[[2]]))
    DF <- DF[!is.na(DF$Overlap),]
    
    List <- list(Mat.Overlap,Mat.rJSD,DF)
    
  } else {
    
    DF.list <- list(Mat.Overlap,Mat.rJSD)
    DF <- data.frame(Overlap=c(DF.list[[1]]),rJSD=c(DF.list[[2]]))
    DF <- DF[!is.na(DF$Overlap),]
    
    Mat.Overlap.t <- t(Mat.Overlap)
    Mat.rJSD.t <- t(Mat.rJSD)
    
    Mat.Overlap[is.na(Mat.Overlap)] <- 0
    Mat.Overlap.t[is.na(Mat.Overlap.t)] <- 0
    Mat.rJSD[is.na(Mat.rJSD)] <- 0
    Mat.rJSD.t[is.na(Mat.rJSD.t)] <- 0
    
    Mat.Overlap.new <- Mat.Overlap + Mat.Overlap.t
    Mat.rJSD.new <- Mat.rJSD + Mat.rJSD.t
    
    diag(Mat.Overlap.new) <- NA
    diag(Mat.rJSD.new) <- NA
    
    List <- list(Mat.Overlap.new,Mat.rJSD.new,DF)
    
  }
    
  return(List)
}

