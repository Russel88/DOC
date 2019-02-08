#' Make null matrix
#'
#' @param otu OTU-table with taxa as rows
#' @param non.zero Shuffle only non-zero (TRUE) or all values (FALSE)
#' @return Shuffled OTU-table
#' @import vegan
#' @export
DOC.otunull <- function(otu,non.zero=TRUE){
  
  if(non.zero==TRUE){
    # Shuffle all non-zero values
    otu.null <- as.data.frame(otu)
    i <- NULL
    otu.null1 <- foreach(i = 1:nrow(otu.null),.combine=rbind) %do% {
      temp <- otu.null[i,]
      if(length(temp[temp != 0])>1) temp[temp != 0] <- sample(temp[temp != 0])
      return(temp)
    }
    
  }
  
  if(non.zero==FALSE){
    # Shuffle all values
    otu.null1 <- permatfull(as.data.frame(otu),fixedmar="none",shuffle="samp",times=1)$perm[[1]]
  }
  
  return(otu.null1)  
}  
