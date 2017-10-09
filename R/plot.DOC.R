#' Plot results from DOC analysis
#'
#' @param x Output from \code{DOC} function
#' @export
plot.DOC <- function(x){
  
  library(ggplot2)
  
  if("Name" %in% colnames(x$DO)){
    
    p <- ggplot(x$DO,aes(x=Overlap)) +
      theme_bw() + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      ylab("rJSD") +
      facet_wrap(~Name) +
      stat_density(alpha=0.2, aes(y=..scaled..)) +
      geom_ribbon(data=x$CI,aes(x=Overlap,ymin=get(colnames(x$CI)[2]),ymax=get(colnames(x$CI)[4])),fill="blue",alpha=0.2) +
      geom_line(data=x$CI,aes(x=Overlap,y=get(colnames(x$CI)[3])),colour="red") +
      geom_vline(data=x$NEG,aes(xintercept=median(Neg.Slope)),colour="black")
    
  } else {
    
    p <- ggplot(x$DO,aes(x=Overlap)) +
      theme_bw() + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      ylab("rJSD") +
      stat_density(alpha=0.2, aes(y=..scaled..)) +
      geom_ribbon(data=x$CI,aes(x=Overlap,ymin=get(colnames(x$CI)[2]),ymax=get(colnames(x$CI)[4])),fill="blue",alpha=0.2) +
      geom_line(data=x$CI,aes(x=Overlap,y=get(colnames(x$CI)[3])),colour="red") +
      geom_vline(data=x$NEG,aes(xintercept=median(Neg.Slope)),colour="black")
    
  }
  
  p
  
}