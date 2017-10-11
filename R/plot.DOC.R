#' Plot results from DOC analysis
#'
#' @param x Output from \code{DOC} function
#' @export
plot.DOC <- function(x){
  
  library(ggplot2)
  
  if("Name" %in% colnames(x$DO)){
  
    x$nm <- aggregate(Neg.Slope ~ Data + Name, data = x$NEG, median)
    
    if(length(unique(rowSums(as.data.frame.matrix(table(x$DO$Data,x$DO$Name))))) == 1){
      
      if(all(x$DO$Data == "Real")){
        p <- ggplot(x$DO,aes(x=Overlap)) +
          theme_bw() + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.position = "none") +
          ylab("Dissimilarity") +
          facet_grid(.~Name) +
          stat_density(alpha=0.2, aes(y=..scaled..), colour = "black") +
          geom_ribbon(data=x$CI,aes(x=Overlap,ymin=get(colnames(x$CI)[2]),ymax=get(colnames(x$CI)[4])),alpha=0.5, fill = "grey", colour = NA) +
          geom_line(data=x$CI,aes(x=Overlap,y=get(colnames(x$CI)[3])),size = 1, colour = "red") +
          geom_vline(data=x$nm,aes(xintercept=Neg.Slope)) +
          coord_cartesian(ylim = c(0,1)) 
      } else {
        p <- ggplot(x$DO,aes(x=Overlap)) +
          theme_bw() + 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.position = "none") +
          ylab("Dissimilarity") +
          facet_grid(Data~.) +
          stat_density(alpha=0.2, aes(y=..scaled..), colour = "black") +
          geom_ribbon(data=x$CI,aes(x=Overlap,ymin=get(colnames(x$CI)[2]),ymax=get(colnames(x$CI)[4])),alpha=0.5, fill = "grey", colour = NA) +
          geom_line(data=x$CI,aes(x=Overlap,y=get(colnames(x$CI)[3])),size = 1, colour = "red") +
          geom_vline(data=x$nm,aes(xintercept=Neg.Slope)) +
          coord_cartesian(ylim = c(0,1)) 
      }
      
    } else {
      
      p <- ggplot(x$DO,aes(x=Overlap)) +
        theme_bw() + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none") +
        ylab("Dissimilarity") +
        facet_grid(Data~Name) +
        stat_density(alpha=0.2, aes(y=..scaled..), colour = "black") +
        geom_ribbon(data=x$CI,aes(x=Overlap,ymin=get(colnames(x$CI)[2]),ymax=get(colnames(x$CI)[4])),alpha=0.5, fill = "grey", colour = NA) +
        geom_line(data=x$CI,aes(x=Overlap,y=get(colnames(x$CI)[3])),size = 1, colour = "red") +
        geom_vline(data=x$nm,aes(xintercept=Neg.Slope)) +
        coord_cartesian(ylim = c(0,1)) 
      
    }
    
  } else {
    
    x$nm <- aggregate(Neg.Slope ~ Data, data = x$NEG, median)

    p <- ggplot(x$DO,aes(x=Overlap)) +
      theme_bw() + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none") +
      ylab("Dissimilarity") +
      facet_grid(Data~.) +
      stat_density(alpha=0.2, aes(y=..scaled..), colour = "black") +
      geom_ribbon(data=x$CI,aes(x=Overlap,ymin=get(colnames(x$CI)[2]),ymax=get(colnames(x$CI)[4])),alpha=0.5, fill = "grey", colour = NA) +
      geom_line(data=x$CI,aes(x=Overlap,y=get(colnames(x$CI)[3])),size = 1, colour = "red") +
      geom_vline(data=x$nm,aes(xintercept=Neg.Slope)) +
      coord_cartesian(ylim = c(0,1)) 
    
  }
  
  p
  
}