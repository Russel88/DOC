DOC: Dissimilarity Overlap Curve analysis
=========================================

This is an implementation of the DOC analysis proposed by [Bashan et al.
2016 Universality of human microbial dynamics. Nature
534](http://www.nature.com/nature/journal/v534/n7606/abs/nature18301.html?foxtrotcallback=true)

The function runs in parallel, set the `cores` argument to enable this.

### Installation

    library(devtools)
    install_github("Russel88/DOC")

    # Plotting requires the ggplot2 package
    install.packages("ggplot2")

### Run the analysis

    results <- DOC(otu)

The input is an OTU-table with taxa as rows

### Plot the result

    plot(results)

The vertical line is the median Fns value from the bootstraps

### DOC for null model

    results.null <- DOC.null(otu)

### Merge two DOC objects, and plot the whole thing

    merged <- DOC.merge(list(real = results, null = results.null))
    plot(merged)
