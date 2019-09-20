[![Travis Build
Status](https://travis-ci.org/Russel88/DOC.svg?branch=master)](https://travis-ci.org/Russel88/DOC)

DOC: Dissimilarity Overlap Curve analysis
=========================================

This is an implementation of the DOC analysis proposed by [Bashan et al.
2016 Universality of human microbial dynamics. Nature
534](http://www.nature.com/nature/journal/v534/n7606/abs/nature18301.html?foxtrotcallback=true)

#### This implementation is different from the original in one important aspect:

Instead of calculating the Fns value only once on the observed data, an
Fns value is calculated for each bootstrap realization making it
possible to asses the robustness of the measure. Besides this, I inteded
to mimic the orignal Matlab code as much as possible, although note that
the robust lo(w)ess algorithms in R and Matlab might give slightly
different results.

#### Citation of this software:

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1169219.svg)](https://doi.org/10.5281/zenodo.1169219)

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

The vertical line is the median Overlap at which the fitted line has a negative slope.

### DOC for null model

    results.null <- DOC.null(otu)

### Merge two or more DOC objects, and plot the whole thing

    # The names (res1, res2, and res3) will appear in the plot
    merged <- DOC.merge(list(res1 = results1, res2 = results2, res3 = results3))
    plot(merged)

    # If you merge a "real"" analysis with a "null" give them the same name:
    merged <- DOC.merge(list(MyResults = results, MyResults = results.null))
    plot(merged)
