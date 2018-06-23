# Making 'Null Effects' Informative

This repository contains the accompanying example data and analysis scripts for the manuscript "Making 'Null Effects' Informative: Statistical Techniques and Inferential Frameworks" by Harms & Lakens (pre-print available at [https://psyarxiv.com/48zca](https://psyarxiv.com/48zca)).

## Contents
This repository consists of the following:

* `Manuscript`: This folder holds the *RMarkdown* manuscript for the paper (see requirements below).
* `Example Data`: The simulated data for the study detailed in the manuscript as CSV file.
* `Analysis`: Example analysis scripts for R, JASP and jamovi (see requirements below).
* `R-scripts`: Contains supplementary scripts needed for calculations in R (i.e. the Bayes factor with the informed alternativ model) and code for Figure 4.

## Requirements
To run the example analysis in R you need to install the following packages:
```
install.packages(c('pwr', 'TOSTER', 'BEST', 'dplyr', 'hypergeo', 'ggplot2'))
```

For the manuscript to be compiled into a PDF you need the following additional R packages along with an available LaTeX distribution (see [installation instructions](https://github.com/crsh/papaja) for `papaja`):
```
install.packages(c('knitr', 'ggbeeswarm', 'devtools'))
devtools::install_github('crsh/papaja')
```

For the TOST equivalence test in the jamovi example, you need to install the TOSTER module (available through the jamovi library).