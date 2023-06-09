
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GeneSelectR

<!-- badges: start -->
<!-- badges: end -->

`GeneSelectR` is a machine learning-based R package developed to enhance
feature selection and biological assessment in RNAseq analysis of
complex biological datasets. Traditional RNAseq datasets can be
challenging to analyze due to their high dimensionality. The standard
differential gene expression analysis approach has been found to have
limitations such as a high false-positive rate and limited gene
coverage. The `GeneSelectR` package aims to overcome these limitations
using machine learning techniques.

![Alt text](./vignettes/images/package-scheme.png)

## Installation

You can install the development version of GeneSelectR from
[GitHub](https://github.com/) with:

NB! Prior to installation you should run this:

``` r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
#> Bioconductor version '3.16' is out-of-date; the current release version '3.17'
#>   is available with R version '4.3'; see https://bioconductor.org/install

BiocManager::install("ViSEAGO")
#> Bioconductor version 3.16 (BiocManager 1.30.20), R 4.2.2 (2022-10-31 ucrt)
#> Warning: package(s) not installed when version(s) same as or greater than current; use
#>   `force = TRUE` to re-install: 'ViSEAGO'
#> Old packages: 'areaplot', 'cachem', 'class', 'cli', 'clock', 'DiagrammeR',
#>   'dplyr', 'DT', 'fs', 'future.apply', 'ggnewscale', 'graphlayouts', 'httpuv',
#>   'httr2', 'igraph', 'influenceR', 'KernSmooth', 'later', 'lattice', 'lme4',
#>   'MASS', 'Matrix', 'nnet', 'parallelly', 'pROC', 'profvis', 'RcppArmadillo',
#>   'recipes', 'rlang', 'scatterpie', 'testthat', 'tibble', 'tidymodels', 'tzdb',
#>   'viridis', 'vroom', 'waldo', 'XML', 'xml2', 'yardstick'
```

``` r
# install.packages("devtools")
devtools::install_github("dzhakparov/GeneSelectR")
```

## Usage

The `GeneSelectR` package uses four machine learning methods for feature
selection:

1.  Recursive Feature Elimination
2.  Boruta
3.  Lasso Regression
4.  Univariate Filtering

In addition, the package also performs Gene Ontology (GO) enrichment to
assess the biological relevance of gene lists. Semantic similarity
analysis of the GO lists is performed using Wang distance and binary cut
clustering. The package then selects the best list based on
cross-validation mean metrics scores.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# An example of usage:
#library(GeneSelectR)
# load your data
#data <- read.csv("YourData.csv")
# perform feature selection
#selected_features <- feature_select(data, method = "lasso")
# perform GO enrichment analysis
#go_enrichment <- go_enrich(selected_features)
# perform semantic similarity analysis
#semantic_analysis <- semantic_sim(go_enrichment)
# select the best list
#best_list <- select_best(semantic_analysis)
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

![Alt text](./vignettes/images/hmap_jaccard_GO.png) ![Alt
text](./vignettes/images/union_0_feature_selection_mean_cv.png) ![Alt
text](./vignettes/images/hmap_GO_clusters.png)

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.
