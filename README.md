
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GeneSelectR

<!-- badges: start -->

[![R-CMD-check](https://github.com/dzhakparov/GeneSelectR-2.0/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dzhakparov/GeneSelectR-2.0/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/GeneSelectR)](https://CRAN.R-project.org/package=GeneSelectR)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

## Overview

<img src="man/figures/GeneSelectR.png" style="float: right; margin: 0px 0px 10px 10px;" width="140px"/>

GeneSelectR is an R package for robust gene selection in
high-dimensional transcriptomic data. It ranks genes by integrating
three complementary dimensions of importance into a single composite
score:

- **Stability (π)** — how consistently a gene is selected across
  repeated cross-validation splits
- **Predictive utility (u)** — the strength of a gene’s association with
  the outcome, combining regularized coefficient magnitude and mutual
  information
- **Biological relevance (b)** — semantic similarity to
  phenotype-specific Gene Ontology terms, literature co-occurrence,
  pathway membership, and optionally biomedical language model
  embeddings

The final gene score is the geometric mean of these normalised
components: **S = (π · u · b)<sup>1/3</sup>**. Genes must score well on
all three axes to rank highly, filtering out features that are
predictive but unstable, stable but biologically orphaned, or
biologically relevant but not individually informative.

## Installation

<!-- Install the stable release from CRAN: -->

Install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("dzhakparov/GeneSelectR-2.0", ref = "v2.0")
```

### Optional dependencies

For biological scoring you will need the following packages:

``` r
# Ontology-based scoring (Reactome, Disease Ontology)
BiocManager::install(c("ReactomePA", "DOSE", "clusterProfiler"))

# Literature mining (PubMed co-occurrence)
install.packages("rentrez")

# LLM-based scoring (BiomedBERT via HuggingFace API)
install.packages("httr2")
# Then set: Sys.setenv(HF_TOKEN = "hf_your_token_here")
```

## Quick Start

``` r
library(GeneSelectR)

# --- Basic usage (GO-based biology score) ---
fit <- geneselectr2_fit(
  X = expression_matrix,       # genes in columns, samples in rows
  y = phenotype_labels,        # factor with two levels
  bio_mode = "targeted",       # user-specified GO priors
  go_terms = c("GO:0006955",   # immune response
               "GO:0045087"),  # innate immune response
  K = 5, R = 10,               # 5-fold CV, 10 repeats
  n_cores = 4
)

# Top genes ranked by composite score
head(fit$gene_scores, 20)
```

## Example

A usage example can be found in this
[`vignette`](https://github.com/dzhakparov/GeneSelectR-2.0/tree/v2.0/vignettes/GeneSelectR2-IMvigor210.html)

**Step 1 — Stability-weighted elastic net.** Gene expression data is
split into K×R outer cross-validation folds. Within each fold, an
elastic-net model with tuned regularization selects a sparse set of
predictive genes. The selection frequency π_g across all folds captures
stability: genes selected in every fold (π = 1) are robust signals;
genes selected sporadically are likely noise or collinearity artifacts.

**Step 2 — Predictive utility scoring.** For each gene, utility combines
(a) the mean absolute elastic-net coefficient across folds and (b)
mutual information with the outcome. The geometric mean of these
percentile-normalised values ensures genes must be both statistically
and information-theoretically associated with the phenotype.

**Step 3 — Biological relevance scoring.** The biology score integrates
the GO similarity score.

**Step 4 — Composite ranking.** The final score S = (π · u ·
b)<sup>1/3</sup> ranks genes by their joint strength across all three
dimensions. Users can extract top-k panels at any cutoff for downstream
validation.

## Biology Scoring Modes

GeneSelectR supports two biology modes plus standalone scoring:

**Targeted mode** (`bio_mode = "targeted"`): User specifies GO terms
reflecting prior knowledge (e.g., immune response terms for an
immunology study). Best when domain knowledge is available.

**Unsupervised mode** (`bio_mode = "datadriven"`): Target GO terms are
identified automatically by enrichment analysis of high-stability,
high-utility genes. Useful for exploratory studies without strong
priors.

## Benchmarking

GeneSelectR 2.0 was benchmarked against six established methods (DGE
t-test, LASSO, mRMR, Boruta, RF importance) on two clinical RNA-seq
datasets:

- **SOS-ALL** — atopic dermatitis in South African children (n = 149)
- **IMvigor210** — anti-PD-L1 immunotherapy response in metastatic
  urothelial carcinoma (n = 192; Mariathasan et al. 2018)

GeneSelectR 2.0 was the top-performing method at every panel size tested
in both datasets. In the immunotherapy cohort, it achieved AUC ≈ 0.74
with just 10 genes — a level no other method matched even with 500.

Synthetic validation (20,000-gene simulations) confirmed that the
underlying elastic-net component detects \>88% of true DE genes at
moderate effect sizes, with five- to seven-fold stability improvements
over LASSO under gene correlation.

Full benchmarking scripts are available in the:
<!-- [`benchmarks/`](https://github.com/dzhakparov/GeneSelectR-2.0/tree/v2.0/benchmarks) -->
<!-- directory. -->

## Citation

If you use GeneSelectR 2.0 in your research, please cite:

> Zhakparov D, Moriarty K, Roqueiro D, Baerenfaller K. GeneSelectR:
> Integrating Stability, Utility, and Biological Relevance for Robust
> Feature Selection in High-Dimensional Biomedical Datasets.
> *\[journal\]*, 2026. doi: \[pending\]

## Contributing

Feedback, bug reports, and pull requests are welcome. Please open an
[issue](https://github.com/dzhakparov/GeneSelectR-2.0/issues).
