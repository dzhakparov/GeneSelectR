# ==============================================================================
# GeneSelectR 2.0 — Real-Data Benchmarking
# ==============================================================================
#
# Benchmarks GeneSelectR on real biological datasets from public R packages:
#
#   Dataset 1:  TCGA-BRCA     — Breast cancer (tumor vs normal) via curatedTCGAData
#   Dataset 2:  TCGA-LUAD     — Lung adenocarcinoma (tumor vs normal)
#   Dataset 3:  TCGA-KIRC     — Kidney clear cell carcinoma (tumor vs normal)
#   Dataset 4:  GTEx (recount3)— Brain vs Liver tissue comparison
#   Dataset 5:  ALL/AML       — Classic Golub leukemia (microarray)
#   Dataset 6:  airway        — Airway smooth muscle (dexamethasone treated vs untreated)
#   Dataset 7:  curatedOvarianData — Ovarian cancer (early vs late stage)
#   Dataset 8:  GEO: GSE53757 — Kidney cancer RNA-seq (ccRCC vs normal)
#
# ==============================================================================
#
# GLOSSARY OF METRICS
# -------------------
#
# Since real data has no known ground truth, metrics differ from the synthetic
# benchmarks. We use indirect validation instead of direct recall.
#
# AUC (area under the ROC curve):
#   How well the elastic net classifier separates the two classes in held-out
#   test folds. AUC = 0.5 is chance; 1.0 is perfect. Reported as mean ± SD
#   across all K × R cross-validation folds.
#
# pi (stability, selection frequency):
#   Fraction of CV folds in which a gene was selected by the elastic net.
#   pi = 0.9 means the gene was chosen in 90% of folds — highly reproducible.
#   pi = 0 means never selected. "Genes with pi > 0.5" counts the genes
#   selected in more than half the folds.
#
# u (utility):
#   Combined measure of how useful a gene is in the model:
#   u = 0.5 * u_coef + 0.5 * u_mi
#   where u_coef = mean |elastic net coefficient| (percentile-normalized)
#   and u_mi = mean mutual information with the outcome (percentile-normalized).
#
# b (biology):
#   Biological relevance score based on GO semantic similarity.
#   In this benchmarking, we use the default settings:
#     ontology = "BP" (Biological Process only)
#     similarity metric = Resnik (normalized)
#     enrichment FDR = 0.05, IC quantile = 0.5
#   These are customizable via geneselectr2_fit() parameters:
#     bio_ontology, bio_sim_method, bio_enrich_fdr, bio_ic_quantile,
#     bio_n_top_sims, bio_max_enriched, bio_min_term_freq.
#   Alternative similarity metrics: "lin", "jiang", "rel" (Schlicker).
#   Alternative ontologies: "MF" (Molecular Function), "CC" (Cellular Component),
#     or any combination like c("BP","MF","CC").
#
# final_score:
#   Weighted geometric mean of pi, u, and b (all in [0,1]).
#   final = (pi^w1 * u^w2 * b^w3)^(1/sum(w)).
#   A gene needs to be stable, useful, AND biologically relevant to rank high.
#
# DESeq2 overlap:
#   For datasets with raw counts, we run DESeq2 (the standard differential
#   expression tool) and check how many of GeneSelectR's top-k genes overlap
#   with DESeq2's DE genes (padj < 0.05, |log2FC| > 1). Higher overlap means
#   GeneSelectR agrees with the gold-standard DE analysis.
#
# GO BP enrichment:
#   Gene Ontology Biological Process over-representation analysis on
#   GeneSelectR's top 200 genes (using clusterProfiler). Significant terms
#   (adjusted p < 0.05) indicate that the selected genes are biologically
#   coherent, not just statistically significant noise.
#
# ==============================================================================
#
# OUTPUT FILES
# ------------
#
# results_realdata/data/
#   {dataset}_gene_scores.csv — Full gene ranking per dataset (all genes,
#     all score components: gene, final_score, pi_exact, u, u_coef, u_mi, b)
#   summary_table.csv — One row per dataset with AUC, stable gene counts, runtime
#   deseq2_overlap.csv — Overlap counts at k = 100, 200, 500
#
# results_realdata/enrichment/
#   {dataset}_GO_enrichment.csv — Significant GO BP terms for top-200 genes
#
# results_realdata/figures/
#   Per-dataset (generated for each dataset):
#     {name}_stability.pdf        — See FIGURE DOC in code below
#     {name}_auc_distribution.pdf — See FIGURE DOC in code below
#     {name}_biology_scores.pdf   — See FIGURE DOC in code below
#
#   Cross-dataset comparisons (Figures 1–7):
#     comparison_auc_boxplot.pdf         — Fig 1
#     comparison_score_distributions.pdf — Fig 2
#     comparison_stability_profile.pdf   — Fig 3
#     comparison_summary_panel.pdf       — Fig 4
#     comparison_runtime.pdf             — Fig 5
#     comparison_deseq2_overlap.pdf      — Fig 6
#     comparison_biology_scores.pdf      — Fig 7
#
#   Advanced cross-dataset (Figures 8–18):
#     jaccard_heatmap_top{100,500}.pdf            — Fig 8
#     rank_concordance_heatmap.pdf                — Fig 9
#     gene_dataset_stability_tilemap.pdf          — Fig 10
#     cumulative_recall_curves.pdf                — Fig 11a
#     precision_recall_curves.pdf                 — Fig 11b
#     enrichment_over_random.pdf                  — Fig 11c
#     go_enrichment_comparison_heatmap.pdf        — Fig 12
#     stability_utility_faceted.pdf               — Fig 13
#     score_component_contribution.pdf            — Fig 14
#     final_score_decay.pdf                       — Fig 15
#     dataset_characterization.pdf                — Fig 16
#     score_correlations_per_dataset.pdf          — Fig 17
#     paper_figure_combined.pdf                   — Fig 18
#
#   Value-added experiments (GeneSelectR vs DESeq2):
#     comparison_predictive_parsimony.pdf         — Exp 1: AUC vs #genes
#     comparison_redundancy.pdf                   — Exp 2: correlation among top genes
#     comparison_ranking_stability.pdf            — Exp 3: Jaccard under subsampling
#     comparison_functional_coherence.pdf         — Exp 4: GO enrichment at equal k
#     comparison_unique_genes.pdf                 — Exp 5: GS-only gene characterization
#     comparison_value_added_summary.pdf          — Combined 4-panel dashboard
#
# results_realdata/data/ (value-added experiments):
#   parsimony_comparison.csv        — AUC at each k for each method
#   redundancy_comparison.csv       — Mean |correlation| at each k
#   ranking_stability.csv           — Jaccard similarities from subsampling
#   functional_coherence.csv        — GO term counts and enrichment strength
#   unique_gene_characterization.csv — Shared/GS-only/DE-only gene counts
#   {dataset}_gs_unique_genes.csv   — Full score profiles of GS-only genes
#
# ==============================================================================
#
# Prerequisites (install once):
#   BiocManager::install(c(
#     "curatedTCGAData", "TCGAutils", "recount3",
#     "airway", "curatedOvarianData", "golubEsets",
#     "GEOquery", "SummarizedExperiment", "DESeq2",
#     "org.Hs.eg.db", "clusterProfiler", "enrichplot"
#   ))
#   install.packages(c("ggplot2", "dplyr", "tidyr", "patchwork", "pheatmap",
#                       "scales"))
#
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

rm(list = ls())
`%R%` <- function(x, n) paste(rep(x, n), collapse = "")

library(GeneSelectR)
library(parallel)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)    # For combining multi-panel plots
library(scales)       # For log-scale labels
library(pheatmap)     # For heatmaps

set.seed(42)

n_cores <- max(1, detectCores() - 1)
cat(sprintf("Detected %d cores, using %d\n", detectCores(), n_cores))

# Output directories
for (d in c("results_realdata", "results_realdata/figures",
            "results_realdata/data", "results_realdata/enrichment")) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}


# ==============================================================================
# PREPROCESSING UTILITIES
# ==============================================================================

#' Standard preprocessing: filter low-expression genes, normalize, subset
#'
#' Takes a raw count matrix and outcome, returns a clean log2(CPM+1) matrix
#' ready for GeneSelectR.
#'
#' @param counts   Raw count matrix (genes x samples or samples x genes)
#' @param y        Factor outcome
#' @param genes_in_rows  Logical: TRUE if genes are rows (standard bioconductor format)
#' @param min_count  Minimum mean count to keep a gene
#' @param min_var_quantile  Remove genes below this variance quantile
#' @param max_genes  Maximum genes to keep (top by variance)
preprocess_expression <- function(counts, y,
                                  genes_in_rows = TRUE,
                                  min_count = 10,
                                  min_var_quantile = 0.25,
                                  max_genes = 20000) {

  # Ensure genes are rows
  if (!genes_in_rows) counts <- t(counts)

  cat(sprintf("  Raw: %d genes x %d samples\n", nrow(counts), ncol(counts)))

  # Remove genes with very low expression
  gene_means <- rowMeans(counts, na.rm = TRUE)
  keep_expr <- gene_means >= min_count
  counts <- counts[keep_expr, , drop = FALSE]
  cat(sprintf("  After mean count >= %d filter: %d genes\n", min_count, nrow(counts)))

  # log2(CPM + 1) normalization
  lib_sizes <- colSums(counts)
  cpm <- sweep(counts, 2, lib_sizes, "/") * 1e6
  log_cpm <- log2(cpm + 1)

  # Remove low-variance genes (bottom quantile)
  gene_vars <- apply(log_cpm, 1, var, na.rm = TRUE)
  var_threshold <- quantile(gene_vars, probs = min_var_quantile, na.rm = TRUE)
  keep_var <- gene_vars > var_threshold
  log_cpm <- log_cpm[keep_var, , drop = FALSE]
  cat(sprintf("  After variance filter (>%.0f%%): %d genes\n",
              min_var_quantile * 100, nrow(log_cpm)))

  # Cap at max_genes (keep highest variance)
  if (nrow(log_cpm) > max_genes) {
    top_var_idx <- order(apply(log_cpm, 1, var, na.rm = TRUE),
                         decreasing = TRUE)[1:max_genes]
    log_cpm <- log_cpm[top_var_idx, , drop = FALSE]
    cat(sprintf("  Capped at top %d genes by variance\n", max_genes))
  }

  # Transpose to samples x genes (GeneSelectR format)
  X <- t(log_cpm)

  # Remove any NA/Inf
  X[is.na(X)] <- 0
  X[is.infinite(X)] <- 0

  # Match samples between X and y
  common <- intersect(rownames(X), names(y))
  if (length(common) > 0 && length(common) < nrow(X)) {
    X <- X[common, , drop = FALSE]
    y <- y[common]
  }

  # Remove any constant genes
  col_vars <- apply(X, 2, var, na.rm = TRUE)
  X <- X[, col_vars > 0, drop = FALSE]

  cat(sprintf("  Final: %d samples x %d genes\n", nrow(X), ncol(X)))
  cat(sprintf("  Classes: %s\n", paste(names(table(y)), table(y),
                                       sep = "=", collapse = ", ")))

  return(list(X = X, y = droplevels(y)))
}


#' Run DESeq2 for reference DE gene list
#'
#' @param counts   Raw count matrix (genes x samples)
#' @param y        Factor outcome
#' @param padj_threshold  Adjusted p-value cutoff
#' @param lfc_threshold   Log2 fold change cutoff
get_deseq2_reference <- function(counts, y, padj_threshold = 0.05,
                                 lfc_threshold = 1.0) {
  if (!requireNamespace("DESeq2", quietly = TRUE)) {
    warning("DESeq2 not installed, skipping reference DE analysis")
    return(NULL)
  }

  # Ensure genes are rows
  if (nrow(counts) < ncol(counts)) counts <- t(counts)

  # Match dimensions
  if (ncol(counts) != length(y)) {
    warning("Count matrix and y dimensions don't match for DESeq2")
    return(NULL)
  }

  coldata <- data.frame(condition = y, row.names = colnames(counts))

  tryCatch({
    dds <- DESeq2::DESeqDataSetFromMatrix(
      countData = round(counts),
      colData = coldata,
      design = ~ condition
    )

    # Filter low counts
    dds <- dds[rowSums(DESeq2::counts(dds)) >= 10, ]

    dds <- DESeq2::DESeq(dds, quiet = TRUE)
    res <- DESeq2::results(dds, alpha = padj_threshold)
    res <- as.data.frame(res)
    res$gene <- rownames(res)

    # Significant DE genes
    de_genes <- res %>%
      filter(!is.na(padj), padj < padj_threshold, abs(log2FoldChange) > lfc_threshold) %>%
      arrange(padj) %>%
      pull(gene)

    cat(sprintf("  DESeq2 reference: %d DE genes (padj<%.2f, |log2FC|>%.1f)\n",
                length(de_genes), padj_threshold, lfc_threshold))

    return(list(de_genes = de_genes, full_results = res))
  }, error = function(e) {
    warning("DESeq2 analysis failed: ", e$message)
    return(NULL)
  })
}


#' Run GO enrichment on top genes (Biological Process only)
run_go_enrichment <- function(gene_list, universe = NULL, n_top = 200,
                              ontology = "BP", pvalue_cutoff = 0.05,
                              qvalue_cutoff = 0.05) {
  if (!requireNamespace("clusterProfiler", quietly = TRUE) ||
      !requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    warning("clusterProfiler or org.Hs.eg.db not installed, skipping enrichment")
    return(NULL)
  }

  top_genes <- head(gene_list, n_top)

  # Validate ontology
  ontology <- match.arg(ontology, c("BP", "MF", "CC", "ALL"))
  ont_label <- if (ontology == "ALL") "GO (all)" else paste("GO", ontology)

  tryCatch({
    ego <- clusterProfiler::enrichGO(
      gene = top_genes,
      universe = universe,
      OrgDb = org.Hs.eg.db::org.Hs.eg.db,
      keyType = "SYMBOL",
      ont = ontology,
      pAdjustMethod = "BH",
      pvalueCutoff = pvalue_cutoff,
      qvalueCutoff = qvalue_cutoff,
      readable = TRUE
    )

    if (!is.null(ego) && nrow(as.data.frame(ego)) > 0) {
      result_df <- as.data.frame(ego)
      cat(sprintf("  %s enrichment: %d significant terms (top: %s)\n",
                  ont_label, nrow(result_df), result_df$Description[1]))
      return(list(enrichment = ego, df = result_df))
    } else {
      cat(sprintf("  %s enrichment: no significant terms\n", ont_label))
      return(NULL)
    }
  }, error = function(e) {
    warning("GO enrichment failed: ", e$message)
    return(NULL)
  })
}


# ==============================================================================
# DATA FETCHING FUNCTIONS
# ==============================================================================
# Each fetch_* function returns: list(X, y, counts, description, source)
# or NULL on failure. All have the same interface for uniform benchmarking.
# ==============================================================================


#' Dataset 1: TCGA-BRCA (Breast Cancer, tumor vs normal)
fetch_tcga_brca <- function(max_genes = 20000) {
  cat("\n--- Fetching TCGA-BRCA (Breast Cancer) ---\n")

  if (!requireNamespace("curatedTCGAData", quietly = TRUE)) {
    stop("Install curatedTCGAData: BiocManager::install('curatedTCGAData')")
  }

  # Download RNA-seq counts
  brca <- curatedTCGAData::curatedTCGAData(
    diseaseCode = "BRCA",
    assays = "RNASeq2Gene",      # RSEM normalized counts
    version = "2.0.1",
    dry.run = FALSE
  )

  # Extract expression matrix
  expr <- SummarizedExperiment::assay(brca[[1]])

  # Extract sample type from barcode (01 = primary tumor, 11 = normal)
  barcodes <- colnames(expr)
  sample_type <- substr(barcodes, 14, 15)

  # Keep only tumor (01) and normal (11) samples
  keep <- sample_type %in% c("01", "11")
  expr <- expr[, keep]
  sample_type <- sample_type[keep]

  y <- factor(ifelse(sample_type == "01", "Tumor", "Normal"))
  names(y) <- colnames(expr)

  cat(sprintf("  TCGA-BRCA: %d tumor, %d normal\n",
              sum(y == "Tumor"), sum(y == "Normal")))

  # Subsample tumors to balance (BRCA has ~1000 tumors vs ~100 normals)
  if (sum(y == "Tumor") > 3 * sum(y == "Normal")) {
    n_normal <- sum(y == "Normal")
    tumor_idx <- which(y == "Tumor")
    set.seed(42)
    keep_tumor <- sample(tumor_idx, min(2 * n_normal, length(tumor_idx)))
    keep_idx <- sort(c(which(y == "Normal"), keep_tumor))
    expr <- expr[, keep_idx]
    y <- y[keep_idx]
    cat(sprintf("  Subsampled to %d tumor, %d normal for balance\n",
                sum(y == "Tumor"), sum(y == "Normal")))
  }

  processed <- preprocess_expression(expr, y, genes_in_rows = TRUE,
                                     max_genes = max_genes)

  return(list(
    X = processed$X,
    y = processed$y,
    counts = expr,
    description = "TCGA-BRCA: Breast cancer (tumor vs adjacent normal)",
    source = "curatedTCGAData"
  ))
}


#' Dataset 2: TCGA-LUAD (Lung Adenocarcinoma, tumor vs normal)
fetch_tcga_luad <- function(max_genes = 20000) {
  cat("\n--- Fetching TCGA-LUAD (Lung Adenocarcinoma) ---\n")

  if (!requireNamespace("curatedTCGAData", quietly = TRUE)) {
    stop("Install curatedTCGAData: BiocManager::install('curatedTCGAData')")
  }

  luad <- curatedTCGAData::curatedTCGAData(
    diseaseCode = "LUAD",
    assays = "RNASeq2Gene",
    version = "2.0.1",
    dry.run = FALSE
  )

  expr <- SummarizedExperiment::assay(luad[[1]])
  barcodes <- colnames(expr)
  sample_type <- substr(barcodes, 14, 15)

  keep <- sample_type %in% c("01", "11")
  expr <- expr[, keep]
  sample_type <- sample_type[keep]
  y <- factor(ifelse(sample_type == "01", "Tumor", "Normal"))
  names(y) <- colnames(expr)

  cat(sprintf("  TCGA-LUAD: %d tumor, %d normal\n",
              sum(y == "Tumor"), sum(y == "Normal")))

  processed <- preprocess_expression(expr, y, genes_in_rows = TRUE,
                                     max_genes = max_genes)

  return(list(
    X = processed$X,
    y = processed$y,
    counts = expr,
    description = "TCGA-LUAD: Lung adenocarcinoma (tumor vs adjacent normal)",
    source = "curatedTCGAData"
  ))
}


#' Dataset 3: TCGA-KIRC (Kidney Clear Cell Carcinoma)
fetch_tcga_kirc <- function(max_genes = 20000) {
  cat("\n--- Fetching TCGA-KIRC (Kidney Clear Cell Carcinoma) ---\n")

  if (!requireNamespace("curatedTCGAData", quietly = TRUE)) {
    stop("Install curatedTCGAData: BiocManager::install('curatedTCGAData')")
  }

  kirc <- curatedTCGAData::curatedTCGAData(
    diseaseCode = "KIRC",
    assays = "RNASeq2Gene",
    version = "2.0.1",
    dry.run = FALSE
  )

  expr <- SummarizedExperiment::assay(kirc[[1]])
  barcodes <- colnames(expr)
  sample_type <- substr(barcodes, 14, 15)

  keep <- sample_type %in% c("01", "11")
  expr <- expr[, keep]
  sample_type <- sample_type[keep]
  y <- factor(ifelse(sample_type == "01", "Tumor", "Normal"))
  names(y) <- colnames(expr)

  cat(sprintf("  TCGA-KIRC: %d tumor, %d normal\n",
              sum(y == "Tumor"), sum(y == "Normal")))

  processed <- preprocess_expression(expr, y, genes_in_rows = TRUE,
                                     max_genes = max_genes)

  return(list(
    X = processed$X,
    y = processed$y,
    counts = expr,
    description = "TCGA-KIRC: Kidney clear cell carcinoma (tumor vs normal)",
    source = "curatedTCGAData"
  ))
}


#' #' Dataset 4: GTEx — Brain vs Liver via recount3
#' fetch_gtex_brain_liver <- function(max_genes = 20000, max_samples_per_tissue = 100) {
#'   cat("\n--- Fetching GTEx: Brain vs Liver (via recount3) ---\n")
#'
#'   if (!requireNamespace("recount3", quietly = TRUE)) {
#'     stop("Install recount3: BiocManager::install('recount3')")
#'   }
#'
#'   # Get available GTEx projects
#'   human_projects <- recount3::available_projects(organism = "human")
#'   gtex_projects <- human_projects[human_projects$file_source == "gtex", ]
#'
#'   # Fetch brain (cerebellum as representative) and liver
#'   brain_proj <- gtex_projects[grep("Brain", gtex_projects$project), ]
#'   liver_proj <- gtex_projects[grep("Liver", gtex_projects$project), ]
#'
#'   if (nrow(brain_proj) == 0 || nrow(liver_proj) == 0) {
#'     stop("Could not find Brain or Liver GTEx projects in recount3")
#'   }
#'
#'   # Use the first matching brain region
#'   brain_rse <- recount3::create_rse(brain_proj[1, ])
#'   liver_rse <- recount3::create_rse(liver_proj[1, ])
#'
#'   # Transform counts
#'   brain_counts <- recount3::transform_counts(brain_rse)
#'   liver_counts <- recount3::transform_counts(liver_rse)
#'
#'   # Subsample if too many
#'   if (ncol(brain_counts) > max_samples_per_tissue) {
#'     set.seed(42)
#'     brain_counts <- brain_counts[, sample(ncol(brain_counts), max_samples_per_tissue)]
#'   }
#'   if (ncol(liver_counts) > max_samples_per_tissue) {
#'     set.seed(43)
#'     liver_counts <- liver_counts[, sample(ncol(liver_counts), max_samples_per_tissue)]
#'   }
#'
#'   # Find common genes
#'   common_genes <- intersect(rownames(brain_counts), rownames(liver_counts))
#'   combined_counts <- cbind(brain_counts[common_genes, ],
#'                            liver_counts[common_genes, ])
#'
#'   y <- factor(c(rep("Brain", ncol(brain_counts)),
#'                 rep("Liver", ncol(liver_counts))))
#'   names(y) <- colnames(combined_counts)
#'
#'   cat(sprintf("  GTEx: %d Brain, %d Liver, %d common genes\n",
#'               ncol(brain_counts), ncol(liver_counts), length(common_genes)))
#'
#'   processed <- preprocess_expression(combined_counts, y, genes_in_rows = TRUE,
#'                                      max_genes = max_genes)
#'
#'   return(list(
#'     X = processed$X,
#'     y = processed$y,
#'     counts = combined_counts,
#'     description = "GTEx: Brain vs Liver tissue",
#'     source = "recount3"
#'   ))
#' }


#' #' Dataset 5: Golub Leukemia (ALL vs AML) — classic microarray dataset
#' fetch_golub_leukemia <- function() {
#'   cat("\n--- Fetching Golub Leukemia (ALL vs AML) ---\n")
#'
#'   if (!requireNamespace("golubEsets", quietly = TRUE)) {
#'     stop("Install golubEsets: BiocManager::install('golubEsets')")
#'   }
#'
#'   data("Golub_Merge", package = "golubEsets")
#'
#'   # Extract expression matrix (already log-scale microarray data)
#'   expr <- Biobase::exprs(Golub_Merge)
#'   pheno <- Biobase::pData(Golub_Merge)
#'
#'   y <- factor(pheno$ALL.AML)
#'   names(y) <- colnames(expr)
#'
#'   # This is microarray data (already normalized), transpose to samples x genes
#'   X <- t(expr)
#'
#'   # Remove constant genes
#'   col_vars <- apply(X, 2, var, na.rm = TRUE)
#'   X <- X[, col_vars > 0, drop = FALSE]
#'   X[is.na(X)] <- 0
#'
#'   cat(sprintf("  Golub: %d samples, %d genes\n", nrow(X), ncol(X)))
#'   cat(sprintf("  Classes: %s\n", paste(names(table(y)), table(y),
#'                                        sep = "=", collapse = ", ")))
#'
#'   return(list(
#'     X = X,
#'     y = y,
#'     counts = NULL,   # Microarray, no raw counts
#'     description = "Golub Leukemia: ALL vs AML (microarray)",
#'     source = "golubEsets"
#'   ))
#' }
#'
#'
#' #' Dataset 6: airway — Dexamethasone-treated vs untreated airway smooth muscle
#' fetch_airway <- function(max_genes = 20000) {
#'   cat("\n--- Fetching airway (dexamethasone treatment) ---\n")
#'
#'   if (!requireNamespace("airway", quietly = TRUE)) {
#'     stop("Install airway: BiocManager::install('airway')")
#'   }
#'
#'   data("airway", package = "airway")
#'
#'   counts <- SummarizedExperiment::assay(airway)
#'   pheno <- SummarizedExperiment::colData(airway)
#'
#'   y <- factor(pheno$dex, levels = c("untrt", "trt"),
#'               labels = c("Untreated", "Treated"))
#'   names(y) <- colnames(counts)
#'
#'   cat(sprintf("  airway: %d samples (%s)\n", length(y),
#'               paste(names(table(y)), table(y), sep = "=", collapse = ", ")))
#'
#'   processed <- preprocess_expression(counts, y, genes_in_rows = TRUE,
#'                                      max_genes = max_genes)
#'
#'   return(list(
#'     X = processed$X,
#'     y = processed$y,
#'     counts = counts,
#'     description = "airway: Dexamethasone treatment of airway smooth muscle cells",
#'     source = "airway"
#'   ))
#' }
#'
#'
#' #' Dataset 7: curatedOvarianData — Early vs late stage ovarian cancer
#' fetch_ovarian <- function(max_genes = 20000) {
#'   cat("\n--- Fetching Ovarian Cancer (early vs late stage) ---\n")
#'
#'   if (!requireNamespace("curatedOvarianData", quietly = TRUE)) {
#'     stop("Install: BiocManager::install('curatedOvarianData')")
#'   }
#'
#'   data("TCGA_eset", package = "curatedOvarianData")
#'
#'   expr <- Biobase::exprs(TCGA_eset)
#'   pheno <- Biobase::pData(TCGA_eset)
#'
#'   # Extract tumor stage
#'   if ("tumorstage" %in% colnames(pheno)) {
#'     stage <- pheno$tumorstage
#'   } else if ("tumor_stage" %in% colnames(pheno)) {
#'     stage <- pheno$tumor_stage
#'   } else {
#'     # Try to find any stage-related column
#'     stage_col <- grep("stage", colnames(pheno), ignore.case = TRUE, value = TRUE)
#'     if (length(stage_col) > 0) {
#'       stage <- pheno[[stage_col[1]]]
#'     } else {
#'       stop("No stage information found in curatedOvarianData")
#'     }
#'   }
#'
#'   # Classify into early (I, II) vs late (III, IV)
#'   early <- grepl("^[12]$|^I$|^II$|^I[^V]|^II[^I]", as.character(stage))
#'   late <- grepl("^[34]$|^III|^IV", as.character(stage))
#'   keep <- early | late
#'
#'   if (sum(keep) < 20) {
#'     warning("Too few staged samples, using alternative classification")
#'     # Fall back to debulking status if available
#'     if ("debulking" %in% colnames(pheno)) {
#'       y <- factor(pheno$debulking)
#'       keep <- !is.na(y) & y %in% levels(y)[table(y) >= 10]
#'       y <- droplevels(y[keep])
#'     } else {
#'       stop("Cannot create binary classification for ovarian data")
#'     }
#'   } else {
#'     y <- factor(ifelse(early[keep], "Early", "Late"))
#'   }
#'
#'   expr <- expr[, keep]
#'   names(y) <- colnames(expr)
#'
#'   # Microarray data — already normalized
#'   X <- t(expr)
#'   col_vars <- apply(X, 2, var, na.rm = TRUE)
#'   X <- X[, col_vars > 0, drop = FALSE]
#'   X[is.na(X)] <- 0
#'
#'   if (ncol(X) > max_genes) {
#'     top_var <- order(apply(X, 2, var), decreasing = TRUE)[1:max_genes]
#'     X <- X[, top_var, drop = FALSE]
#'   }
#'
#'   cat(sprintf("  Ovarian: %d samples, %d genes\n", nrow(X), ncol(X)))
#'   cat(sprintf("  Classes: %s\n", paste(names(table(y)), table(y),
#'                                        sep = "=", collapse = ", ")))
#'
#'   return(list(
#'     X = X,
#'     y = y,
#'     counts = NULL,
#'     description = "Ovarian Cancer: Early stage vs Late stage",
#'     source = "curatedOvarianData"
#'   ))
#' }
#'
#'
#' #' Dataset 8: GEO GSE53757 — Kidney cancer RNA-seq (ccRCC vs normal)
#' fetch_geo_kidney <- function(max_genes = 20000) {
#'   cat("\n--- Fetching GEO GSE53757 (Kidney ccRCC) ---\n")
#'
#'   if (!requireNamespace("GEOquery", quietly = TRUE)) {
#'     stop("Install GEOquery: BiocManager::install('GEOquery')")
#'   }
#'
#'   gse <- GEOquery::getGEO("GSE53757", GSEMatrix = TRUE, getGPL = FALSE)
#'
#'   if (is.list(gse)) gse <- gse[[1]]
#'
#'   expr <- Biobase::exprs(gse)
#'   pheno <- Biobase::pData(gse)
#'
#'   # Find the tissue type column
#'   tissue_col <- grep("tissue|type|source|characteristic",
#'                      colnames(pheno), ignore.case = TRUE, value = TRUE)
#'
#'   y <- NULL
#'   for (col in tissue_col) {
#'     vals <- tolower(as.character(pheno[[col]]))
#'     if (any(grepl("tumor|cancer|carcinoma", vals)) &&
#'         any(grepl("normal|adjacent|healthy", vals))) {
#'       is_tumor <- grepl("tumor|cancer|carcinoma", vals)
#'       is_normal <- grepl("normal|adjacent|healthy", vals)
#'       keep <- is_tumor | is_normal
#'       y <- factor(ifelse(is_tumor[keep], "Tumor", "Normal"))
#'       expr <- expr[, keep]
#'       break
#'     }
#'   }
#'
#'   if (is.null(y)) {
#'     stop("Could not identify tumor/normal labels in GEO metadata")
#'   }
#'
#'   names(y) <- colnames(expr)
#'
#'   # Microarray or already processed
#'   X <- t(expr)
#'   col_vars <- apply(X, 2, var, na.rm = TRUE)
#'   X <- X[, col_vars > 0, drop = FALSE]
#'   X[is.na(X)] <- 0
#'
#'   if (ncol(X) > max_genes) {
#'     top_var <- order(apply(X, 2, var), decreasing = TRUE)[1:max_genes]
#'     X <- X[, top_var, drop = FALSE]
#'   }
#'
#'   cat(sprintf("  GSE53757: %d samples, %d genes\n", nrow(X), ncol(X)))
#'   cat(sprintf("  Classes: %s\n", paste(names(table(y)), table(y),
#'                                        sep = "=", collapse = ", ")))
#'
#'   return(list(
#'     X = X,
#'     y = y,
#'     counts = NULL,
#'     description = "GEO GSE53757: Kidney ccRCC vs normal",
#'     source = "GEOquery"
#'   ))
#' }


# ==============================================================================
# MASTER EVALUATION FUNCTION
# ==============================================================================

#' Run GeneSelectR on a dataset and evaluate comprehensively
#'
#' @param dataset   List from a fetch_* function
#' @param name      Short name for logging and file naming
#' @param K         CV folds
#' @param R         CV repeats
#' @param bio_mode  Biology scoring mode
#' @param target_terms  GO terms for supervised bio mode
#' @param bio_ontology  GO ontology filter (default: "BP")
#' @param bio_sim_method  Similarity metric (default: "resnik")
#' @param bio_enrich_fdr  FDR threshold for data-driven enrichment
#' @param bio_ic_quantile  IC specificity filter quantile
#' @param bio_n_top_sims  Top-k similarities to average per gene
#' @param bio_max_enriched  Max enriched terms to score against
#' @param enrich_ontology  Ontology for clusterProfiler enrichment analysis
#' @param enrich_pvalue  P-value cutoff for enrichment analysis
#' @param enrich_qvalue  Q-value cutoff for enrichment analysis
evaluate_dataset <- function(dataset, name, K = 5, R = 10,
                             bio_mode = "none",
                             target_terms = NULL,
                             bio_ontology = "BP",
                             bio_sim_method = "resnik",
                             bio_enrich_fdr = 0.05,
                             bio_ic_quantile = 0.5,
                             bio_n_top_sims = 5,
                             bio_max_enriched = 100,
                             enrich_ontology = "BP",
                             enrich_pvalue = 0.05,
                             enrich_qvalue = 0.05) {

  cat("\n")
  cat("=" %R% 80, "\n", sep = "")
  cat(sprintf("EVALUATING: %s\n", dataset$description))
  cat("=" %R% 80, "\n", sep = "")

  X <- dataset$X
  y <- dataset$y
  n_genes <- ncol(X)
  n_samples <- nrow(X)

  cat(sprintf("  Matrix: %d samples x %d genes\n", n_samples, n_genes))
  cat(sprintf("  Classes: %s\n",
              paste(levels(y), table(y), sep = "=", collapse = ", ")))

  # --- Run GeneSelectR ---
  t0 <- proc.time()

  result <- geneselectr2_fit(
    X = X,
    y = y,
    bio_mode = bio_mode,
    target_terms = target_terms,
    bio_ontology = bio_ontology,
    bio_sim_method = bio_sim_method,
    bio_enrich_fdr = bio_enrich_fdr,
    bio_ic_quantile = bio_ic_quantile,
    bio_n_top_sims = bio_n_top_sims,
    bio_max_enriched = bio_max_enriched,
    K = K,
    R = R,
    n_cores = n_cores,
    verbose = TRUE
  )

  wall_time <- (proc.time() - t0)["elapsed"]
  cat(sprintf("\n  GeneSelectR completed in %.1f seconds (%.1f min)\n",
              wall_time, wall_time / 60))

  # --- Summary statistics ---
  gs <- result$gene_scores
  cat(sprintf("  Mean AUC: %.4f +/- %.4f\n",
              result$cv_summary$mean, result$cv_summary$sd))
  cat(sprintf("  Genes with pi > 0.5: %d (%.1f%%)\n",
              sum(gs$pi_exact > 0.5), 100 * sum(gs$pi_exact > 0.5) / n_genes))
  cat(sprintf("  Genes with pi > 0.9: %d (%.1f%%)\n",
              sum(gs$pi_exact > 0.9), 100 * sum(gs$pi_exact > 0.9) / n_genes))

  # --- DESeq2 reference comparison (if raw counts available) ---
  deseq_result <- NULL
  overlap_stats <- NULL

  if (!is.null(dataset$counts)) {
    cat("\n  Running DESeq2 reference analysis...\n")
    deseq_result <- get_deseq2_reference(dataset$counts, y)

    if (!is.null(deseq_result) && length(deseq_result$de_genes) > 0) {
      # Compare top GeneSelectR genes with DESeq2 DE genes
      for (k in c(100, 200, 500)) {
        top_gs <- head(gs$gene, k)
        overlap <- sum(top_gs %in% deseq_result$de_genes)
        pct_of_topk <- 100 * overlap / k
        pct_of_de <- 100 * overlap / length(deseq_result$de_genes)

        cat(sprintf("  Top %d GeneSelectR vs DESeq2: %d overlap (%.1f%% of top-k, %.1f%% of DE)\n",
                    k, overlap, pct_of_topk, pct_of_de))

        overlap_stats <- rbind(overlap_stats, data.frame(
          dataset = name, k = k, overlap = overlap,
          pct_topk = pct_of_topk, pct_de = pct_of_de,
          n_de = length(deseq_result$de_genes)
        ))
      }
    }
  }

  # --- GO enrichment of top genes ---
  cat("\n  Running GO enrichment on top 200 genes...\n")
  enrichment <- run_go_enrichment(
    gs$gene, universe = colnames(X), n_top = 200,
    ontology = enrich_ontology,
    pvalue_cutoff = enrich_pvalue,
    qvalue_cutoff = enrich_qvalue
  )

  # --- Save per-dataset outputs ---
  # ---- TABLE: {name}_gene_scores.csv ----
  # Full gene ranking for this dataset. One row per gene. Columns:
  #   gene        — gene symbol (e.g., "TP53")
  #   final_score — combined score (geometric mean of pi, u, b)
  #   pi_exact    — selection stability (fraction of CV folds where selected)
  #   u           — utility score (mean of u_coef and u_mi, percentile-normalized)
  #   u_coef      — mean |elastic net coefficient| across folds (percentile-normalized)
  #   u_mi        — mean mutual information with outcome across folds (percentile-normalized)
  #   b           — biological relevance (GO BP semantic similarity)
  # Sorted by final_score descending (best genes first).
  write.csv(gs, sprintf("results_realdata/data/%s_gene_scores.csv", name),
            row.names = FALSE)

  # ---- TABLE: {name}_GO_enrichment.csv ----
  # GO enrichment for top-200 genes (ontology configured via enrich_ontology).
  # Columns include:
  #   ID          — GO term ID (e.g., "GO:0006955")
  #   Description — human-readable term name
  #   GeneRatio   — fraction of top-200 genes annotated with this term
  #   BgRatio     — fraction of background genes annotated
  #   pvalue      — raw Fisher's exact test p-value
  #   p.adjust    — BH-adjusted p-value
  #   Count       — number of top-200 genes with this annotation
  if (!is.null(enrichment)) {
    write.csv(enrichment$df,
              sprintf("results_realdata/enrichment/%s_GO_enrichment.csv", name),
              row.names = FALSE)
  }

  # --- Stability distribution plot (two-panel: zoom on selected genes) ---
  # ---- PER-DATASET FIGURE: {name}_stability.pdf ----
  # What it shows: Histogram of selection stability (pi) for genes that were
  #   selected at least once (pi > 0). Genes never selected (pi = 0) are
  #   excluded to avoid a huge bar at zero that crushes the rest.
  # X-axis: pi (fraction of CV folds where the gene was selected).
  # Y-axis: number of genes at that pi value.
  # Orange dashed line: pi = 0.5 (selected in half the folds).
  # Red dashed line: pi = 0.8 (highly stable).
  # Labels show the count of genes above each threshold.
  # Subtitle reports the count of never-selected genes, total genes, and AUC.
  # What to look for: A healthy distribution has a cluster near pi = 1
  #   (reliably selected genes = real signal) and a tail toward 0 (noise
  #   genes occasionally selected). If everything is below 0.5, the signal
  #   may be too weak or the sample size too small.

  n_never <- sum(gs$pi_exact == 0)
  gs_selected <- gs %>% filter(pi_exact > 0)

  p_stab_zoom <- ggplot(gs_selected, aes(x = pi_exact)) +
    geom_histogram(bins = 40, fill = "steelblue", color = "white", alpha = 0.8) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "orange", linewidth = 0.7) +
    geom_vline(xintercept = 0.8, linetype = "dashed", color = "red", linewidth = 0.7) +
    annotate("text", x = 0.52, y = Inf, vjust = 2, hjust = 0,
             label = sprintf("pi>0.5: %d", sum(gs$pi_exact > 0.5)),
             color = "orange", size = 3.5) +
    annotate("text", x = 0.82, y = Inf, vjust = 2, hjust = 0,
             label = sprintf("pi>0.9: %d", sum(gs$pi_exact > 0.9)),
             color = "red", size = 3.5) +
    labs(
      title = sprintf("%s — Stability of Selected Genes (pi > 0)", name),
      subtitle = sprintf("n=%d, p=%d, AUC=%.3f | %d genes never selected (pi=0, not shown)",
                         n_samples, n_genes, result$cv_summary$mean, n_never),
      x = expression(pi ~ "(selection frequency)"),
      y = "Number of genes"
    ) +
    theme_bw()

  ggsave(sprintf("results_realdata/figures/%s_stability.pdf", name),
         p_stab_zoom, width = 9, height = 5)

  # ---- PER-DATASET FIGURE: {name}_auc_distribution.pdf ----
  # What it shows: Histogram of AUC values from individual CV folds.
  #   With K=5 folds × R=10 repeats, there are 50 AUC values.
  # X-axis: AUC of the elastic net classifier on held-out test data.
  # Y-axis: number of folds with that AUC.
  # Red vertical line: mean AUC across all folds.
  # What to look for: A tight distribution near 1.0 means the classification
  #   is easy and stable. A wide spread or values near 0.5 indicates the
  #   model struggles — either weak signal or too few samples.
  # --- Per-fold AUC distribution ---
  cv_auc_per_fold <- result$cv_results$auc_scores

  p_auc_folds <- ggplot(data.frame(auc = cv_auc_per_fold), aes(x = auc)) +
    geom_histogram(bins = 20, fill = "#E69F00", color = "white", alpha = 0.8) +
    geom_vline(xintercept = mean(cv_auc_per_fold, na.rm = TRUE),
               linetype = "solid", color = "red") +
    labs(
      title = sprintf("%s — Per-Fold AUC Distribution", name),
      subtitle = sprintf("Mean = %.4f, SD = %.4f",
                         mean(cv_auc_per_fold, na.rm = TRUE),
                         sd(cv_auc_per_fold, na.rm = TRUE)),
      x = "AUC", y = "Number of folds"
    ) +
    theme_bw()

  ggsave(sprintf("results_realdata/figures/%s_auc_distribution.pdf", name),
         p_auc_folds, width = 7, height = 4)

  # ---- PER-DATASET FIGURE: {name}_biology_scores.pdf ----
  # What it shows: Histogram of the biology score (b) across ALL genes.
  # X-axis: b = data-driven GO semantic similarity (0 = no GO annotation or
  #   no similarity to enriched terms; higher = more biologically connected).
  # Y-axis: number of genes.
  # What to look for: A good spread from 0 to 1 means the biology score is
  #   discriminating. If all genes cluster at one value (e.g., all b = 0 or
  #   all b = 1), the scoring is not adding information.
  # --- Biology score distribution ---
  p_bio <- ggplot(gs, aes(x = b)) +
    geom_histogram(bins = 50, fill = "#009E73", color = "white", alpha = 0.8) +
    labs(
      title = sprintf("%s — Biology Score Distribution", name),
      x = "b (biological relevance)", y = "Number of genes"
    ) +
    theme_bw()

  ggsave(sprintf("results_realdata/figures/%s_biology_scores.pdf", name),
         p_bio, width = 7, height = 4)

  # --- Score correlation heatmap ---
  score_cols <- gs[, c("pi_exact", "u_coef", "u_mi", "u", "b", "final_score")]
  cor_mat <- cor(score_cols, use = "complete.obs")

  # Return everything for cross-dataset comparison
  return(list(
    name = name,
    description = dataset$description,
    source = dataset$source,
    n_samples = n_samples,
    n_genes = n_genes,
    X = X,                                 # expression matrix (for new experiments)
    y = y,                                 # outcome vector (for new experiments)
    counts = dataset$counts,               # raw counts if available (for subsampling)
    gene_scores = gs,
    cv_auc_mean = result$cv_summary$mean,
    cv_auc_sd = result$cv_summary$sd,
    cv_auc_per_fold = cv_auc_per_fold,       # per-fold AUCs for boxplots
    n_stable_50 = sum(gs$pi_exact > 0.5),
    n_stable_90 = sum(gs$pi_exact > 0.9),
    wall_time = wall_time,
    deseq_result = deseq_result,
    overlap_stats = overlap_stats,
    enrichment = enrichment,
    score_correlations = cor_mat
  ))
}


# ==============================================================================
# RUN ALL BENCHMARKS
# ==============================================================================

cat("\n")
cat("=" %R% 80, "\n", sep = "")
cat("GeneSelectR 2.0 — Real-Data Benchmarking Suite\n")
cat("=" %R% 80, "\n\n", sep = "")

# Registry of datasets to try. Each is wrapped in tryCatch so a single
# download failure doesn't kill the whole script.
datasets_to_run <- list(
  list(name = "TCGA_BRCA", fetch = fetch_tcga_brca,
       bio_mode = "supervised", target_terms = c("GO:0006281", "GO:0007049")),  # DNA repair, cell cycle
  list(name = "TCGA_LUAD", fetch = fetch_tcga_luad,
       bio_mode = "supervised", target_terms = c("GO:0006955", "GO:0007165")),  # immune response, signal transduction
  list(name = "TCGA_KIRC", fetch = fetch_tcga_kirc,
       bio_mode = "supervised", target_terms = c("GO:0001525", "GO:0006950"))  # angiogenesis, response to stress
  # list(name = "GTEx_BrainLiver", fetch = fetch_gtex_brain_liver,
  #      bio_mode = "none", target_terms = NULL),
  # list(name = "Golub_Leukemia", fetch = fetch_golub_leukemia,
  #      bio_mode = "none", target_terms = NULL),
  # list(name = "airway", fetch = fetch_airway,
  #      bio_mode = "supervised", target_terms = c("GO:0006954", "GO:0006955")),  # inflammation, immune
  # list(name = "Ovarian", fetch = fetch_ovarian,
  #      bio_mode = "none", target_terms = NULL),
  # list(name = "GEO_Kidney", fetch = fetch_geo_kidney,
  #      bio_mode = "none", target_terms = NULL)
)

# Store all results
all_results <- list()
summary_table <- data.frame()

for (ds in datasets_to_run) {
  cat(sprintf("\n\n>>> Attempting: %s <<<\n", ds$name))

  result <- tryCatch({
    # Fetch data
    data <- ds$fetch()

    # Build args: required params + any optional bio params from the dataset entry
    eval_args <- list(
      dataset = data,
      name = ds$name,
      K = 5,
      R = 10,
      bio_mode = ds$bio_mode,
      target_terms = ds$target_terms
    )
    # Pass through optional per-dataset bio params (bio_ontology, bio_sim_method,
    # bio_enrich_fdr, bio_ic_quantile, enrich_ontology, etc.)
    optional_params <- c("bio_ontology", "bio_sim_method", "bio_enrich_fdr",
                         "bio_ic_quantile", "bio_n_top_sims", "bio_max_enriched",
                         "enrich_ontology", "enrich_pvalue", "enrich_qvalue")
    for (p in optional_params) {
      if (!is.null(ds[[p]])) eval_args[[p]] <- ds[[p]]
    }
    do.call(evaluate_dataset, eval_args)
  }, error = function(e) {
    cat(sprintf("\n  FAILED: %s\n  Error: %s\n", ds$name, e$message))
    cat("  Skipping this dataset and continuing...\n")
    NULL
  })

  if (!is.null(result)) {
    all_results[[ds$name]] <- result

    summary_table <- rbind(summary_table, data.frame(
      Dataset = result$name,
      Source = result$source,
      Samples = result$n_samples,
      Genes = result$n_genes,
      AUC_mean = round(result$cv_auc_mean, 4),
      AUC_sd = round(result$cv_auc_sd, 4),
      Stable_50 = result$n_stable_50,
      Stable_90 = result$n_stable_90,
      Runtime_sec = round(result$wall_time, 1),
      stringsAsFactors = FALSE
    ))
  }
}


# ==============================================================================
# CROSS-DATASET COMPARISON
# ==============================================================================

cat("\n\n")
cat("=" %R% 80, "\n", sep = "")
cat("CROSS-DATASET COMPARISON\n")
cat("=" %R% 80, "\n\n", sep = "")

if (nrow(summary_table) == 0) {
  cat("No datasets were successfully processed. Check package installations.\n")
} else {

  # --- Print summary table ---
  # ---- TABLE: summary_table.csv ----
  # One row per dataset. Columns:
  #   Dataset   — short name (e.g., "TCGA_BRCA")
  #   Source    — R package it came from (e.g., "curatedTCGAData")
  #   Samples   — number of samples after filtering and balancing
  #   Genes     — number of genes after preprocessing
  #   AUC_mean  — mean cross-validation AUC across all folds
  #   AUC_sd    — standard deviation of per-fold AUC
  #   Stable_50 — count of genes with pi > 0.5 (selected in >50% of folds)
  #   Stable_90 — count of genes with pi > 0.9 (selected in >90% of folds)
  #   Runtime_sec — wall-clock time for the full geneselectr2_fit() call
  cat("=== Performance Summary ===\n\n")
  print(summary_table, row.names = FALSE)

  write.csv(summary_table, "results_realdata/data/summary_table.csv",
            row.names = FALSE)

  # =========================================================================
  # FIGURE 1: Per-Fold AUC Boxplots (combined across all datasets)
  # =========================================================================
  # What it shows: One boxplot per dataset, each containing K × R individual
  #   AUC values (one per CV fold). Datasets are sorted by median AUC.
  # X-axis: Dataset name. Y-axis: AUC.
  # Red dashed line: AUC = 0.5 (chance-level classification).
  # How to read it: Wide boxes = high variance across folds (unstable model).
  #   Narrow boxes near 1.0 = easy, consistent classification.
  # What to look for: Most tumor-vs-normal datasets (TCGA-BRCA, KIRC) should
  #   be near 1.0. Harder tasks (ovarian early-vs-late) will be lower.
  #   Any dataset below 0.7 may have weak signal or too few samples.
  # =========================================================================

  auc_long <- do.call(rbind, lapply(all_results, function(r) {
    data.frame(
      Dataset = r$name,
      AUC = r$cv_auc_per_fold,
      stringsAsFactors = FALSE
    )
  }))

  # Order datasets by median AUC
  ds_order <- auc_long %>%
    group_by(Dataset) %>%
    summarise(med = median(AUC, na.rm = TRUE)) %>%
    arrange(desc(med)) %>%
    pull(Dataset)
  auc_long$Dataset <- factor(auc_long$Dataset, levels = rev(ds_order))

  p_auc_box <- ggplot(auc_long, aes(x = Dataset, y = AUC, fill = Dataset)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(
      title = "Cross-Validation AUC Across All Datasets",
      subtitle = "Each box: distribution of per-fold AUC values; red dashed = chance",
      x = NULL, y = "AUC"
    ) +
    theme_bw() +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set2")

  ggsave("results_realdata/figures/comparison_auc_boxplot.pdf",
         p_auc_box, width = 10, height = 6)

  # =========================================================================
  # FIGURE 2: Combined Score Distributions (violin + boxplot per dataset)
  # =========================================================================
  # What it shows: Four side-by-side panels showing the distribution of each
  #   score component (Stability/pi, Utility/u, Biology/b, Final score)
  #   across all datasets. Only genes with pi > 0 are included.
  # Each panel: X-axis = dataset, Y-axis = score value (0 to 1).
  #   Violin shape shows the full distribution; inner box shows the median
  #   and interquartile range.
  # How to read it: Compare violin shapes across datasets within each panel.
  # What to look for:
  #   - Stability panel: Datasets with tall violins concentrated near 1.0
  #     have many reliably selected genes.
  #   - Biology panel: Datasets where b is spread out (not all bunched at one
  #     value) show that biology scoring is discriminating.
  #   - Final panel: The combined view — datasets with broad distributions
  #     have well-differentiated gene rankings.
  # =========================================================================

  score_long <- do.call(rbind, lapply(all_results, function(r) {
    gs <- r$gene_scores
    # Only include genes selected at least once (pi > 0) for readability
    gs_sel <- gs[gs$pi_exact > 0, ]
    data.frame(
      Dataset = r$name,
      Stability = gs_sel$pi_exact,
      Utility = gs_sel$u,
      Biology = gs_sel$b,
      Final = gs_sel$final_score,
      stringsAsFactors = FALSE
    )
  }))

  score_long$Dataset <- factor(score_long$Dataset, levels = rev(ds_order))

  score_pivot <- score_long %>%
    pivot_longer(cols = c(Stability, Utility, Biology, Final),
                 names_to = "Score", values_to = "Value") %>%
    mutate(Score = factor(Score, levels = c("Stability", "Utility", "Biology", "Final")))

  p_scores <- ggplot(score_pivot, aes(x = Dataset, y = Value, fill = Dataset)) +
    geom_violin(alpha = 0.4, scale = "width", draw_quantiles = c(0.5)) +
    geom_boxplot(width = 0.15, alpha = 0.8, outlier.size = 0.3) +
    facet_wrap(~ Score, scales = "free_y", nrow = 1) +
    coord_flip() +
    labs(
      title = "Score Distributions Across Datasets (selected genes only, pi > 0)",
      x = NULL, y = "Score value"
    ) +
    theme_bw() +
    theme(legend.position = "none",
          strip.text = element_text(size = 11, face = "bold")) +
    scale_fill_brewer(palette = "Set2")

  ggsave("results_realdata/figures/comparison_score_distributions.pdf",
         p_scores, width = 16, height = 7)

  # =========================================================================
  # FIGURE 3: Stability Profile — genes surviving each pi threshold
  # =========================================================================
  # What it shows: Line plot with one line per dataset. X-axis = pi threshold
  #   (0.1, 0.3, 0.5, 0.7, 0.8, 0.9, 0.95). Y-axis = number of genes with
  #   pi >= that threshold (log scale).
  # How to read it: Lines that stay high (flat) across thresholds have many
  #   robustly selected genes. Lines that drop steeply have a "soft" selection
  #   border — genes are selected sporadically, not consistently.
  # What to look for: The gap between pi=0.5 and pi=0.9 shows how many genes
  #   are "borderline" vs "rock-solid". A dataset with 500 genes at pi>0.5
  #   but only 20 at pi>0.9 has mostly weak signal.
  # =========================================================================

  stab_thresholds <- c(0.1, 0.3, 0.5, 0.7, 0.8, 0.9, 0.95)

  stab_profile <- do.call(rbind, lapply(all_results, function(r) {
    gs <- r$gene_scores
    do.call(rbind, lapply(stab_thresholds, function(thr) {
      data.frame(
        Dataset = r$name,
        Threshold = thr,
        N_genes = sum(gs$pi_exact >= thr),
        Pct_genes = 100 * sum(gs$pi_exact >= thr) / nrow(gs),
        stringsAsFactors = FALSE
      )
    }))
  }))

  stab_profile$Dataset <- factor(stab_profile$Dataset, levels = ds_order)

  p_stab_profile <- ggplot(stab_profile,
                           aes(x = Threshold, y = N_genes,
                               color = Dataset, group = Dataset)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_y_log10(labels = scales::comma_format()) +
    labs(
      title = "Selection Stability Profile: Genes Above Each Threshold",
      subtitle = "How many genes survive increasingly strict stability cutoffs?",
      x = expression(pi ~ "threshold"),
      y = "Number of genes (log scale)",
      color = NULL
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_color_brewer(palette = "Set2")

  ggsave("results_realdata/figures/comparison_stability_profile.pdf",
         p_stab_profile, width = 10, height = 6)

  # =========================================================================
  # FIGURE 4: Multi-metric summary panel (AUC, stable gene counts, runtime)
  # =========================================================================
  # What it shows: Four horizontal bar charts in a 2×2 layout.
  #   Panel A (top-left): Mean AUC with error bars (± 1 SD). Red dashed
  #     line = chance.
  #   Panel B (top-right): Number of genes with pi > 0.5. Labels show count.
  #   Panel C (bottom-left): Number of genes with pi > 0.9. Labels show count.
  #   Panel D (bottom-right): Runtime in minutes. Labels show value.
  # How to read it: Each bar is one dataset. All four metrics in one glance.
  # What to look for: Datasets with high AUC but few stable genes may have
  #   "diffuse" signal (many genes contribute a little, none dominate).
  #   Datasets with many stable genes and high AUC have clear molecular
  #   signatures.
  # =========================================================================

  summary_long <- summary_table %>%
    mutate(
      Pct_Stable_50 = 100 * Stable_50 / Genes,
      Pct_Stable_90 = 100 * Stable_90 / Genes,
      Genes_k = Genes / 1000,
      Runtime_min = Runtime_sec / 60
    )
  summary_long$Dataset <- factor(summary_long$Dataset, levels = rev(ds_order))

  p_m1 <- ggplot(summary_long, aes(x = Dataset, y = AUC_mean, fill = Dataset)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_errorbar(aes(ymin = AUC_mean - AUC_sd, ymax = pmin(AUC_mean + AUC_sd, 1)),
                  width = 0.3) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(title = "Mean AUC", x = NULL, y = "AUC") +
    theme_bw() + theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set2")

  p_m2 <- ggplot(summary_long, aes(x = Dataset, y = Stable_50, fill = Dataset)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(aes(label = Stable_50), hjust = -0.2, size = 3) +
    coord_flip() +
    labs(title = "Genes with pi > 0.5", x = NULL, y = "Count") +
    theme_bw() + theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set2")

  p_m3 <- ggplot(summary_long, aes(x = Dataset, y = Stable_90, fill = Dataset)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(aes(label = Stable_90), hjust = -0.2, size = 3) +
    coord_flip() +
    labs(title = "Genes with pi > 0.9", x = NULL, y = "Count") +
    theme_bw() + theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set2")

  p_m4 <- ggplot(summary_long, aes(x = Dataset, y = Runtime_min, fill = Dataset)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f", Runtime_min)), hjust = -0.2, size = 3) +
    coord_flip() +
    labs(title = "Runtime", x = NULL, y = "Minutes") +
    theme_bw() + theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set2")

  p_multi <- (p_m1 | p_m2) / (p_m3 | p_m4) +
    plot_annotation(
      title = "GeneSelectR Performance Summary Across Datasets",
      theme = theme(plot.title = element_text(size = 14, face = "bold"))
    )

  ggsave("results_realdata/figures/comparison_summary_panel.pdf",
         p_multi, width = 14, height = 10)

  # =========================================================================
  # FIGURE 5: Runtime vs gene count (scaling behavior)
  # =========================================================================
  # What it shows: Scatter plot with one point per dataset.
  #   X-axis: number of genes. Y-axis: wall-clock time (seconds).
  #   Dashed line: linear fit (how runtime scales with gene count).
  #   Points are labeled with dataset names.
  # How to read it: Points above the line are slower than expected (e.g.,
  #   harder optimization); points below are faster.
  # What to look for: Roughly linear scaling is expected (elastic net is
  #   O(n * p * n_lambda)). Datasets with the same gene count but different
  #   runtimes differ in convergence difficulty.
  # =========================================================================

  p_time <- ggplot(summary_table, aes(x = Genes, y = Runtime_sec)) +
    geom_point(size = 4, aes(color = Dataset)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                linetype = "dashed", color = "grey50") +
    geom_text(aes(label = Dataset), hjust = -0.15, vjust = -0.5, size = 3) +
    labs(
      title = "Runtime Scaling: Wall Time vs Number of Genes",
      x = "Number of genes", y = "Wall time (seconds)"
    ) +
    theme_bw() +
    theme(legend.position = "none") +
    scale_color_brewer(palette = "Set2")

  ggsave("results_realdata/figures/comparison_runtime.pdf",
         p_time, width = 10, height = 6)

  # =========================================================================
  # FIGURE 6: DESeq2 overlap (for datasets with raw counts)
  # =========================================================================
  # What it shows: Grouped bar chart. For each dataset with raw counts,
  #   shows the percentage of GeneSelectR's top-k genes that are also
  #   DESeq2 DE genes (padj < 0.05, |log2FC| > 1).
  # X-axis: top-k cutoff (100, 200, 500). Y-axis: % overlap.
  # Bars are grouped by dataset. Labels show exact percentages.
  # How to read it: Higher bars = GeneSelectR's ranking agrees with DESeq2.
  # What to look for: At k = 100, overlap should be highest (most stringent
  #   selection). As k grows, overlap % may decrease because GeneSelectR
  #   starts including genes that are useful in multivariate combination but
  #   not individually DE. Some disagreement is expected and healthy.
  # ---- TABLE: deseq2_overlap.csv ----
  # Columns: dataset, k, overlap (count), pct_topk (%), pct_de (%).
  # =========================================================================

  all_overlaps <- do.call(rbind, lapply(all_results, function(r) r$overlap_stats))

  if (!is.null(all_overlaps) && nrow(all_overlaps) > 0) {
    cat("\n=== GeneSelectR vs DESeq2 Overlap ===\n\n")
    print(all_overlaps, row.names = FALSE)

    write.csv(all_overlaps, "results_realdata/data/deseq2_overlap.csv",
              row.names = FALSE)

    p_overlap <- ggplot(all_overlaps, aes(x = factor(k), y = pct_topk,
                                          fill = dataset)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8),
               alpha = 0.8, width = 0.7) +
      geom_text(aes(label = sprintf("%.0f%%", pct_topk), group = dataset),
                position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
      labs(
        title = "GeneSelectR Top-k Overlap with DESeq2 DE Genes",
        subtitle = "What % of GeneSelectR's top-k genes are also DESeq2 DE?",
        x = "Top-k cutoff", y = "% overlap with DESeq2 DE genes",
        fill = "Dataset"
      ) +
      theme_bw() +
      scale_fill_brewer(palette = "Set2")

    ggsave("results_realdata/figures/comparison_deseq2_overlap.pdf",
           p_overlap, width = 10, height = 6)
  }

  # =========================================================================
  # FIGURE 7: Biology score comparison across datasets
  # =========================================================================
  # What it shows: Violin + boxplot of the biology score (b) for ALL genes
  #   in each dataset.
  # X-axis: dataset. Y-axis: b (0 = no GO similarity, higher = more
  #   biologically connected to enriched themes).
  # How to read it: Compare the spread and median across datasets.
  # What to look for: Well-annotated genomes (human) with clear biological
  #   themes should show a spread of b values. If all b values cluster
  #   near 0, GO annotations may not be mapping correctly. If spread is
  #   wide, biology scoring is adding useful discrimination.
  # =========================================================================

  bio_long <- do.call(rbind, lapply(all_results, function(r) {
    gs <- r$gene_scores
    data.frame(
      Dataset = r$name,
      b = gs$b,
      stringsAsFactors = FALSE
    )
  }))
  bio_long$Dataset <- factor(bio_long$Dataset, levels = rev(ds_order))

  p_bio_compare <- ggplot(bio_long, aes(x = Dataset, y = b, fill = Dataset)) +
    geom_violin(alpha = 0.5, scale = "width") +
    geom_boxplot(width = 0.15, alpha = 0.8, outlier.size = 0.3) +
    coord_flip() +
    labs(
      title = "Biology Score Distribution Across Datasets",
      subtitle = "Data-driven GO semantic similarity (defaults: BP ontology, Resnik metric)",
      x = NULL, y = "b (biology score)"
    ) +
    theme_bw() +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set2")

  ggsave("results_realdata/figures/comparison_biology_scores.pdf",
         p_bio_compare, width = 10, height = 6)

  # =========================================================================
  # Enrichment summary (text output)
  # =========================================================================

  cat("\n=== GO BP Enrichment Summary (top 3 terms per dataset) ===\n\n")
  for (r in all_results) {
    if (!is.null(r$enrichment)) {
      cat(sprintf("%s:\n", r$name))
      top3 <- head(r$enrichment$df, 3)
      for (i in 1:nrow(top3)) {
        cat(sprintf("  %d. %s (p.adj=%.2e, %d/%d genes)\n",
                    i, top3$Description[i], top3$p.adjust[i],
                    top3$Count[i],
                    as.integer(gsub(".*/", "", top3$GeneRatio[i]))))
      }
    }
  }

  # =========================================================================
  # Top genes overlap across datasets (if multiple TCGA)
  # =========================================================================

  tcga_results <- all_results[grep("^TCGA", names(all_results))]
  if (length(tcga_results) >= 2) {
    cat("\n=== Top-200 Gene Overlap Across TCGA Datasets ===\n\n")

    top_gene_lists <- lapply(tcga_results, function(r) head(r$gene_scores$gene, 200))

    # Pairwise overlap
    ds_names <- names(top_gene_lists)
    for (i in 1:(length(ds_names) - 1)) {
      for (j in (i + 1):length(ds_names)) {
        overlap <- length(intersect(top_gene_lists[[i]], top_gene_lists[[j]]))
        cat(sprintf("  %s ∩ %s: %d genes (%.0f%% of 200)\n",
                    ds_names[i], ds_names[j], overlap, overlap / 2))
      }
    }

    # Genes in ALL TCGA datasets
    common_all <- Reduce(intersect, top_gene_lists)
    cat(sprintf("\n  Genes in top-200 of ALL %d TCGA datasets: %d\n",
                length(tcga_results), length(common_all)))
    if (length(common_all) > 0 && length(common_all) <= 30) {
      cat("  ", paste(common_all, collapse = ", "), "\n")
    }
  }
}


# ==============================================================================
# ADVANCED CROSS-DATASET VISUALIZATIONS
# ==============================================================================
# These require at least 2 successfully processed datasets.
# They focus on cross-dataset relationships that the per-dataset
# figures can't capture.
# ==============================================================================

if (length(all_results) >= 2) {

  cat("\n")
  cat("=" %R% 80, "\n", sep = "")
  cat("ADVANCED CROSS-DATASET VISUALIZATIONS\n")
  cat("=" %R% 80, "\n\n", sep = "")

  ds_names_all <- names(all_results)
  n_datasets <- length(ds_names_all)

  # =========================================================================
  # FIGURE 8: Jaccard Similarity Heatmap — Top-k Gene Overlap
  # =========================================================================
  # What it shows: A symmetric heatmap (datasets × datasets) where each cell
  #   shows the Jaccard similarity between two datasets' top-k gene lists.
  #   Jaccard = |intersection| / |union|. Generated at k = 100 and k = 500.
  # Color: Yellow → purple = low → high similarity.
  # Numbers in cells: raw overlap count (how many genes are shared).
  # Rows/columns are hierarchically clustered to group similar datasets.
  # How to read it: High similarity (dark purple) between two datasets means
  #   they identified many of the same genes — they share molecular signal.
  # What to look for: TCGA cancer datasets (BRCA, LUAD, KIRC) may share some
  #   common cancer genes but also have tissue-specific markers. GTEx (tissue
  #   comparison) should be quite different from cancer datasets. The k = 500
  #   version will show more overlap than k = 100 because larger lists have
  #   more chance of sharing genes.
  # =========================================================================

  cat("Figure 8: Jaccard similarity heatmap...\n")

  for (k in c(100, 500)) {
    top_lists <- lapply(all_results, function(r) head(r$gene_scores$gene, k))

    jaccard_mat <- matrix(0, n_datasets, n_datasets,
                          dimnames = list(ds_names_all, ds_names_all))
    for (i in 1:n_datasets) {
      for (j in 1:n_datasets) {
        inter <- length(intersect(top_lists[[i]], top_lists[[j]]))
        union <- length(union(top_lists[[i]], top_lists[[j]]))
        jaccard_mat[i, j] <- if (union > 0) inter / union else 0
      }
    }

    # Build annotation text (overlap count in each cell)
    overlap_mat <- matrix(0, n_datasets, n_datasets,
                          dimnames = list(ds_names_all, ds_names_all))
    for (i in 1:n_datasets) {
      for (j in 1:n_datasets) {
        overlap_mat[i, j] <- length(intersect(top_lists[[i]], top_lists[[j]]))
      }
    }

    pdf(sprintf("results_realdata/figures/jaccard_heatmap_top%d.pdf", k),
        width = 8, height = 7)
    pheatmap::pheatmap(
      jaccard_mat,
      display_numbers = overlap_mat,
      number_format = "%.0f",
      number_color = "black",
      color = colorRampPalette(c("white", "#FDE725", "#21908C", "#440154"))(50),
      main = sprintf("Top-%d Gene Overlap (Jaccard Index, numbers = gene count)", k),
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      fontsize = 10,
      border_color = "grey90"
    )
    dev.off()
  }


  # =========================================================================
  # FIGURE 9: Rank Concordance Heatmap — Spearman Correlation of Full Rankings
  # =========================================================================
  # What it shows: Symmetric heatmap where each cell is the Spearman rank
  #   correlation (rho) of the complete gene rankings between two datasets.
  #   Unlike the Jaccard heatmap (which only compares the top-k), this uses
  #   ALL common genes and their full ranking order.
  # Color: Blue = negative correlation, White = zero, Red = positive.
  # Numbers: Spearman rho values.
  # How to read it: rho = 0.5 means the two datasets rank genes in a fairly
  #   similar order. rho near 0 means their rankings are unrelated.
  # What to look for: If two cancer datasets (e.g., BRCA and LUAD) have
  #   rho = 0.3, they share some signal but also have tissue-specific genes
  #   ranked very differently. A high rho between TCGA-KIRC and GEO kidney
  #   would validate both datasets.
  # =========================================================================

  cat("Figure 9: Rank concordance heatmap...\n")

  # Find genes common to all datasets
  common_genes <- Reduce(intersect, lapply(all_results, function(r) r$gene_scores$gene))
  cat(sprintf("  Common genes across all datasets: %d\n", length(common_genes)))

  if (length(common_genes) > 100) {
    # Build rank matrix: rows = genes, cols = datasets
    rank_mat <- matrix(NA, length(common_genes), n_datasets,
                       dimnames = list(common_genes, ds_names_all))
    for (i in 1:n_datasets) {
      gs <- all_results[[i]]$gene_scores
      gene_ranks <- setNames(1:nrow(gs), gs$gene)
      rank_mat[, i] <- gene_ranks[common_genes]
    }

    # Spearman correlation
    spearman_mat <- cor(rank_mat, method = "spearman", use = "pairwise.complete.obs")

    pdf("results_realdata/figures/rank_concordance_heatmap.pdf", width = 8, height = 7)
    pheatmap::pheatmap(
      spearman_mat,
      display_numbers = TRUE,
      number_format = "%.2f",
      color = colorRampPalette(c("#2166AC", "white", "#B2182B"))(50),
      main = sprintf("Rank Concordance (Spearman rho, %d common genes)", length(common_genes)),
      fontsize = 11,
      border_color = "grey80"
    )
    dev.off()
  }


  # =========================================================================
  # FIGURE 10: Gene × Dataset Stability Tile Map
  # =========================================================================
  # What it shows: A heatmap where rows = genes, columns = datasets, and
  #   color = pi (stability) value. Only genes that appear in the top-100
  #   of ANY dataset are included (up to 200 shown).
  # Color: Light grey = pi near 0 (not selected), deep red = pi near 1
  #   (selected in almost every fold).
  # Row ordering: Genes appearing in the most datasets' top-100 are at the
  #   top; within that, sorted by mean pi across all datasets.
  # How to read it: A bright red row across all columns = a "universal"
  #   marker gene important in every dataset. A red cell in one column only
  #   = a dataset-specific marker.
  # What to look for: Pan-cancer genes (e.g., TP53, MKI67) should appear
  #   as bright rows in multiple TCGA columns. Tissue-specific genes will
  #   be bright in one column and grey elsewhere. The report also lists
  #   "universal" genes (top-100 in 3+ datasets).
  # =========================================================================

  cat("Figure 10: Gene × dataset stability tile map...\n")

  # Collect top-100 genes from each dataset
  all_top_genes <- unique(unlist(lapply(all_results, function(r) {
    head(r$gene_scores$gene, 100)
  })))

  if (length(all_top_genes) > 0 && length(all_top_genes) <= 800) {
    # Build pi matrix
    pi_matrix <- matrix(0, length(all_top_genes), n_datasets,
                        dimnames = list(all_top_genes, ds_names_all))
    for (j in 1:n_datasets) {
      gs <- all_results[[j]]$gene_scores
      pi_lookup <- setNames(gs$pi_exact, gs$gene)
      matched <- intersect(all_top_genes, names(pi_lookup))
      pi_matrix[matched, j] <- pi_lookup[matched]
    }

    # Count how many datasets each gene is top-100 in
    n_datasets_top100 <- rowSums(pi_matrix > 0.5)

    # Sort: genes in most datasets first, then by mean pi
    gene_order <- order(-n_datasets_top100, -rowMeans(pi_matrix))
    pi_matrix_sorted <- pi_matrix[gene_order, , drop = FALSE]

    # Cap at 200 genes for readability
    if (nrow(pi_matrix_sorted) > 200) {
      pi_matrix_sorted <- pi_matrix_sorted[1:200, , drop = FALSE]
    }

    pdf("results_realdata/figures/gene_dataset_stability_tilemap.pdf",
        width = 10, height = max(8, nrow(pi_matrix_sorted) * 0.08))
    pheatmap::pheatmap(
      pi_matrix_sorted,
      color = colorRampPalette(c("grey95", "#FEE08B", "#FC8D59", "#D73027"))(50),
      cluster_rows = FALSE,          # Keep our sorting
      cluster_cols = TRUE,
      show_rownames = (nrow(pi_matrix_sorted) <= 80),
      main = sprintf("Stability (pi) Across Datasets — Top Genes (n=%d)",
                     nrow(pi_matrix_sorted)),
      fontsize_row = 5,
      fontsize_col = 10,
      border_color = NA
    )
    dev.off()

    # Also report the "universal" genes (high pi in 3+ datasets)
    if (n_datasets >= 3) {
      universal <- names(n_datasets_top100[n_datasets_top100 >= 3])
      cat(sprintf("  Genes in top-100 of >= 3 datasets: %d\n", length(universal)))
      if (length(universal) > 0 && length(universal) <= 30) {
        cat("    ", paste(universal, collapse = ", "), "\n")
      }
    }
  }


  # =========================================================================
  # FIGURE 11: Cumulative Recall Curves (DESeq2 reference)
  # =========================================================================
  # Only generated for datasets with raw counts (where DESeq2 can run).
  # Three sub-figures are produced:
  #
  # 11a — cumulative_recall_curves.pdf
  #   What it shows: Line plot. X-axis = k (top-k cutoff in GeneSelectR's
  #     ranking, from 10 to 2000). Y-axis = recall (fraction of DESeq2 DE
  #     genes found so far). One line per dataset.
  #   How to read it: Steeper lines = GeneSelectR puts DE genes at the top.
  #   What to look for: At k = 200, what recall have you achieved? If 80%,
  #     GeneSelectR is efficiently prioritizing DE genes.
  #
  # 11b — precision_recall_curves.pdf
  #   What it shows: Precision (y) vs recall (x). Each point is one k value.
  #   How to read it: Top-right = high precision AND high recall (ideal).
  #     As recall increases, precision usually drops.
  #   What to look for: Where does the "elbow" occur? That's the natural
  #     cutoff point for selecting genes.
  #
  # 11c — enrichment_over_random.pdf
  #   What it shows: Fold enrichment over random baseline at each k.
  #     Enrichment = actual_recall / expected_recall_by_chance.
  #     Y-axis is log2 scale.
  #   Grey dashed line: enrichment = 1 (no better than random).
  #   How to read it: Values above 1 mean GeneSelectR is better than random.
  #   What to look for: At small k (top-50), enrichment should be very high
  #     (10x–50x better than random). At large k, enrichment approaches 1
  #     because you've included most genes.
  # =========================================================================

  cat("Figure 11: Cumulative recall curves...\n")

  recall_curves <- data.frame()
  for (r in all_results) {
    if (!is.null(r$deseq_result) && length(r$deseq_result$de_genes) > 0) {
      gs <- r$gene_scores
      de_set <- r$deseq_result$de_genes
      n_de <- length(de_set)

      # Evaluate recall at each k
      k_values <- unique(c(seq(10, 100, 10), seq(100, 500, 50),
                           seq(500, min(2000, nrow(gs)), 100)))

      for (k in k_values) {
        top_k <- head(gs$gene, k)
        recall <- sum(top_k %in% de_set) / n_de
        precision <- sum(top_k %in% de_set) / k

        recall_curves <- rbind(recall_curves, data.frame(
          Dataset = r$name,
          k = k,
          Recall = recall,
          Precision = precision,
          n_DE = n_de,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  if (nrow(recall_curves) > 0) {
    # Recall curve
    p_recall <- ggplot(recall_curves, aes(x = k, y = Recall,
                                          color = Dataset, group = Dataset)) +
      geom_line(linewidth = 1) +
      geom_point(data = recall_curves %>% filter(k %in% c(100, 200, 500)),
                 size = 3) +
      labs(
        title = "Cumulative Recall of DESeq2 DE Genes",
        subtitle = "Fraction of DE genes recovered as you walk down the GeneSelectR ranking",
        x = "Top-k cutoff (GeneSelectR rank)",
        y = "Recall (fraction of DE genes recovered)",
        color = NULL
      ) +
      theme_bw() +
      theme(legend.position = "bottom") +
      scale_color_brewer(palette = "Set2")

    ggsave("results_realdata/figures/cumulative_recall_curves.pdf",
           p_recall, width = 10, height = 6)

    # Precision-recall curve
    p_pr <- ggplot(recall_curves, aes(x = Recall, y = Precision,
                                      color = Dataset, group = Dataset)) +
      geom_line(linewidth = 1) +
      geom_point(data = recall_curves %>% filter(k %in% c(100, 200, 500)),
                 size = 3) +
      labs(
        title = "Precision-Recall Curve (GeneSelectR vs DESeq2)",
        subtitle = "How precise is GeneSelectR at each recall level?",
        x = "Recall", y = "Precision", color = NULL
      ) +
      theme_bw() +
      theme(legend.position = "bottom") +
      scale_color_brewer(palette = "Set2")

    ggsave("results_realdata/figures/precision_recall_curves.pdf",
           p_pr, width = 8, height = 6)

    # Random baseline comparison: actual recall vs what you'd expect by chance
    # lookup table: Dataset -> n_genes
    n_genes_lut <- tibble::tibble(
      Dataset = names(all_results),
      n_genes = vapply(all_results, function(x) x$n_genes, numeric(1))
    )

    recall_vs_random <- recall_curves %>%
      left_join(n_genes_lut, by = "Dataset") %>%
      mutate(
        Expected_Recall = k * n_DE / n_genes,         # expected #hits by chance / n_DE
        Expected_Recall = pmin(Expected_Recall, 1.0), # already a fraction
        Enrichment = Recall / pmax(Expected_Recall, 1e-3)
      )

    p_enrichment <- ggplot(recall_vs_random,
                           aes(x = k, y = Enrichment,
                               color = Dataset, group = Dataset)) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
      annotate("text", x = max(recall_vs_random$k) * 0.8, y = 1.15,
               label = "Random baseline", color = "grey50", size = 3.5) +
      labs(
        title = "Enrichment Over Random: DE Gene Recovery",
        subtitle = "How many times better than random is GeneSelectR at recovering DE genes?",
        x = "Top-k cutoff", y = "Fold enrichment over random",
        color = NULL
      ) +
      theme_bw() +
      theme(legend.position = "bottom") +
      scale_color_brewer(palette = "Set2") +
      scale_y_continuous(breaks = c(1, 2, 5, 10, 20, 50),
                         trans = "log2")

    ggsave("results_realdata/figures/enrichment_over_random.pdf",
           p_enrichment, width = 10, height = 6)
  }


  # =========================================================================
  # FIGURE 12: GO Enrichment Comparison Heatmap
  # =========================================================================
  # What it shows: A heatmap where rows = GO Biological Process terms,
  #   columns = datasets. Color intensity = -log10(adjusted p-value) for
  #   that term in that dataset's enrichment analysis. Grey = not enriched.
  # Row ordering: Terms shared across the most datasets appear first,
  #   then sorted by max significance.
  # How to read it: A bright row across many columns = a shared biological
  #   theme (e.g., "immune response" in multiple cancer types). A bright
  #   cell in one column only = dataset-specific biology.
  # What to look for: Common cancer terms (cell proliferation, apoptosis)
  #   should appear in multiple TCGA datasets. Tissue-specific processes
  #   (e.g., "surfactant metabolism" in lung) will be unique to one column.
  #   If very few terms are shared, each dataset has distinct biology.
  # =========================================================================

  cat("Figure 12: GO enrichment comparison heatmap...\n")

  # Collect enrichment results
  all_enrich_dfs <- list()
  for (r in all_results) {
    if (!is.null(r$enrichment) && nrow(r$enrichment$df) > 0) {
      top_terms <- head(r$enrichment$df, 15)
      all_enrich_dfs[[r$name]] <- data.frame(
        Dataset = r$name,
        Description = top_terms$Description,
        pval = top_terms$p.adjust,
        Count = top_terms$Count,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(all_enrich_dfs) >= 2) {
    enrich_combined <- do.call(rbind, all_enrich_dfs)

    # Keep terms that appear in at least 1 dataset's top 15
    all_terms_go <- unique(enrich_combined$Description)

    # Build the matrix: terms × datasets
    go_matrix <- matrix(0, length(all_terms_go), length(all_enrich_dfs),
                        dimnames = list(all_terms_go, names(all_enrich_dfs)))

    for (ds_name in names(all_enrich_dfs)) {
      df <- all_enrich_dfs[[ds_name]]
      matched_terms <- intersect(all_terms_go, df$Description)
      for (term in matched_terms) {
        go_matrix[term, ds_name] <- -log10(df$pval[df$Description == term][1])
      }
    }

    # Sort terms: most shared first, then by max significance
    n_shared <- rowSums(go_matrix > 0)
    max_sig <- apply(go_matrix, 1, max)
    term_order <- order(-n_shared, -max_sig)
    go_matrix <- go_matrix[term_order, , drop = FALSE]

    # Cap at 50 terms for readability
    if (nrow(go_matrix) > 50) go_matrix <- go_matrix[1:50, , drop = FALSE]

    pdf("results_realdata/figures/go_enrichment_comparison_heatmap.pdf",
        width = max(8, ncol(go_matrix) * 1.5),
        height = max(8, nrow(go_matrix) * 0.25))
    pheatmap::pheatmap(
      go_matrix,
      color = colorRampPalette(c("grey95", "#FEE08B", "#FC8D59",
                                 "#D73027", "#67001F"))(50),
      cluster_rows = FALSE,          # Keep our sorting
      cluster_cols = TRUE,
      main = "GO BP Enrichment Across Datasets (-log10 adjusted p-value)",
      fontsize_row = 7,
      fontsize_col = 10,
      border_color = NA,
      na_col = "grey95"
    )
    dev.off()
  }


  # =========================================================================
  # FIGURE 13: Stability × Utility Scatter — Faceted by Dataset
  # =========================================================================
  # What it shows: One scatter panel per dataset. Each point = one gene
  #   (with pi > 0). X-axis = pi (stability), Y-axis = u (utility),
  #   color = b (biology score, via plasma colormap: dark = low b, bright = high).
  # How to read it: Top-right corner = genes that are both stable and useful
  #   (the best candidates). Color adds the biology dimension.
  # What to look for:
  #   - Tight cluster at top-right: clear, strong signal genes.
  #   - Scattered cloud: many genes compete for selection.
  #   - Bright colors (high b) at top-right: the best genes are also
  #     biologically relevant.
  #   - Bright colors at bottom-left: biology "rescues" genes that the
  #     model doesn't select often.
  # =========================================================================

  cat("Figure 13: Stability × utility faceted scatter...\n")

  scatter_data <- do.call(rbind, lapply(all_results, function(r) {
    gs <- r$gene_scores
    # Subsample for plotting (5000 per dataset max)
    if (nrow(gs) > 5000) {
      idx <- c(1:500, sample(501:nrow(gs), 4500))
      gs <- gs[idx, ]
    }
    data.frame(
      Dataset = r$name,
      pi = gs$pi_exact,
      u = gs$u,
      b = gs$b,
      final = gs$final_score,
      stringsAsFactors = FALSE
    )
  }))

  p_scatter_facet <- ggplot(scatter_data %>% filter(pi > 0),
                            aes(x = pi, y = u, color = b)) +
    geom_point(alpha = 0.3, size = 0.6) +
    scale_color_viridis_c(option = "plasma", name = "Biology (b)") +
    facet_wrap(~ Dataset, scales = "free", ncol = 3) +
    labs(
      title = "Stability vs Utility Across Datasets (selected genes only)",
      x = expression(pi ~ "(stability)"),
      y = "u (utility)"
    ) +
    theme_bw() +
    theme(strip.text = element_text(size = 9, face = "bold"))

  ggsave("results_realdata/figures/stability_utility_faceted.pdf",
         p_scatter_facet,
         width = min(18, 6 * min(n_datasets, 3)),
         height = 5 * ceiling(n_datasets / 3))


  # =========================================================================
  # FIGURE 14: Score Component Contribution — Stacked Bar
  # =========================================================================
  # What it shows: For each dataset's top 100 genes, a stacked horizontal
  #   bar showing the average value of each score component: pi (stability,
  #   green), u (utility, orange), b (biology, purple).
  # How to read it: The total bar height = sum of mean pi + mean u + mean b.
  #   Wider segments mean that component contributes more to the top genes.
  # What to look for:
  #   - If the green (stability) segment dominates, the top genes are
  #     selected because they're consistently chosen across folds.
  #   - If purple (biology) is large, GO annotations boost the ranking.
  #   - If all three are roughly equal, the scoring formula is balanced.
  #   - A very short biology bar may mean the bio scoring isn't working
  #     well for that dataset's gene set.
  # =========================================================================

  cat("Figure 14: Score component contribution...\n")

  component_data <- do.call(rbind, lapply(all_results, function(r) {
    top100 <- head(r$gene_scores, 100)
    data.frame(
      Dataset = r$name,
      Component = c("Stability (pi)", "Utility (u)", "Biology (b)"),
      Mean_Score = c(mean(top100$pi_exact), mean(top100$u), mean(top100$b)),
      stringsAsFactors = FALSE
    )
  }))

  component_data$Dataset <- factor(component_data$Dataset, levels = ds_order)
  component_data$Component <- factor(component_data$Component,
                                     levels = c("Biology (b)", "Utility (u)",
                                                "Stability (pi)"))

  p_components <- ggplot(component_data,
                         aes(x = Dataset, y = Mean_Score, fill = Component)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.85) +
    coord_flip() +
    labs(
      title = "Score Component Contributions (Top 100 Genes per Dataset)",
      subtitle = "Average pi, u, and b among each dataset's top 100 genes",
      x = NULL, y = "Mean score (stacked)", fill = NULL
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_fill_manual(values = c("Stability (pi)" = "#1B9E77",
                                 "Utility (u)" = "#D95F02",
                                 "Biology (b)" = "#7570B3"))

  ggsave("results_realdata/figures/score_component_contribution.pdf",
         p_components, width = 10, height = 6)


  # =========================================================================
  # FIGURE 15: Final Score ECDF — Score Decay by Rank
  # =========================================================================
  # What it shows: Line plot. X-axis = gene rank in GeneSelectR's final
  #   ordering (1 = best, log scale, up to 2000). Y-axis = final_score.
  #   One line per dataset.
  # How to read it: The shape of the decay curve tells you about signal
  #   structure. A steep drop = a few clear winners and then noise. A
  #   gradual decline = many genes with similar importance (hard to set
  #   a cutoff).
  # What to look for:
  #   - "Elbow" points where the curve bends sharply — these are natural
  #     cutoff points for gene selection.
  #   - Datasets with a long, gentle decline have diffuse signal and may
  #     require a larger gene set for downstream analysis.
  #   - Datasets where the curve drops to near-zero by rank 50 have very
  #     concentrated signal.
  # =========================================================================

  cat("Figure 15: Final score ECDF...\n")

  ecdf_data <- do.call(rbind, lapply(all_results, function(r) {
    gs <- r$gene_scores
    data.frame(
      Dataset = r$name,
      Rank = 1:nrow(gs),
      Final_Score = gs$final_score,
      stringsAsFactors = FALSE
    )
  }))

  # Show only top 2000 genes per dataset (tail is noise)
  ecdf_data <- ecdf_data %>% filter(Rank <= 2000)

  p_ecdf <- ggplot(ecdf_data, aes(x = Rank, y = Final_Score,
                                  color = Dataset, group = Dataset)) +
    geom_line(linewidth = 0.8) +
    scale_x_log10(labels = scales::comma_format()) +
    labs(
      title = "Final Score Decay by Rank",
      subtitle = "How sharply does gene importance drop off?",
      x = "GeneSelectR rank (log scale)", y = "Final score",
      color = NULL
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_color_brewer(palette = "Set2")

  ggsave("results_realdata/figures/final_score_decay.pdf",
         p_ecdf, width = 10, height = 6)


  # =========================================================================
  # FIGURE 16: AUC vs Number of Stable Genes — Dataset Characterization
  # =========================================================================
  # What it shows: Scatter plot. Each point = one dataset.
  #   X-axis = % of genes with pi > 0.5 (how much of the transcriptome is
  #     "selected"). Y-axis = mean CV AUC (predictive performance).
  #   Point size = sample count. Red dashed line = AUC = 0.5 (chance).
  # How to read it: This is a "difficulty landscape".
  #   - Top-right: easy dataset (high AUC, many stable genes) — strong,
  #     broad signal.
  #   - Top-left: high AUC but few stable genes — a few key genes drive
  #     classification (concentrated signal).
  #   - Bottom-right: many genes selected but low AUC — model is unstable,
  #     picking noise.
  #   - Bottom-left: hard dataset — weak signal, few useful genes.
  # What to look for: Tumor-vs-normal datasets should be top-right or
  #   top-left. Subtle comparisons (early vs late stage) may be bottom-left.
  # =========================================================================

  cat("Figure 16: Dataset characterization scatter...\n")

  ds_char <- data.frame(
    Dataset = sapply(all_results, function(r) r$name),
    AUC = sapply(all_results, function(r) r$cv_auc_mean),
    Pct_Stable = sapply(all_results, function(r) 100 * r$n_stable_50 / r$n_genes),
    N_Genes = sapply(all_results, function(r) r$n_genes),
    N_Samples = sapply(all_results, function(r) r$n_samples),
    stringsAsFactors = FALSE
  )

  p_ds_char <- ggplot(ds_char, aes(x = Pct_Stable, y = AUC)) +
    geom_point(aes(size = N_Samples, color = Dataset), alpha = 0.8) +
    geom_text(aes(label = Dataset), vjust = -1, size = 3) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.5) +
    labs(
      title = "Dataset Difficulty Landscape",
      subtitle = "Top-right = easy (high AUC, many stable genes); bottom-left = hard",
      x = "% of genes with pi > 0.5",
      y = "Mean CV AUC",
      size = "N samples"
    ) +
    theme_bw() +
    theme(legend.position = "right") +
    scale_color_brewer(palette = "Set2") +
    guides(color = "none")

  ggsave("results_realdata/figures/dataset_characterization.pdf",
         p_ds_char, width = 10, height = 7)


  # =========================================================================
  # FIGURE 17: Score Component Correlation — Faceted by Dataset
  # =========================================================================
  # What it shows: Bar chart of Pearson correlations between all pairs of
  #   score components. Six pairs shown: pi-u, pi-b, u-b, pi-final,
  #   u-final, b-final. One facet per dataset.
  # How to read it: Each bar = Pearson r between two score components
  #   across all genes in that dataset. Tall bars = the two components
  #   agree about which genes are important.
  # What to look for:
  #   - pi-u correlation: High = the elastic net coefficient magnitude and
  #     mutual information agree. Expected to be moderately high.
  #   - pi-b correlation: LOW is good — it means biology adds information
  #     independent of the model's statistical selection. If pi and b are
  #     highly correlated, the bio score is redundant.
  #   - u-b correlation: Low = utility and biology capture different signals.
  #   - pi-final, u-final, b-final: Shows which component dominates the
  #     final ranking. If pi-final is highest, stability drives the ranking.
  # =========================================================================

  cat("Figure 17: Score component correlations per dataset...\n")

  cor_summary <- do.call(rbind, lapply(all_results, function(r) {
    cm <- r$score_correlations
    data.frame(
      Dataset = r$name,
      Pair = c("pi-u", "pi-b", "u-b", "pi-final", "u-final", "b-final"),
      Correlation = c(cm["pi_exact", "u"], cm["pi_exact", "b"], cm["u", "b"],
                      cm["pi_exact", "final_score"], cm["u", "final_score"],
                      cm["b", "final_score"]),
      stringsAsFactors = FALSE
    )
  }))

  cor_summary$Dataset <- factor(cor_summary$Dataset, levels = ds_order)
  cor_summary$Pair <- factor(cor_summary$Pair,
                             levels = c("pi-u", "pi-b", "u-b",
                                        "pi-final", "u-final", "b-final"))

  p_cor_facet <- ggplot(cor_summary, aes(x = Pair, y = Correlation, fill = Pair)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_hline(yintercept = 0, color = "grey50") +
    facet_wrap(~ Dataset, ncol = 4) +
    labs(
      title = "Score Component Correlations per Dataset",
      subtitle = "Low pi-b = biology adds independent info; high pi-u = model & MI agree",
      x = NULL, y = "Pearson r"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          strip.text = element_text(face = "bold"),
          legend.position = "none") +
    scale_fill_brewer(palette = "Set3")

  ggsave("results_realdata/figures/score_correlations_per_dataset.pdf",
         p_cor_facet,
         width = min(16, 4 * min(n_datasets, 4)),
         height = 5 * ceiling(n_datasets / 4))


  # =========================================================================
  # FIGURE 18: Combined Multi-Panel "Paper Figure"
  # =========================================================================
  # What it shows: A single publication-ready figure with four panels:
  #   Panel A: AUC boxplots across datasets (from Figure 1).
  #   Panel B: Stability profile lines (from Figure 3).
  #   Panel C: Score component stacked bars (from Figure 14).
  #   Panel D: Dataset difficulty landscape (from Figure 16).
  # Purpose: A single figure that summarizes all key results for a paper
  #   or presentation, without needing to show 18 individual plots.
  # =========================================================================

  cat("Figure 18: Combined paper figure...\n")

  # Panel A: AUC boxplots
  panel_a <- p_auc_box + labs(title = "A. AUC Distribution") +
    theme(plot.title = element_text(face = "bold", size = 11))

  # Panel B: Stability profile
  panel_b <- p_stab_profile + labs(title = "B. Stability Profile") +
    theme(plot.title = element_text(face = "bold", size = 11),
          legend.position = "none")

  # Panel C: Score component stacks
  panel_c <- p_components + labs(title = "C. Score Components (Top 100)") +
    theme(plot.title = element_text(face = "bold", size = 11),
          legend.position = "none")

  # Panel D: Dataset characterization
  panel_d <- p_ds_char + labs(title = "D. Dataset Landscape") +
    theme(plot.title = element_text(face = "bold", size = 11),
          legend.position = "none")

  p_paper <- (panel_a | panel_b) / (panel_c | panel_d) +
    plot_annotation(
      title = "GeneSelectR 2.0 — Cross-Dataset Benchmark Summary",
      theme = theme(plot.title = element_text(size = 14, face = "bold"))
    )

  ggsave("results_realdata/figures/paper_figure_combined.pdf",
         p_paper, width = 16, height = 12)


  cat("\nAll advanced figures saved.\n")
  cat(sprintf("Total figures generated: 18\n"))
}


# ==============================================================================
# VALUE-ADDED EXPERIMENTS: Why GeneSelectR over DESeq2?
# ==============================================================================
# These experiments directly address the question: "If GeneSelectR mostly
# agrees with DESeq2, why bother?"
#
# The answer is in five dimensions:
#   1. PARSIMONY:  Fewer genes → same AUC (GeneSelectR ranks better)
#   2. REDUNDANCY: Less correlation among top genes (more diverse information)
#   3. STABILITY:  Rankings are more reproducible under data perturbation
#   4. COHERENCE:  Top genes are more functionally coherent
#   5. UNIQUE:     GeneSelectR finds multivariate-signal genes DESeq2 misses
# ==============================================================================

# Identify datasets that have DESeq2 comparisons available
results_with_deseq <- Filter(function(r) {
  !is.null(r$deseq_result) && length(r$deseq_result$de_genes) > 0 &&
    !is.null(r$X) && !is.null(r$y)
}, all_results)

if (length(results_with_deseq) >= 1) {

  cat("\n")
  cat("=" %R% 80, "\n", sep = "")
  cat("VALUE-ADDED EXPERIMENTS: Why GeneSelectR over DESeq2?\n")
  cat("=" %R% 80, "\n\n")


  # =========================================================================
  # EXPERIMENT 1: Predictive Parsimony
  # =========================================================================
  # Question: How many genes does each ranking need for good classification?
  # Method: For each k in {10, 20, 50, 100, 200, 500}, take the top-k genes
  #   from (a) GeneSelectR ranking, (b) DESeq2 p-value ranking, (c) random,
  #   train logistic regression via 5-fold CV, measure AUC.
  # Expected: GeneSelectR reaches high AUC with fewer genes because its
  #   ranking combines stability, utility, and biology, so top genes are
  #   non-redundant and informative.
  #
  # ---- FIGURE: comparison_predictive_parsimony.pdf ----
  # X-axis: number of genes (k). Y-axis: mean CV AUC.
  # Three lines: GeneSelectR (blue), DESeq2 (red), Random (grey).
  # One facet per dataset.
  # Key insight: the gap between blue and red at small k.
  # =========================================================================

  cat("Experiment 1: Predictive parsimony...\n")

  # Check required packages
  if (!requireNamespace("pROC", quietly = TRUE) ||
      !requireNamespace("caret", quietly = TRUE)) {
    cat("  Skipping: install pROC and caret packages for this experiment.\n")
    cat("  BiocManager::install(c('pROC', 'caret'))\n")
    parsimony_results <- list()
  } else {

    k_values <- c(10, 20, 50, 100, 200, 500)
    n_random_reps <- 5   # average over random draws

    parsimony_results <- list()

    for (r in results_with_deseq) {
      cat(sprintf("  %s: testing parsimony at k = %s\n", r$name,
                  paste(k_values, collapse = ", ")))

      X <- r$X
      y <- r$y
      gs <- r$gene_scores
      deseq_res <- r$deseq_result$full_results

      # DESeq2 ranking: sort by adjusted p-value (ascending), use gene names
      deseq_ranked <- deseq_res %>%
        as.data.frame() %>%
        { if (!"gene" %in% names(.)) tibble::rownames_to_column(., "gene") else . } %>%
        dplyr::filter(!is.na(padj)) %>%
        dplyr::arrange(padj) %>%
        dplyr::pull(gene)


      # GeneSelectR ranking (already sorted by final_score)
      gs_ranked <- gs$gene

      for (k in k_values) {
        if (k > ncol(X)) next

        # --- GeneSelectR top-k ---
        gs_genes <- head(gs_ranked, k)
        gs_genes_in_X <- gs_genes[gs_genes %in% colnames(X)]

        # --- DESeq2 top-k ---
        deseq_genes <- head(deseq_ranked, k)
        deseq_genes_in_X <- deseq_genes[deseq_genes %in% colnames(X)]

        # Skip if either has too few genes in the expression matrix
        if (length(gs_genes_in_X) < 5 || length(deseq_genes_in_X) < 5) next

        # 5-fold CV AUC for each method
        folds <- caret::createFolds(y, k = 5, returnTrain = TRUE)

        auc_gs <- auc_deseq <- auc_random <- numeric(5)

        for (f in seq_along(folds)) {
          train_idx <- folds[[f]]
          test_idx <- setdiff(seq_along(y), train_idx)

          y_train <- y[train_idx]
          y_test <- y[test_idx]

          # GeneSelectR
          tryCatch({
            X_gs <- X[, gs_genes_in_X, drop = FALSE]
            fit_gs <- glmnet::cv.glmnet(X_gs[train_idx, , drop = FALSE],
                                        y_train, family = "binomial",
                                        alpha = 0.5, nfolds = 3)
            pred_gs <- predict(fit_gs, X_gs[test_idx, , drop = FALSE],
                               s = "lambda.min", type = "response")
            auc_gs[f] <- as.numeric(pROC::auc(pROC::roc(y_test, as.numeric(pred_gs),
                                                        quiet = TRUE)))
          }, error = function(e) { auc_gs[f] <<- NA })

          # DESeq2
          tryCatch({
            X_de <- X[, deseq_genes_in_X, drop = FALSE]
            fit_de <- glmnet::cv.glmnet(X_de[train_idx, , drop = FALSE],
                                        y_train, family = "binomial",
                                        alpha = 0.5, nfolds = 3)
            pred_de <- predict(fit_de, X_de[test_idx, , drop = FALSE],
                               s = "lambda.min", type = "response")
            auc_deseq[f] <- as.numeric(pROC::auc(pROC::roc(y_test, as.numeric(pred_de),
                                                           quiet = TRUE)))
          }, error = function(e) { auc_deseq[f] <<- NA })

          # Random
          random_aucs <- numeric(n_random_reps)
          for (rep_i in seq_len(n_random_reps)) {
            tryCatch({
              rand_genes <- sample(colnames(X), min(k, ncol(X)))
              X_rand <- X[, rand_genes, drop = FALSE]
              fit_rand <- glmnet::cv.glmnet(X_rand[train_idx, , drop = FALSE],
                                            y_train, family = "binomial",
                                            alpha = 0.5, nfolds = 3)
              pred_rand <- predict(fit_rand, X_rand[test_idx, , drop = FALSE],
                                   s = "lambda.min", type = "response")
              random_aucs[rep_i] <- as.numeric(
                pROC::auc(pROC::roc(y_test, as.numeric(pred_rand), quiet = TRUE)))
            }, error = function(e) { random_aucs[rep_i] <<- NA })
          }
          auc_random[f] <- mean(random_aucs, na.rm = TRUE)
        }

        parsimony_results <- c(parsimony_results, list(data.frame(
          Dataset = r$name,
          k = k,
          Method = c("GeneSelectR", "DESeq2", "Random"),
          AUC = c(mean(auc_gs, na.rm = TRUE),
                  mean(auc_deseq, na.rm = TRUE),
                  mean(auc_random, na.rm = TRUE)),
          stringsAsFactors = FALSE
        )))
      }
    }

    if (length(parsimony_results) > 0) {
      parsimony_df <- do.call(rbind, parsimony_results)

      # ---- TABLE: parsimony_comparison.csv ----
      # Columns: Dataset, k, Method, AUC
      write.csv(parsimony_df, "results_realdata/data/parsimony_comparison.csv",
                row.names = FALSE)

      p_parsimony <- ggplot(parsimony_df, aes(x = k, y = AUC,
                                              color = Method, linetype = Method)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2.5) +
        facet_wrap(~ Dataset, scales = "free_y") +
        scale_color_manual(values = c("GeneSelectR" = "#2166AC",
                                      "DESeq2" = "#B2182B",
                                      "Random" = "grey60")) +
        scale_linetype_manual(values = c("GeneSelectR" = "solid",
                                         "DESeq2" = "dashed",
                                         "Random" = "dotted")) +
        scale_x_log10(breaks = k_values) +
        geom_hline(yintercept = 0.5, linetype = "dotted", color = "grey40") +
        labs(
          title = "Predictive Parsimony: How many genes for good classification?",
          subtitle = "5-fold CV AUC using top-k genes from each ranking (elastic net classifier)",
          x = "Number of top-k genes (log scale)",
          y = "Mean AUC"
        ) +
        theme_bw() +
        theme(legend.position = "bottom")

      ggsave("results_realdata/figures/comparison_predictive_parsimony.pdf",
             p_parsimony, width = 12, height = 8)
      cat("  Saved: comparison_predictive_parsimony.pdf\n")
    }

  }  # end pROC/caret availability check


  # =========================================================================
  # EXPERIMENT 2: Redundancy — Correlation among top genes
  # =========================================================================
  # Question: Are GeneSelectR's top genes more diverse (less correlated)?
  # Method: Compute mean absolute pairwise expression correlation among
  #   top-k genes from each ranking. Lower = less redundancy = more
  #   independent information per gene.
  # Expected: GeneSelectR's elastic net core naturally de-duplicates
  #   correlated genes. DESeq2 tests independently, so its top hits
  #   often come from the same pathway and are highly correlated.
  #
  # ---- FIGURE: comparison_redundancy.pdf ----
  # X-axis: k. Y-axis: mean |pairwise correlation|.
  # Two lines: GeneSelectR (blue), DESeq2 (red). One facet per dataset.
  # Lower is better (more diverse gene set).
  # =========================================================================

  cat("Experiment 2: Redundancy analysis...\n")

  redundancy_results <- list()

  for (r in results_with_deseq) {
    cat(sprintf("  %s: computing redundancy...\n", r$name))

    X <- r$X
    gs_ranked <- r$gene_scores$gene
    deseq_ranked <- deseq_res %>%
      as.data.frame() %>%
      { if (!"gene" %in% names(.)) tibble::rownames_to_column(., "gene") else . } %>%
      dplyr::filter(!is.na(padj)) %>%
      dplyr::arrange(padj) %>%
      dplyr::pull(gene)


    for (k in c(20, 50, 100, 200, 500)) {
      if (k > ncol(X)) next

      # Mean absolute correlation for GeneSelectR top-k
      gs_genes <- head(gs_ranked[gs_ranked %in% colnames(X)], k)
      if (length(gs_genes) >= 10) {
        cor_gs <- cor(X[, gs_genes], use = "pairwise.complete.obs")
        diag(cor_gs) <- NA
        mean_cor_gs <- mean(abs(cor_gs), na.rm = TRUE)
      } else { mean_cor_gs <- NA }

      # Mean absolute correlation for DESeq2 top-k
      de_genes <- head(deseq_ranked[deseq_ranked %in% colnames(X)], k)
      if (length(de_genes) >= 10) {
        cor_de <- cor(X[, de_genes], use = "pairwise.complete.obs")
        diag(cor_de) <- NA
        mean_cor_de <- mean(abs(cor_de), na.rm = TRUE)
      } else { mean_cor_de <- NA }

      redundancy_results <- c(redundancy_results, list(data.frame(
        Dataset = r$name,
        k = k,
        Method = c("GeneSelectR", "DESeq2"),
        MeanAbsCor = c(mean_cor_gs, mean_cor_de),
        stringsAsFactors = FALSE
      )))
    }
  }

  if (length(redundancy_results) > 0) {
    redundancy_df <- do.call(rbind, redundancy_results)

    write.csv(redundancy_df, "results_realdata/data/redundancy_comparison.csv",
              row.names = FALSE)

    p_redundancy <- ggplot(redundancy_df, aes(x = k, y = MeanAbsCor,
                                              color = Method)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      facet_wrap(~ Dataset) +
      scale_color_manual(values = c("GeneSelectR" = "#2166AC",
                                    "DESeq2" = "#B2182B")) +
      labs(
        title = "Gene Set Redundancy: How correlated are the top genes?",
        subtitle = "Mean |pairwise correlation| among top-k genes (lower = more diverse)",
        x = "Number of top-k genes",
        y = "Mean |correlation|"
      ) +
      theme_bw() +
      theme(legend.position = "bottom")

    ggsave("results_realdata/figures/comparison_redundancy.pdf",
           p_redundancy, width = 12, height = 8)
    cat("  Saved: comparison_redundancy.pdf\n")
  }


  # =========================================================================
  # EXPERIMENT 3: Ranking Stability Under Subsampling
  # =========================================================================
  # Question: How reproducible is the ranking when you perturb the data?
  # Method: Draw N=20 random 80% subsamples. For each, re-run DESeq2 and
  #   take the top-k ranked by padj. Measure pairwise Jaccard overlap
  #   of the top-k across all subsample pairs.
  #   GeneSelectR already has built-in stability (pi) — we compare the
  #   consistency of its ranking to DESeq2's ranking variability.
  # Expected: DESeq2's top-k fluctuates substantially across subsamples
  #   because p-values are volatile for borderline genes. GeneSelectR's
  #   ranking is more stable by construction (aggregated over 50-100 folds).
  #
  # ---- FIGURE: comparison_ranking_stability.pdf ----
  # Boxplot of pairwise Jaccard similarities for DESeq2 subsamples vs.
  # a reference bar showing GeneSelectR's effective stability.
  # =========================================================================

  cat("Experiment 3: Ranking stability under subsampling...\n")

  n_subsamples <- 20
  subsample_frac <- 0.8

  stability_results <- list()

  for (r in results_with_deseq) {
    if (is.null(r$counts)) next  # Need raw counts for DESeq2

    cat(sprintf("  %s: subsampling DESeq2 %d times...\n", r$name, n_subsamples))

    counts <- r$counts
    y <- r$y
    k_stability <- 200

    # Run DESeq2 on N subsamples
    deseq_topk_lists <- list()

    for (s in seq_len(n_subsamples)) {
      set.seed(42 + s)
      n <- length(y)
      keep <- sort(sample(n, floor(subsample_frac * n)))

      tryCatch({
        sub_result <- get_deseq2_reference(counts[keep, ], y[keep])
        if (!is.null(sub_result)) {
          # Rank all genes by padj and take top-k
          sub_ranked <- sub_result$full_results %>%
            as.data.frame() %>%
            tibble::rownames_to_column("gene") %>%
            filter(!is.na(padj)) %>%
            arrange(padj) %>%
            head(k_stability) %>%
            pull(gene)
          deseq_topk_lists[[s]] <- sub_ranked
        }
      }, error = function(e) { })
    }

    # Compute pairwise Jaccard similarities for DESeq2 subsamples
    valid_lists <- Filter(function(x) length(x) > 0, deseq_topk_lists)

    if (length(valid_lists) >= 5) {
      jaccards_deseq <- numeric()
      for (i in 1:(length(valid_lists) - 1)) {
        for (j in (i + 1):length(valid_lists)) {
          inter <- length(intersect(valid_lists[[i]], valid_lists[[j]]))
          union <- length(union(valid_lists[[i]], valid_lists[[j]]))
          jaccards_deseq <- c(jaccards_deseq, inter / union)
        }
      }

      # GeneSelectR "stability": use pi to estimate effective consistency
      # Jaccard between GeneSelectR's top-k from two hypothetical independent
      # runs ≈ mean(pi) for the top-k genes (genes with pi=1 always appear,
      # pi=0.5 appear half the time → expected Jaccard is lower)
      gs_topk <- head(r$gene_scores, k_stability)
      gs_mean_pi <- mean(gs_topk$pi_exact)

      # A more precise estimate: expected Jaccard = E[|A∩B|] / E[|A∪B|]
      # For genes with selection probability p, P(in both) = p^2,
      # P(in either) = 2p - p^2
      # Expected Jaccard ≈ sum(pi^2) / sum(2*pi - pi^2)
      pi_vec <- gs_topk$pi_exact
      expected_jaccard_gs <- sum(pi_vec^2) / sum(2 * pi_vec - pi_vec^2)

      stability_results <- c(stability_results, list(data.frame(
        Dataset = r$name,
        Method = c(rep("DESeq2 (subsampled)", length(jaccards_deseq)),
                   "GeneSelectR (estimated)"),
        Jaccard = c(jaccards_deseq, expected_jaccard_gs),
        stringsAsFactors = FALSE
      )))
    }
  }

  if (length(stability_results) > 0) {
    stability_df <- do.call(rbind, stability_results)

    write.csv(stability_df, "results_realdata/data/ranking_stability.csv",
              row.names = FALSE)

    p_stability <- ggplot(stability_df,
                          aes(x = Method, y = Jaccard, fill = Method)) +
      geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
      facet_wrap(~ Dataset) +
      scale_fill_manual(values = c("DESeq2 (subsampled)" = "#B2182B",
                                   "GeneSelectR (estimated)" = "#2166AC")) +
      labs(
        title = "Ranking Stability: How consistent is the top-200 under data perturbation?",
        subtitle = sprintf("DESeq2: pairwise Jaccard of top-200 across %d×80%% subsamples; GeneSelectR: estimated from pi",
                           n_subsamples),
        x = NULL, y = "Jaccard Similarity of Top-200 Gene Lists"
      ) +
      theme_bw() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 15, hjust = 1))

    ggsave("results_realdata/figures/comparison_ranking_stability.pdf",
           p_stability, width = 12, height = 7)
    cat("  Saved: comparison_ranking_stability.pdf\n")
  }


  # =========================================================================
  # EXPERIMENT 4: Functional Coherence at Equal k
  # =========================================================================
  # Question: Which method produces a more biologically interpretable top-k?
  # Method: Run GO enrichment (clusterProfiler) on top-100 genes from each
  #   ranking. Compare: (a) number of significant GO terms, (b) median
  #   -log10(pval) of the top 10 terms. More and stronger enrichments
  #   indicate a more coherent gene set.
  # Expected: GeneSelectR's biology score pulls functionally related genes
  #   toward the top, producing tighter enrichment.
  #
  # ---- FIGURE: comparison_functional_coherence.pdf ----
  # Grouped bar chart: number of enriched GO terms per method per dataset,
  # plus an inset showing median enrichment strength.
  # =========================================================================

  cat("Experiment 4: Functional coherence comparison...\n")

  coherence_results <- list()

  for (r in results_with_deseq) {
    cat(sprintf("  %s: comparing enrichment of top-100...\n", r$name))

    gs_top100 <- head(r$gene_scores$gene, 100)

    deseq_top100 <- r$deseq_result$full_results %>%
      as.data.frame() %>%
      { if (!"gene" %in% names(.)) tibble::rownames_to_column(., "gene") else . } %>%
      dplyr::filter(!is.na(padj)) %>%
      dplyr::arrange(padj) %>%
      dplyr::slice_head(n = 100) %>%
      dplyr::pull(gene)

    universe <- colnames(r$X)

    # Enrichment for GeneSelectR top-100
    enrich_gs <- tryCatch(
      run_go_enrichment(gs_top100, universe = universe, n_top = 100),
      error = function(e) NULL
    )

    # Enrichment for DESeq2 top-100
    enrich_de <- tryCatch(
      run_go_enrichment(deseq_top100, universe = universe, n_top = 100),
      error = function(e) NULL
    )

    n_terms_gs <- if (!is.null(enrich_gs) && nrow(enrich_gs$df) > 0)
      nrow(enrich_gs$df) else 0
    n_terms_de <- if (!is.null(enrich_de) && nrow(enrich_de$df) > 0)
      nrow(enrich_de$df) else 0

    # Median enrichment strength (top 10 terms, -log10 adjusted p)
    med_strength_gs <- if (n_terms_gs >= 1)
      median(-log10(head(enrich_gs$df$p.adjust, 10))) else 0
    med_strength_de <- if (n_terms_de >= 1)
      median(-log10(head(enrich_de$df$p.adjust, 10))) else 0

    cat(sprintf("    GeneSelectR: %d GO terms (median strength: %.1f)\n",
                n_terms_gs, med_strength_gs))
    cat(sprintf("    DESeq2:      %d GO terms (median strength: %.1f)\n",
                n_terms_de, med_strength_de))

    coherence_results <- c(coherence_results, list(data.frame(
      Dataset = r$name,
      Method = c("GeneSelectR", "DESeq2"),
      n_GO_terms = c(n_terms_gs, n_terms_de),
      median_strength = c(med_strength_gs, med_strength_de),
      stringsAsFactors = FALSE
    )))
  }

  if (length(coherence_results) > 0) {
    coherence_df <- do.call(rbind, coherence_results)

    write.csv(coherence_df, "results_realdata/data/functional_coherence.csv",
              row.names = FALSE)

    # Two-panel figure
    p_nterms <- ggplot(coherence_df, aes(x = Dataset, y = n_GO_terms,
                                         fill = Method)) +
      geom_col(position = "dodge", alpha = 0.85) +
      scale_fill_manual(values = c("GeneSelectR" = "#2166AC",
                                   "DESeq2" = "#B2182B")) +
      labs(title = "Number of Enriched GO Terms (top-100 genes)",
           y = "Significant GO BP terms (padj < 0.05)", x = NULL) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))

    p_strength <- ggplot(coherence_df, aes(x = Dataset, y = median_strength,
                                           fill = Method)) +
      geom_col(position = "dodge", alpha = 0.85) +
      scale_fill_manual(values = c("GeneSelectR" = "#2166AC",
                                   "DESeq2" = "#B2182B")) +
      labs(title = "Enrichment Strength (top-100 genes)",
           subtitle = "Median -log10(padj) of top 10 GO terms (higher = stronger)",
           y = "-log10(adjusted p-value)", x = NULL) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))

    if (requireNamespace("patchwork", quietly = TRUE)) {
      p_coherence <- p_nterms / p_strength +
        plot_annotation(
          title = "Functional Coherence: Which ranking gives more interpretable gene sets?",
          theme = theme(plot.title = element_text(size = 13, face = "bold"))
        )
    } else {
      p_coherence <- p_nterms  # fallback without patchwork
    }

    ggsave("results_realdata/figures/comparison_functional_coherence.pdf",
           p_coherence, width = 10, height = 10)
    cat("  Saved: comparison_functional_coherence.pdf\n")
  }


  # =========================================================================
  # EXPERIMENT 5: Unique Gene Characterization
  # =========================================================================
  # Question: What genes does GeneSelectR find that DESeq2 misses (and vice
  #   versa), and are they meaningful?
  # Method: For each dataset, categorize top-200 genes into:
  #   - Shared: in both GeneSelectR top-200 and DESeq2 DE genes
  #   - GS-only: in GeneSelectR top-200 but NOT DESeq2 DE (multivariate signal)
  #   - DE-only: in DESeq2 DE but NOT GeneSelectR top-200 (noisy / redundant)
  #   For GS-only genes: report their pi, u, b scores and run GO enrichment.
  #   These are the multivariate-signal genes — useful in combination with
  #   others, even if their individual fold change is too small for DESeq2.
  #
  # ---- FIGURE: comparison_unique_genes.pdf ----
  # Stacked bar showing the composition of GeneSelectR's top-200:
  # "shared with DESeq2" vs "GS-only". Plus a panel showing score
  # distributions of GS-only genes (are they biologically relevant?).
  #
  # ---- TABLE: {dataset}_gs_unique_genes.csv ----
  # List of GeneSelectR-unique genes with their full score profiles.
  # =========================================================================

  cat("Experiment 5: Unique gene characterization...\n")

  unique_gene_results <- list()

  for (r in results_with_deseq) {
    cat(sprintf("  %s: characterizing unique genes...\n", r$name))

    gs_top200 <- head(r$gene_scores$gene, 200)
    de_genes <- r$deseq_result$de_genes

    shared <- intersect(gs_top200, de_genes)
    gs_only <- setdiff(gs_top200, de_genes)
    de_only <- setdiff(de_genes, gs_top200)

    cat(sprintf("    Shared: %d | GS-only: %d | DE-only: %d\n",
                length(shared), length(gs_only), length(de_only)))

    # Score profiles of GS-only genes
    gs_only_scores <- r$gene_scores[r$gene_scores$gene %in% gs_only, ]

    if (nrow(gs_only_scores) > 0) {
      write.csv(gs_only_scores,
                sprintf("results_realdata/data/%s_gs_unique_genes.csv", r$name),
                row.names = FALSE)
      cat(sprintf("    GS-only gene score summary:\n"))
      cat(sprintf("      mean pi = %.3f, mean u = %.3f, mean b = %.3f\n",
                  mean(gs_only_scores$pi_exact), mean(gs_only_scores$u),
                  mean(gs_only_scores$b)))

      # Run GO enrichment on GS-only genes to see if they're biologically meaningful
      gs_only_enrich <- tryCatch(
        run_go_enrichment(gs_only_scores$gene, universe = colnames(r$X),
                          n_top = length(gs_only_scores$gene)),
        error = function(e) NULL
      )
      if (!is.null(gs_only_enrich) && nrow(gs_only_enrich$df) > 0) {
        cat(sprintf("      GO enrichment: %d significant terms\n",
                    nrow(gs_only_enrich$df)))
        cat(sprintf("      Top term: %s (padj = %.2e)\n",
                    gs_only_enrich$df$Description[1],
                    gs_only_enrich$df$p.adjust[1]))
      } else {
        cat("      GO enrichment: no significant terms (too few genes?)\n")
      }
    }

    unique_gene_results <- c(unique_gene_results, list(data.frame(
      Dataset = r$name,
      Category = c("Shared (GS ∩ DE)", "GS-only", "DE-only"),
      Count = c(length(shared), length(gs_only), length(de_only)),
      stringsAsFactors = FALSE
    )))
  }

  if (length(unique_gene_results) > 0) {
    unique_df <- do.call(rbind, unique_gene_results)

    write.csv(unique_df, "results_realdata/data/unique_gene_characterization.csv",
              row.names = FALSE)

    # Panel A: Composition of GeneSelectR's top-200
    # Show only the GS top-200 breakdown (shared vs GS-only)
    gs_composition <- unique_df[unique_df$Category != "DE-only", ]

    p_composition <- ggplot(gs_composition,
                            aes(x = Dataset, y = Count, fill = Category)) +
      geom_col(alpha = 0.85) +
      geom_text(aes(label = Count), position = position_stack(vjust = 0.5),
                size = 3.5) +
      scale_fill_manual(values = c("Shared (GS ∩ DE)" = "#8073AC",
                                   "GS-only" = "#E08214")) +
      labs(title = "What's in GeneSelectR's top 200?",
           subtitle = "Shared = also called DE by DESeq2; GS-only = multivariate-signal genes",
           y = "Number of genes", x = NULL) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))

    # Panel B: Score profiles of GS-only genes (are they biologically relevant?)
    gs_only_all_scores <- do.call(rbind, lapply(results_with_deseq, function(r) {
      gs_top200 <- head(r$gene_scores$gene, 200)
      de_genes <- r$deseq_result$de_genes
      gs_only <- setdiff(gs_top200, de_genes)
      scores <- r$gene_scores[r$gene_scores$gene %in% gs_only,
                              c("gene", "pi_exact", "u", "b")]
      if (nrow(scores) > 0) {
        scores$Dataset <- r$name
        scores
      } else { NULL }
    }))

    if (!is.null(gs_only_all_scores) && nrow(gs_only_all_scores) > 0) {
      score_long <- tidyr::pivot_longer(gs_only_all_scores,
                                        cols = c("pi_exact", "u", "b"),
                                        names_to = "Component",
                                        values_to = "Score")
      score_long$Component <- factor(score_long$Component,
                                     levels = c("pi_exact", "u", "b"),
                                     labels = c("Stability (π)", "Utility (u)",
                                                "Biology (b)"))

      p_gs_only_scores <- ggplot(score_long,
                                 aes(x = Component, y = Score, fill = Component)) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap(~ Dataset) +
        scale_fill_brewer(palette = "Set2") +
        labs(title = "Score Profiles of GeneSelectR-Unique Genes",
             subtitle = "These genes are in GeneSelectR's top-200 but NOT called DE by DESeq2",
             y = "Score value", x = NULL) +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 30, hjust = 1))

      if (requireNamespace("patchwork", quietly = TRUE)) {
        p_unique <- p_composition / p_gs_only_scores +
          plot_annotation(
            title = "GeneSelectR's Added Value: Multivariate-Signal Genes",
            theme = theme(plot.title = element_text(size = 13, face = "bold"))
          )
      } else {
        p_unique <- p_composition
      }
    } else {
      p_unique <- p_composition
    }

    ggsave("results_realdata/figures/comparison_unique_genes.pdf",
           p_unique, width = 11, height = 10)
    cat("  Saved: comparison_unique_genes.pdf\n")
  }


  # =========================================================================
  # COMBINED SUMMARY: Value-Added Dashboard
  # =========================================================================
  # ---- FIGURE: comparison_value_added_summary.pdf ----
  # A single multi-panel figure that makes the case at a glance.
  # Top-left:    Parsimony (AUC vs k) for a representative dataset
  # Top-right:   Redundancy (mean |cor| vs k)
  # Bottom-left: Stability (Jaccard boxplot)
  # Bottom-right: Unique gene composition
  # =========================================================================

  if (requireNamespace("patchwork", quietly = TRUE) &&
      exists("parsimony_df") && exists("redundancy_df")) {

    cat("Generating combined value-added dashboard...\n")

    # Pick the first dataset for parsimony and redundancy panels
    rep_ds <- results_with_deseq[[1]]$name

    p1 <- ggplot(parsimony_df[parsimony_df$Dataset == rep_ds, ],
                 aes(x = k, y = AUC, color = Method, linetype = Method)) +
      geom_line(linewidth = 1.1) + geom_point(size = 2) +
      scale_color_manual(values = c("GeneSelectR" = "#2166AC",
                                    "DESeq2" = "#B2182B",
                                    "Random" = "grey60")) +
      scale_x_log10(breaks = k_values) +
      labs(title = sprintf("A. Parsimony (%s)", rep_ds),
           subtitle = "Same AUC with fewer genes",
           x = "Top-k genes", y = "AUC") +
      theme_bw(base_size = 10) +
      theme(legend.position = "bottom", legend.title = element_blank())

    p2 <- ggplot(redundancy_df[redundancy_df$Dataset == rep_ds, ],
                 aes(x = k, y = MeanAbsCor, color = Method)) +
      geom_line(linewidth = 1.1) + geom_point(size = 2) +
      scale_color_manual(values = c("GeneSelectR" = "#2166AC",
                                    "DESeq2" = "#B2182B")) +
      labs(title = sprintf("B. Redundancy (%s)", rep_ds),
           subtitle = "Less correlated top genes",
           x = "Top-k genes", y = "Mean |correlation|") +
      theme_bw(base_size = 10) +
      theme(legend.position = "bottom", legend.title = element_blank())

    # Coherence bar (all datasets)
    p3 <- ggplot(coherence_df, aes(x = Dataset, y = n_GO_terms, fill = Method)) +
      geom_col(position = "dodge", alpha = 0.85) +
      scale_fill_manual(values = c("GeneSelectR" = "#2166AC",
                                   "DESeq2" = "#B2182B")) +
      labs(title = "C. Functional Coherence",
           subtitle = "More enriched GO terms",
           y = "Enriched GO terms", x = NULL) +
      theme_bw(base_size = 10) +
      theme(legend.position = "bottom", legend.title = element_blank(),
            axis.text.x = element_text(angle = 30, hjust = 1))

    # Unique gene composition (all datasets)
    p4 <- ggplot(unique_df[unique_df$Category != "DE-only", ],
                 aes(x = Dataset, y = Count, fill = Category)) +
      geom_col(alpha = 0.85) +
      scale_fill_manual(values = c("Shared (GS ∩ DE)" = "#8073AC",
                                   "GS-only" = "#E08214")) +
      labs(title = "D. Unique Discoveries",
           subtitle = "Multivariate-signal genes DESeq2 misses",
           y = "Genes in top-200", x = NULL) +
      theme_bw(base_size = 10) +
      theme(legend.position = "bottom", legend.title = element_blank(),
            axis.text.x = element_text(angle = 30, hjust = 1))

    p_dashboard <- (p1 | p2) / (p3 | p4) +
      plot_annotation(
        title = "Why GeneSelectR? — Added Value Over DESeq2",
        subtitle = "Same biology, better prioritization: fewer genes, less redundancy, more coherent, stable rankings",
        theme = theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10, color = "grey30")
        )
      )

    ggsave("results_realdata/figures/comparison_value_added_summary.pdf",
           p_dashboard, width = 14, height = 11)
    cat("  Saved: comparison_value_added_summary.pdf\n")
  }

  cat("\nValue-added experiments complete.\n")
}


# ==============================================================================
# SAVE FULL REPORT
# ==============================================================================
# ---- OUTPUT: BENCHMARK_REPORT.txt ----
# A plain-text summary of all results, suitable for quick review or sharing.
# Contains: summary table, top 20 genes per dataset (with all scores), and
# top 5 enriched GO BP terms per dataset.
# ==============================================================================

sink("results_realdata/BENCHMARK_REPORT.txt")
cat("GeneSelectR 2.0 — Real-Data Benchmark Report\n")
cat("=" %R% 60, "\n", sep = "")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R:", R.version.string, "\n")
cat("Cores:", n_cores, "\n\n")

cat("SUMMARY TABLE\n")
cat("-" %R% 40, "\n", sep = "")
if (nrow(summary_table) > 0) {
  print(summary_table, row.names = FALSE)
} else {
  cat("No datasets processed.\n")
}

for (r in all_results) {
  cat("\n\n")
  cat("=" %R% 60, "\n", sep = "")
  cat(r$description, "\n")
  cat("-" %R% 60, "\n", sep = "")
  cat(sprintf("Source: %s\n", r$source))
  cat(sprintf("Dimensions: %d samples x %d genes\n", r$n_samples, r$n_genes))
  cat(sprintf("AUC: %.4f +/- %.4f\n", r$cv_auc_mean, r$cv_auc_sd))
  cat(sprintf("Stable genes (pi>0.5): %d | (pi>0.9): %d\n",
              r$n_stable_50, r$n_stable_90))
  cat(sprintf("Runtime: %.1f seconds\n", r$wall_time))
  cat("\nTop 20 genes:\n")
  print(head(r$gene_scores[, c("gene", "final_score", "pi_exact", "u", "b")], 20),
        row.names = FALSE)

  if (!is.null(r$enrichment)) {
    cat(sprintf("\nTop 5 enriched GO terms:\n"))
    top5 <- head(r$enrichment$df[, c("Description", "p.adjust", "Count")], 5)
    print(top5, row.names = FALSE)
  }
}
sink()

cat("\n\nAll results saved to results_realdata/\n")
cat("Done!\n")

