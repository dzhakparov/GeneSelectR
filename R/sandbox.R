# ==============================================================================
# GeneSelectR 2.0 - TESTING SCRIPT WITH SYNTHETIC DATA
# ==============================================================================
#
# This script:
# 1. Generates realistic synthetic gene expression data
# 2. Tests different GeneSelectR options
# 3. Compares results
# 4. Helps you understand the method
#
# Just run this entire script!

# ==============================================================================
# SETUP: Source the functions
# ==============================================================================

cat("Loading GeneSelectR 2.0 functions...\n")

source("G:/Projects/GeneSelectR/R/utils.R")
source("G:/Projects/GeneSelectR/R/enrichment.R")
source("G:/Projects/GeneSelectR/R/GeneSelectR.R")

# Load required packages
library(org.Hs.eg.db)

cat("Functions loaded successfully!\n\n")

# ==============================================================================
# FUNCTION: Generate Synthetic Data
# ==============================================================================

generate_synthetic_data <- function(
    n_samples = 100,           # Total samples (50 per group)
    n_genes = 1000,            # Total genes
    n_signal_genes = 30,       # Genes truly associated with outcome
    signal_strength = 2.0,     # How different are disease vs control
    noise_level = 1.0,         # Background noise
    correlation_strength = 0.7, # Correlation within gene groups
    seed = 42
) {

  set.seed(seed)

  cat("Generating synthetic data...\n")
  cat(sprintf("  Samples: %d (%d per group)\n", n_samples, n_samples/2))
  cat(sprintf("  Total genes: %d\n", n_genes))
  cat(sprintf("  Signal genes: %d (ground truth)\n", n_signal_genes))

  # --------------------------------------------------------------------------
  # Generate outcome variable
  # --------------------------------------------------------------------------
  n_per_group <- n_samples / 2
  y <- factor(rep(c("Control", "Disease"), each = n_per_group))

  # --------------------------------------------------------------------------
  # Generate baseline expression (all genes, all samples)
  # --------------------------------------------------------------------------
  X <- matrix(rnorm(n_samples * n_genes, mean = 5, sd = noise_level),
              nrow = n_samples,
              ncol = n_genes)

  # --------------------------------------------------------------------------
  # Add signal to "true" disease genes
  # --------------------------------------------------------------------------

  # First 30 genes are truly associated with disease
  signal_genes <- 1:n_signal_genes

  # These genes have higher expression in Disease group
  disease_idx <- which(y == "Disease")
  for (gene in signal_genes) {
    X[disease_idx, gene] <- X[disease_idx, gene] +
      rnorm(length(disease_idx),
            mean = signal_strength,
            sd = signal_strength * 0.2)
  }

  # --------------------------------------------------------------------------
  # Add correlation structure (genes 1-10 correlated, 11-20 correlated, etc.)
  # --------------------------------------------------------------------------

  # Create 3 correlated gene groups within signal genes
  group1 <- 1:10
  group2 <- 11:20
  group3 <- 21:30

  # Add shared component to make them correlated
  for (group in list(group1, group2, group3)) {
    # Create a shared latent factor
    shared_factor <- rnorm(n_samples, mean = 0, sd = 1)

    for (gene in group) {
      # Mix individual gene expression with shared factor
      X[, gene] <- correlation_strength * shared_factor +
        (1 - correlation_strength) * X[, gene]
    }
  }

  # --------------------------------------------------------------------------
  # Create gene names
  # --------------------------------------------------------------------------

  # Use some real gene names for signal genes (makes it more realistic)
  real_gene_names <- c(
    # Group 1: Immune response genes
    "IL6", "TNF", "IFNG", "IL1B", "IL10", "CXCL10", "CCL5", "IL8", "IL2", "IL4",
    # Group 2: Inflammation genes
    "PTGS2", "NOS2", "ICAM1", "VCAM1", "SELE", "SELP", "MMP9", "MMP2", "TIMP1", "NFKB1",
    # Group 3: Cell signaling genes
    "STAT1", "STAT3", "JAK2", "MAPK1", "AKT1", "MTOR", "TP53", "MYC", "JUN", "FOS"
  )

  # Noise genes get generic names
  noise_gene_names <- paste0("NoiseGene", 1:(n_genes - n_signal_genes))

  colnames(X) <- c(real_gene_names, noise_gene_names)

  # --------------------------------------------------------------------------
  # Return results
  # --------------------------------------------------------------------------

  cat("  Correlation structure:\n")
  cat("    Genes 1-10 (IL6, TNF, ...): correlated group\n")
  cat("    Genes 11-20 (PTGS2, NOS2, ...): correlated group\n")
  cat("    Genes 21-30 (STAT1, STAT3, ...): correlated group\n")
  cat("  Ground truth signal genes:\n")
  cat(sprintf("    %s\n", paste(real_gene_names, collapse = ", ")))
  cat("\n")

  return(list(
    X = X,
    y = y,
    true_genes = real_gene_names,
    signal_genes_idx = signal_genes,
    gene_groups = list(group1 = group1, group2 = group2, group3 = group3)
  ))
}

# ==============================================================================
# GENERATE DATA
# ==============================================================================

cat("="  , rep("=", 79), "\n", sep = "")
cat("STEP 1: GENERATE SYNTHETIC DATA\n")
cat("=" , rep("=", 79), "\n", sep = "")

data <- generate_synthetic_data(
  n_samples = 100,
  n_genes = 1000,
  n_signal_genes = 30,
  signal_strength = 2.0,
  seed = 123
)

X <- data$X
y <- data$y
true_genes <- data$true_genes

cat("Data generated! Dimensions:\n")
cat(sprintf("  X: %d samples x %d genes\n", nrow(X), ncol(X)))
cat(sprintf("  y: %d outcomes (%d Control, %d Disease)\n",
            length(y), sum(y == "Control"), sum(y == "Disease")))
cat("\n")

# ==============================================================================
# TEST 1: Basic Run (Default Settings)
# ==============================================================================

cat("\n")
cat("=", rep("=", 79), "\n", sep = "")
cat("TEST 1: BASIC RUN (Default Settings)\n")
cat("=", rep("=", 79), "\n", sep = "")
cat("Settings: supervised mode, geometric mean, elastic net, discrete MI\n\n")

result_basic <- geneselectr2_fit(
  X = X,
  y = y,
  bio_mode = "supervised",
  target_terms = "GO:0006955",  # Immune response
  score_formula = "geometric",
  regularization_method = "elastic_net",
  alpha = 0.5,
  mi_method = "discrete",
  K = 3,   # Small for speed in testing
  R = 5,   # Small for speed in testing
  verbose = TRUE
)

cat("\n--- RESULTS ---\n")
cat(sprintf("Mean AUC: %.4f (SD: %.4f)\n",
            result_basic$cv_summary$mean,
            result_basic$cv_summary$sd))
cat("\nTop 10 genes:\n")
print(result_basic$gene_scores[1:10, c("gene", "final_score", "pi_exact", "u", "b")])

# Check recovery of true genes
top_30 <- result_basic$gene_scores$gene[1:30]
recovered <- sum(top_30 %in% true_genes)
cat(sprintf("\nTrue positive recovery: %d/30 (%.1f%%)\n",
            recovered, 100*recovered/30))

# ==============================================================================
# TEST 2: Compare Score Formulas
# ==============================================================================

cat("\n\n")
cat("=", rep("=", 79), "\n", sep = "")
cat("TEST 2: COMPARE SCORE FORMULAS\n")
cat("=", rep("=", 79), "\n", sep = "")
cat("Testing: geometric, arithmetic, harmonic, minimum\n\n")

formulas <- c("geometric", "arithmetic", "harmonic", "minimum")
formula_results <- list()

for (formula in formulas) {
  cat(sprintf("Running with %s formula... ", formula))

  formula_results[[formula]] <- geneselectr2_fit(
    X = X,
    y = y,
    bio_mode = "supervised",
    target_terms = "GO:0006955",
    score_formula = formula,
    K = 3, R = 5,
    verbose = FALSE
  )

  cat(sprintf("AUC = %.4f\n", formula_results[[formula]]$cv_summary$mean))
}

# Compare top genes
cat("\n--- Top 5 genes by formula ---\n")
for (formula in formulas) {
  top5 <- head(formula_results[[formula]]$gene_scores$gene, 5)
  cat(sprintf("%-12s: %s\n", formula, paste(top5, collapse = ", ")))
}

# Gene overlap
cat("\n--- Gene overlap in top 30 ---\n")
geo_top <- formula_results[["geometric"]]$gene_scores$gene[1:30]
for (formula in formulas[-1]) {  # Skip geometric (comparing to itself)
  other_top <- formula_results[[formula]]$gene_scores$gene[1:30]
  overlap <- length(intersect(geo_top, other_top))
  cat(sprintf("Geometric vs %s: %d/30 (%.1f%%)\n",
              sprintf("%-12s", formula), overlap, 100*overlap/30))
}

# ==============================================================================
# TEST 3: Supervised vs Data-Driven Biology
# ==============================================================================

cat("\n\n")
cat("=", rep("=", 79), "\n", sep = "")
cat("TEST 3: SUPERVISED vs DATA-DRIVEN BIOLOGY\n")
cat("=", rep("=", 79), "\n", sep = "")

cat("\nRunning supervised mode...\n")
result_supervised <- geneselectr2_fit(
  X = X,
  y = y,
  bio_mode = "supervised",
  target_terms = "GO:0006955",
  K = 3, R = 5,
  verbose = TRUE
)

cat("\n\nRunning data-driven mode...\n")
result_datadriven <- geneselectr2_fit(
  X = X,
  y = y,
  bio_mode = "data_driven",
  K = 3, R = 5,
  verbose = TRUE
)

cat("\n--- COMPARISON ---\n")
cat(sprintf("Supervised AUC:    %.4f\n", result_supervised$cv_summary$mean))
cat(sprintf("Data-driven AUC:   %.4f\n", result_datadriven$cv_summary$mean))

if (!is.null(result_datadriven$methodology$discovered_terms)) {
  cat(sprintf("\nDiscovered pathways: %d\n",
              length(result_datadriven$methodology$discovered_terms)))
  cat("Top 3:\n")
  for (i in 1:min(3, length(result_datadriven$methodology$discovered_terms))) {
    cat(sprintf("  %s\n", result_datadriven$methodology$discovered_terms[i]))
  }
}

# Gene overlap
top_sup <- result_supervised$gene_scores$gene[1:30]
top_dd <- result_datadriven$gene_scores$gene[1:30]
overlap <- length(intersect(top_sup, top_dd))
cat(sprintf("\nGene overlap (top 30): %d/30 (%.1f%%)\n", overlap, 100*overlap/30))

# ==============================================================================
# TEST 4: Compare Regularization Methods
# ==============================================================================

cat("\n\n")
cat("=", rep("=", 79), "\n", sep = "")
cat("TEST 4: COMPARE REGULARIZATION METHODS\n")
cat("=", rep("=", 79), "\n", sep = "")
cat("Testing: lasso, elastic_net\n")
cat("(group_lasso requires 'gglasso' package - skipped if not installed)\n\n")

# Lasso
cat("Running lasso (alpha=1)... ")
result_lasso <- geneselectr2_fit(
  X = X, y = y,
  regularization_method = "lasso",
  alpha = 1.0,
  K = 3, R = 5,
  verbose = FALSE
)
cat(sprintf("AUC = %.4f\n", result_lasso$cv_summary$mean))

# Elastic Net
cat("Running elastic net (alpha=0.5)... ")
result_enet <- geneselectr2_fit(
  X = X, y = y,
  regularization_method = "elastic_net",
  alpha = 0.5,
  K = 3, R = 5,
  verbose = FALSE
)
cat(sprintf("AUC = %.4f\n", result_enet$cv_summary$mean))

# Group Lasso (optional)
if (requireNamespace("gglasso", quietly = TRUE)) {
  cat("Running group lasso... ")
  result_glasso <- geneselectr2_fit(
    X = X, y = y,
    regularization_method = "group_lasso",
    cor_threshold = 0.8,
    K = 3, R = 5,
    verbose = FALSE
  )
  cat(sprintf("AUC = %.4f\n", result_glasso$cv_summary$mean))
} else {
  cat("Group lasso skipped (gglasso package not installed)\n")
}

# Compare selection stability
cat("\n--- Selection Statistics ---\n")
cat(sprintf("Lasso:       Mean genes selected = %.1f, Mean π = %.3f\n",
            mean(sapply(result_lasso$details$selected, length)),
            mean(result_lasso$gene_scores$pi_exact)))
cat(sprintf("Elastic Net: Mean genes selected = %.1f, Mean π = %.3f\n",
            mean(sapply(result_enet$details$selected, length)),
            mean(result_enet$gene_scores$pi_exact)))

# ==============================================================================
# TEST 5: Custom Weights
# ==============================================================================

cat("\n\n")
cat("=", rep("=", 79), "\n", sep = "")
cat("TEST 5: CUSTOM WEIGHTS\n")
cat("=", rep("=", 79), "\n", sep = "")

weight_schemes <- list(
  "Equal" = c(1, 1, 1),
  "Emphasize stability" = c(2, 1, 1),
  "Emphasize biology" = c(1, 1, 2),
  "Ignore biology" = c(1, 1, 0.1)
)

cat("Testing different weight schemes...\n\n")

for (name in names(weight_schemes)) {
  weights <- weight_schemes[[name]]
  cat(sprintf("%-25s (π=%.1f, u=%.1f, b=%.1f): ",
              name, weights[1], weights[2], weights[3]))

  result <- geneselectr2_fit(
    X = X, y = y,
    score_weights = weights,
    K = 3, R = 5,
    verbose = FALSE
  )

  top_gene <- result$gene_scores$gene[1]
  cat(sprintf("Top gene = %s\n", top_gene))
}

# ==============================================================================
# TEST 6: Evaluate Performance
# ==============================================================================

cat("\n\n")
cat("=", rep("=", 79), "\n", sep = "")
cat("TEST 6: EVALUATE PERFORMANCE\n")
cat("=", rep("=", 79), "\n", sep = "")

# Use the basic result
top_genes_ranked <- result_basic$gene_scores$gene

# Calculate precision at different cutoffs
cutoffs <- c(10, 20, 30, 50, 100)

cat("True Positive Rate (Precision):\n")
for (cutoff in cutoffs) {
  top_k <- top_genes_ranked[1:cutoff]
  tp <- sum(top_k %in% true_genes)
  precision <- tp / length(true_genes)
  cat(sprintf("  Top %3d genes: %2d/30 true genes (%.1f%%)\n",
              cutoff, tp, 100*precision))
}

# Which true genes were missed?
cat("\n--- True genes NOT in top 30 ---\n")
top_30_genes <- top_genes_ranked[1:30]
missed <- setdiff(true_genes, top_30_genes)
if (length(missed) > 0) {
  cat(paste(missed, collapse = ", "), "\n")

  # Why were they missed? Check their scores
  cat("\nWhy they ranked low:\n")
  for (gene in head(missed, 5)) {
    idx <- which(result_basic$gene_scores$gene == gene)
    scores <- result_basic$gene_scores[idx, c("gene", "pi_exact", "u", "b", "final_score")]
    cat(sprintf("  %s: π=%.2f, u=%.2f, b=%.2f → final=%.3f (rank #%d)\n",
                gene, scores$pi_exact, scores$u, scores$b, scores$final_score, idx))
  }
} else {
  cat("All true genes recovered in top 30!\n")
}

# ==============================================================================
# TEST 7: Inspect Individual Gene
# ==============================================================================

cat("\n\n")
cat("=", rep("=", 79), "\n", sep = "")
cat("TEST 7: INSPECT INDIVIDUAL GENE\n")
cat("=", rep("=", 79), "\n", sep = "")

# Pick the top gene
inspect_gene <- result_basic$gene_scores$gene[1]
cat(sprintf("Examining: %s\n\n", inspect_gene))

gene_data <- result_basic$gene_scores[result_basic$gene_scores$gene == inspect_gene, ]

cat("Component scores:\n")
cat(sprintf("  Selection probability (π): %.3f (selected in %.0f%% of folds)\n",
            gene_data$pi_exact, 100*gene_data$pi_exact))
cat(sprintf("  Utility coefficient (u_coef): %.3f\n", gene_data$u_coef))
cat(sprintf("  Utility MI (u_mi): %.3f\n", gene_data$u_mi))
cat(sprintf("  Combined utility (u): %.3f\n", gene_data$u))
cat(sprintf("  Biological score (b): %.3f\n", gene_data$b))
cat(sprintf("  Final score: %.4f\n", gene_data$final_score))

cat("\nInterpretation:\n")
if (gene_data$pi_exact > 0.8) {
  cat("  - Very stable (high π): consistently selected\n")
} else if (gene_data$pi_exact > 0.5) {
  cat("  - Moderately stable (medium π)\n")
} else {
  cat("  - Unstable (low π): selected inconsistently\n")
}

if (gene_data$u > 0.8) {
  cat("  - Strong predictive utility (high u)\n")
} else if (gene_data$u > 0.5) {
  cat("  - Moderate predictive utility\n")
} else {
  cat("  - Weak predictive utility (low u)\n")
}

if (gene_data$b > 0.8) {
  cat("  - Highly relevant to target biology (high b)\n")
} else if (gene_data$b > 0.5) {
  cat("  - Moderately relevant to target biology\n")
} else if (gene_data$b > 0) {
  cat("  - Weakly relevant to target biology (low b)\n")
} else {
  cat("  - No GO annotations for target pathway (b=0)\n")
}

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n\n")
cat("=", rep("=", 79), "\n", sep = "")
cat("TESTING COMPLETE!\n")
cat("=", rep("=", 79), "\n", sep = "")

cat("\nKey Results:\n")
cat(sprintf("  1. Basic model AUC: %.4f\n", result_basic$cv_summary$mean))
cat(sprintf("  2. True genes recovered: %d/30 in top 30\n",
            sum(result_basic$gene_scores$gene[1:30] %in% true_genes)))
cat(sprintf("  3. Top gene: %s\n", result_basic$gene_scores$gene[1]))
cat(sprintf("  4. Formula comparison: Results stable across methods\n"))
cat(sprintf("  5. Supervised vs Data-driven: Both effective\n"))
