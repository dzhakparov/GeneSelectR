#' Fit GeneSelectR 2.0
#'
#' Integrates selection stability (pi), predictive utility (u), and biological
#' relevance (b) to rank genes. Uses repeated cross-validation with regularized
#' regression and optional GO enrichment.
#'
#' @param X Numeric matrix of expression values (samples x genes)
#' @param y Factor with two levels indicating binary outcome
#' @param gene_names Character vector of gene symbols (optional, uses colnames(X) if NULL)
#' @param bio_mode Character, one of "supervised", "data_driven", or "none"
#' @param target_terms Character vector of GO term IDs for supervised mode
#' @param bio_ontology Character vector of GO ontologies to use for biology scoring.
#'   Any subset of c("BP", "MF", "CC"). Default: "BP" (Biological Process only).
#'   Use c("BP", "MF", "CC") for all ontologies.
#' @param bio_sim_method Semantic similarity metric for biology scoring. One of:
#'   "resnik" (default), "lin", "jiang", "rel" (Schlicker relevance).
#' @param bio_enrich_fdr FDR threshold for data-driven enrichment. Default: 0.05.
#' @param bio_min_term_freq Minimum annotation frequency for candidate terms.
#'   NULL = auto-calculate. Default: NULL.
#' @param bio_max_enriched Maximum enriched terms for scoring. Default: 100.
#' @param bio_n_top_sims Top-k similarities to average per gene. Default: 5.
#' @param bio_ic_quantile IC specificity filter quantile. Default: 0.5.
#' @param score_formula Character, one of "geometric", "arithmetic", "harmonic", "minimum"
#' @param score_weights Numeric vector of length 3, weights for (pi, u, b)
#' @param regularization_method Character, "lasso", "elastic_net", or "group_lasso"
#' @param alpha Numeric, elastic net mixing parameter (1=lasso, 0=ridge)
#' @param gene_groups Named vector of group assignments for group lasso (optional)
#' @param group_penalty Penalty type for group lasso (default: "grLasso")
#' @param mi_method Character, "discrete" or "continuous"
#' @param mi_bins Integer, number of bins for discrete MI
#' @param K Integer, number of cross-validation folds (default: 5)
#' @param R Integer, number of CV repeats (default: 20)
#' @param use_cache Logical, whether to use GO annotation caching (default: TRUE)
#' @param n_cores Integer, number of CPU cores for parallel processing (default: 1)
#' @param random_seed Integer, random seed for reproducibility
#' @param verbose Logical, print progress messages (default: TRUE)
#'
#' @return List with components:
#' \describe{
#'   \item{gene_scores}{Data frame of genes ranked by final score}
#'   \item{cv_results}{List of cross-validation metrics}
#'   \item{cv_summary}{Summary statistics of CV performance}
#'   \item{parameters}{List of input parameters used}
#'   \item{timing}{Timing information for each step}
#' }
#'
#' @examples
#' \dontrun{
#' # Default: BP ontology, Resnik similarity
#' result <- geneselectr2_fit(X, y, bio_mode = "data_driven")
#'
#' # Custom: all ontologies, Lin similarity, strict enrichment
#' result <- geneselectr2_fit(X, y,
#'   bio_mode = "data_driven",
#'   bio_ontology = c("BP", "MF", "CC"),
#'   bio_sim_method = "lin",
#'   bio_enrich_fdr = 0.01,
#'   bio_ic_quantile = 0.7
#' )
#'
#' # Supervised with specific GO terms
#' result <- geneselectr2_fit(X, y,
#'   bio_mode = "supervised",
#'   target_terms = c("GO:0006955", "GO:0006954"),
#'   bio_sim_method = "rel"
#' )
#' }
#'
#' @export
#'
#' @importFrom parallel makeCluster stopCluster parLapply clusterExport
#' @importFrom stats glm predict binomial
geneselectr2_fit <- function(
    X,
    y,
    gene_names = NULL,
    bio_mode = "supervised",
    target_terms = NULL,
    bio_ontology = "BP",
    bio_sim_method = "resnik",
    bio_enrich_fdr = 0.05,
    bio_min_term_freq = NULL,
    bio_max_enriched = 100,
    bio_n_top_sims = 5,
    bio_ic_quantile = 0.5,
    score_formula = "geometric",
    score_weights = c(1, 1, 1),
    regularization_method = "elastic_net",
    alpha = 0.5,
    gene_groups = NULL,
    group_penalty = "grLasso",
    mi_method = "discrete",
    mi_bins = 5,
    K = 5,
    R = 20,
    use_cache = TRUE,
    n_cores = 1,
    random_seed = 123,
    verbose = TRUE
) {

  start_time <- Sys.time()

  # Force evaluation of all arguments that will be sent to parallel workers.
  force(X)
  force(y)
  force(regularization_method)
  force(alpha)
  force(gene_groups)
  force(group_penalty)
  force(mi_method)
  force(mi_bins)
  force(K)
  force(R)
  force(n_cores)
  force(verbose)
  force(bio_ontology)
  force(bio_sim_method)
  force(bio_enrich_fdr)
  force(bio_min_term_freq)
  force(bio_max_enriched)
  force(bio_n_top_sims)
  force(bio_ic_quantile)

  if (verbose) {
    cat("\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat("GeneSelectR 2.0\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
  }

  set_cache_options(verbose = verbose)

  # Gene names
  if (is.null(gene_names)) {
    gene_names <- colnames(X)
  }
  colnames(X) <- gene_names

  # Validate inputs
  validate_inputs(X, y, gene_names, K)

  n_samples <- nrow(X)
  n_genes <- ncol(X)

  if (verbose) {
    cat(sprintf("Samples: %d | Genes: %d\n", n_samples, n_genes))
    cat(sprintf("Outcome: %s vs %s\n", levels(y)[1], levels(y)[2]))
    cat(sprintf("CV: %d-fold x %d repeats = %d total folds\n", K, R, K*R))
    cat(sprintf("Regularization: %s (alpha=%.2f)\n", regularization_method, alpha))
    cat(sprintf("Biology mode: %s\n", bio_mode))
    if (bio_mode != "none") {
      cat(sprintf("  Ontology: %s | Similarity: %s\n",
                  paste(bio_ontology, collapse = "+"), bio_sim_method))
      if (bio_mode == "data_driven") {
        cat(sprintf("  Enrichment FDR: %.3f | IC quantile: %.2f\n",
                    bio_enrich_fdr, bio_ic_quantile))
        cat(sprintf("  Max enriched terms: %d | Top-k sims: %d\n",
                    bio_max_enriched, bio_n_top_sims))
      }
    }
    if (use_cache) cat("Caching: ENABLED\n")
    if (n_cores > 1) cat(sprintf("Parallel: %d cores\n", n_cores))
    cat("\n")
  }

  # --- Auto-generate gene groups for group lasso if not provided ---
  if (regularization_method == "group_lasso" && is.null(gene_groups)) {
    if (verbose) cat("Generating gene groups via correlation clustering...\n")
    gene_groups <- auto_generate_groups(X)
    if (verbose) {
      cat(sprintf("  Created %d groups (mean %.1f genes/group, range %d-%d)\n",
                  length(unique(gene_groups)),
                  n_genes / length(unique(gene_groups)),
                  min(table(gene_groups)),
                  max(table(gene_groups))))
    }
  }

  # ============================================================================
  # STEP 1: Cross-Validation (now includes MI computation per fold)
  # ============================================================================

  if (verbose) cat("Step 1/4: Running cross-validation (with per-fold MI)...\n")

  cv_folds <- create_cv_folds(y, K, R, random_seed)
  n_folds <- length(cv_folds)

  # Run CV (with optional parallelization)
  if (n_cores > 1) {
    if (verbose) cat(sprintf("  Using %d cores for parallel processing\n", n_cores))

    cl <- parallel::makeCluster(n_cores)

    parallel::clusterEvalQ(cl, {
      requireNamespace("glmnet", quietly = TRUE)
      requireNamespace("pROC", quietly = TRUE)
      if (requireNamespace("grpreg", quietly = TRUE)) TRUE
    })

    parallel::clusterExport(cl, c(
      "run_cv_fold", "fit_regularized_model",
      "compute_auc", "compute_mi_vectorized",
      "compute_mutual_information",
      "discretize_continuous", "adaptive_bin_count",
      "auto_generate_groups",
      "X", "y", "regularization_method", "alpha",
      "mi_method", "mi_bins", "gene_groups", "group_penalty"
    ), envir = environment())

    cv_results <- parallel::parLapply(cl, cv_folds, function(fold) {
      run_cv_fold(fold, X, y, regularization_method, alpha,
                  mi_method = mi_method, mi_bins = mi_bins,
                  gene_groups = gene_groups, group_penalty = group_penalty)
    })

    parallel::stopCluster(cl)

  } else {
    cv_results <- lapply(cv_folds, function(fold) {
      run_cv_fold(fold, X, y, regularization_method, alpha,
                  mi_method = mi_method, mi_bins = mi_bins,
                  gene_groups = gene_groups, group_penalty = group_penalty,
                  show_progress = verbose && fold$fold == 1)
    })
  }

  cv_auc <- sapply(cv_results, function(x) x$auc)

  if (verbose) {
    cat(sprintf("  Mean AUC: %.4f +/- %.4f\n", mean(cv_auc, na.rm = TRUE),
                sd(cv_auc, na.rm = TRUE)))
  }

  # ============================================================================
  # STEP 2: Aggregate CV Results (now includes MI aggregation)
  # ============================================================================

  if (verbose) cat("\nStep 2/4: Aggregating CV results...\n")

  agg_start <- Sys.time()

  agg_results <- aggregate_cv_results(cv_results, n_genes)

  pi_exact <- agg_results$pi_exact
  u_coef   <- agg_results$u_coef
  u_mi     <- agg_results$u_mi

  agg_time <- as.numeric(difftime(Sys.time(), agg_start, units = "secs"))

  if (verbose) {
    cat(sprintf("  Aggregation completed in %.2f seconds\n", agg_time))
    cat(sprintf("  Genes selected in at least 1 fold: %d (%.1f%%)\n",
                sum(pi_exact > 0), 100 * sum(pi_exact > 0) / n_genes))
    cat(sprintf("  Genes with pi > 0.5: %d (%.1f%%)\n",
                sum(pi_exact > 0.5), 100 * sum(pi_exact > 0.5) / n_genes))
    cat(sprintf("  Mean MI (aggregated from folds): %.4f\n", mean(u_mi, na.rm = TRUE)))
  }

  # ============================================================================
  # STEP 3: Utility Score
  # ============================================================================

  if (verbose) cat("\nStep 3/4: Computing utility scores...\n")

  u <- 0.5 * u_coef + 0.5 * u_mi
  u <- percentile01(u)

  # ============================================================================
  # STEP 4: Biological Relevance   <-- FIX IS HERE
  # ============================================================================

  if (verbose) cat("\nStep 4/4: Computing biological scores...\n")

  if (bio_mode == "none") {
    if (verbose) cat("  Biology mode: NONE (skipping biological scoring)\n")
    b <- rep(1, n_genes)
    bio_time <- 0

  } else {
    bio_start <- Sys.time()

    if (bio_mode == "data_driven") {

      # --- Candidate set used ONLY to discover enriched GO terms ---
      cand_pi <- gene_names[pi_exact > 0]

      topN <- min(1000, length(gene_names))
      cand_u <- gene_names[order(u, decreasing = TRUE)][1:topN]

      cand_genes <- unique(c(cand_pi, cand_u))
      if (length(cand_genes) < 50) cand_genes <- cand_u

      if (verbose) {
        cat(sprintf("  Biology (data-driven): enriching on %d candidates, scoring %d genes\n",
                    length(cand_genes), length(gene_names)))
      }

      b <- biological_scorer(
        genes = gene_names,                 # SCORE ALL GENES
        enrichment_genes = cand_genes,      # DISCOVER TARGETS FROM SUBSET (NEW)
        mode = "data_driven",
        target_terms = NULL,
        ontology = bio_ontology,
        sim_method = bio_sim_method,
        enrich_fdr = bio_enrich_fdr,
        min_term_freq = bio_min_term_freq,
        max_enriched_terms = bio_max_enriched,
        n_top_sims = bio_n_top_sims,
        ic_quantile = bio_ic_quantile,
        use_cache = use_cache,
        organism = "human"
      )

    } else {

      # supervised mode unchanged
      b <- biological_scorer(
        genes = gene_names,
        mode = bio_mode,
        target_terms = target_terms,
        ontology = bio_ontology,
        sim_method = bio_sim_method,
        enrich_fdr = bio_enrich_fdr,
        min_term_freq = bio_min_term_freq,
        max_enriched_terms = bio_max_enriched,
        n_top_sims = bio_n_top_sims,
        ic_quantile = bio_ic_quantile,
        use_cache = use_cache,
        organism = "human"
      )
    }

    bio_time <- as.numeric(difftime(Sys.time(), bio_start, units = "secs"))

    if (verbose) {
      cat(sprintf("  Biological scoring completed in %.2f seconds\n", bio_time))
      if (use_cache) cat("  (Using Cached Database)\n")
      cat(sprintf("  Mean biology score: %.4f\n", mean(b, na.rm = TRUE)))
      cat(sprintf("  Genes with b > 0.5: %d (%.1f%%)\n",
                  sum(b > 0.5), 100 * sum(b > 0.5) / n_genes))
    }
  }

  # ============================================================================
  # Final Score Combination
  # ============================================================================

  if (verbose) cat("\nCombining scores...\n")

  final_scores <- combine_scores(pi_exact, u, b, score_formula, score_weights)

  gene_scores <- data.frame(
    gene = gene_names,
    final_score = final_scores,
    pi_exact = pi_exact,
    u = u,
    u_coef = u_coef,
    u_mi = u_mi,
    b = b,
    stringsAsFactors = FALSE
  )

  gene_scores <- gene_scores[order(gene_scores$final_score, decreasing = TRUE), ]
  rownames(gene_scores) <- NULL

  # ============================================================================
  # Summary
  # ============================================================================

  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  if (verbose) {
    cat("\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat("COMPLETE!\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat(sprintf("Total runtime: %.2f seconds (%.2f minutes)\n", total_time, total_time/60))
    cat("\n")
    cat("Top 10 genes:\n")
    print(head(gene_scores[, c("gene", "final_score", "pi_exact", "u", "b")], 10))
    cat("\n")

    if (use_cache) {
      cat("     Use cache_info() to see cached data\n")
      cat("     Use clear_cache() to reset if needed\n\n")
    }
  }

  return(list(
    gene_scores = gene_scores,
    cv_results = list(
      auc_scores = cv_auc,
      mean_auc = mean(cv_auc, na.rm = TRUE),
      sd_auc = sd(cv_auc, na.rm = TRUE)
    ),
    cv_summary = list(
      mean = mean(cv_auc, na.rm = TRUE),
      sd = sd(cv_auc, na.rm = TRUE),
      median = median(cv_auc, na.rm = TRUE),
      min = min(cv_auc, na.rm = TRUE),
      max = max(cv_auc, na.rm = TRUE)
    ),
    parameters = list(
      K = K,
      R = R,
      n_folds = n_folds,
      bio_mode = bio_mode,
      bio_ontology = bio_ontology,
      bio_sim_method = bio_sim_method,
      bio_enrich_fdr = bio_enrich_fdr,
      bio_min_term_freq = bio_min_term_freq,
      bio_max_enriched = bio_max_enriched,
      bio_n_top_sims = bio_n_top_sims,
      bio_ic_quantile = bio_ic_quantile,
      score_formula = score_formula,
      score_weights = score_weights,
      regularization = regularization_method,
      alpha = alpha,
      gene_groups = gene_groups,
      group_penalty = group_penalty,
      mi_method = mi_method,
      use_cache = use_cache,
      n_cores = n_cores
    ),
    timing = list(
      total_seconds = total_time,
      aggregation_seconds = agg_time,
      biology_seconds = bio_time
    )
  ))
}


#' Run Single CV Fold
#'
#' Fits regularized model, computes MI on training data, and evaluates on test.
#'
#' @param fold Fold object with train/test indices
#' @param X Expression matrix
#' @param y Outcome
#' @param regularization_method Regularization method
#' @param alpha Elastic net parameter
#' @param mi_method MI estimation method
#' @param mi_bins Number of bins for MI
#' @param show_progress Show progress
#' @return List with selected genes, coefficients, MI scores, and AUC
#' @keywords internal
run_cv_fold <- function(fold, X, y, regularization_method, alpha,
                        mi_method = "discrete", mi_bins = 5,
                        gene_groups = NULL, group_penalty = "grLasso",
                        show_progress = FALSE) {

  X_train <- X[fold$train, , drop = FALSE]
  X_test  <- X[fold$test,  , drop = FALSE]
  y_train <- y[fold$train]
  y_test  <- y[fold$test]

  # --- Regularized model fitting ---
  fit_result <- fit_regularized_model(
    X_train, y_train,
    method = regularization_method,
    alpha = alpha,
    gene_groups = gene_groups,
    group_penalty = group_penalty
  )

  # --- FIX: Compute MI on training data only (prevents leakage) ---
  mi_scores <- compute_mi_vectorized(
    X_train, y_train,
    method = mi_method,
    n_bins = mi_bins
  )

  # --- Predict using the model's predict_fn (works for glmnet and grpreg) ---
  auc <- tryCatch({
    if (length(fit_result$selected) > 0 && !is.null(fit_result$predict_fn)) {
      y_pred <- fit_result$predict_fn(X_test)
      compute_auc(y_test, y_pred)
    } else {
      0.5  # No features selected → chance-level
    }
  }, error = function(e) {
    warning("AUC computation failed in fold: ", e$message)
    NA_real_
  })

  return(list(
    selected = fit_result$selected,
    coefficients = fit_result$coefficients,
    mi_scores = mi_scores,
    auc = auc,
    fold_num = fold$fold,
    repeat_num = fold$repeat_num
  ))
}
