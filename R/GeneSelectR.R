# GeneSelectR 2.0 (main fit) - FINAL VERSION
#
#' Fit GeneSelectR 2.0 with multiple formula and biology options
#'
#' @param X numeric matrix (samples x genes), colnames are gene symbols
#' @param y factor with 2 levels (binary outcome)
#'
#' @param bio_mode "supervised" or "data_driven"
#' @param target_terms GO term IDs for supervised mode
#' @param OrgDb organism database
#' @param keytype gene ID type (default "SYMBOL")
#'
#' @param score_formula aggregation formula
#'   Options: "geometric", "arithmetic", "harmonic", "minimum"
#' @param score_weights numeric vector length 3: weights for (pi, u, b)
#'
#' @param regularization_method "lasso", "elastic_net", or "group_lasso"
#' @param alpha elastic net parameter (1=lasso, 0.5=elastic net)
#' @param cor_threshold for group_lasso: correlation threshold for grouping
#'
#' @param mi_method "discrete", "continuous", or "dcor"
#' @param mi_bins bins for discrete MI
#' @param adaptive_bins use adaptive binning
#'
#' @param K outer CV folds
#' @param R outer CV repeats
#' @param seed random seed
#' @param use_lambda_1se use conservative lambda
#' @param inner_folds inner CV folds for lambda selection
#'
#' @param bio_measure semantic similarity measure
#' @param bio_combine aggregation method for gene's GO terms
#' @param bio_normalize percentile normalize bio scores
#' @param bio_top_u top genes by utility to include in bio scoring
#' @param bio_cap cap on number of genes to score
#'
#' @param score_normalize percentile normalize components before aggregation
#' @param parallel use parallel processing
#' @param n_cores number of cores for parallel
#' @param verbose show progress
#'
#' @return list with CV metrics, gene scores, and details
geneselectr2_fit <- function(
    X, y,

    # ============================================================================
    # BIOLOGY MODE
    # ============================================================================
    bio_mode = c("supervised", "data_driven"),
    target_terms = c("GO:0006955"),  # Only for supervised
    OrgDb = org.Hs.eg.db::org.Hs.eg.db,
    keytype = "SYMBOL",

    # ============================================================================
    # SCORE AGGREGATION
    # ============================================================================
    score_formula = c("geometric", "arithmetic", "harmonic", "minimum"),
    score_weights = c(1, 1, 1),  # For pi, u, b

    # ============================================================================
    # REGULARIZATION METHOD
    # ============================================================================
    regularization_method = c("lasso", "elastic_net", "group_lasso"),
    alpha = 1,  # For lasso/elastic_net
    cor_threshold = 0.8,  # For group_lasso

    # ============================================================================
    # MUTUAL INFORMATION METHOD
    # ============================================================================
    mi_method = c("discrete", "continuous", "dcor"),
    mi_bins = 5,
    adaptive_bins = TRUE,

    # ============================================================================
    # CROSS-VALIDATION SETTINGS
    # ============================================================================
    K = 5,
    R = 20,
    seed = 1,
    use_lambda_1se = FALSE,
    inner_folds = 5,

    # ============================================================================
    # BIOLOGICAL SCORING PARAMETERS
    # ============================================================================
    bio_measure = "Resnik",
    bio_combine = "max",
    bio_normalize = TRUE,
    bio_top_u = 5000,
    bio_cap = 8000,

    # ============================================================================
    # OTHER PARAMETERS
    # ============================================================================
    score_normalize = TRUE,
    parallel = FALSE,
    n_cores = NULL,
    verbose = TRUE
) {
  # ============================================================================
  # VALIDATE INPUTS
  # ============================================================================

  if (verbose) cat("GeneSelectR 2.0 FINAL: Validating inputs...\n")

  validate_geneselectr_inputs(X, y, K, R)

  # Match arguments
  bio_mode <- match.arg(bio_mode)
  score_formula <- match.arg(score_formula)
  regularization_method <- match.arg(regularization_method)
  mi_method <- match.arg(mi_method)

  # Validate weights
  if (length(score_weights) != 3) {
    stop("score_weights must be length 3 (for pi, u, b)")
  }

  # ============================================================================
  # SETUP
  # ============================================================================

  genes <- colnames(X)
  n_genes <- length(genes)
  pos_level <- levels(y)[2]

  if (verbose) {
    cat(sprintf("  Genes: %d, Samples: %d\n", n_genes, nrow(X)))
    cat(sprintf("  Biology mode: %s\n", bio_mode))
    cat(sprintf("  Score formula: %s\n", score_formula))
    cat(sprintf("  Regularization: %s\n", regularization_method))
    cat(sprintf("  MI method: %s\n", mi_method))
  }

  # ============================================================================
  # CREATE GENE GROUPS (if using group lasso)
  # ============================================================================

  gene_groups <- NULL
  if (regularization_method == "group_lasso") {
    if (verbose) {
      cat(sprintf("Creating gene groups (correlation > %.2f)...\n", cor_threshold))
    }
    gene_groups <- create_gene_groups(X, cor_threshold = cor_threshold)
    if (verbose) {
      cat(sprintf("  Created %d gene groups\n", length(unique(gene_groups))))
    }
  }

  # ============================================================================
  # CREATE CV FOLDS
  # ============================================================================

  if (verbose) {
    cat(sprintf("Creating %d-fold CV x %d repeats (%d total)...\n", K, R, K*R))
  }

  folds <- make_repeated_stratified_folds(y, K = K, R = R, seed = seed)
  nfold <- length(folds)

  # ============================================================================
  # INITIALIZE STORAGE
  # ============================================================================

  aucs <- numeric(nfold)
  selected_list <- vector("list", nfold)
  coef_list <- vector("list", nfold)
  mi_list <- vector("list", nfold)
  lambdas <- numeric(nfold)
  fold_success <- logical(nfold)

  # ============================================================================
  # SETUP PARALLEL PROCESSING
  # ============================================================================

  if (parallel) {
    if (!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("future.apply", quietly = TRUE)) {
      warning("Parallel packages not available. Using sequential.")
      parallel <- FALSE
    } else {
      if (verbose) cat("Setting up parallel processing...\n")
      if (is.null(n_cores)) n_cores <- max(1, parallel::detectCores() - 1)
      future::plan(future::multisession, workers = n_cores)
      if (verbose) cat(sprintf("  Using %d cores\n", n_cores))
    }
  }

  # ============================================================================
  # PROGRESS BAR
  # ============================================================================

  if (verbose && !parallel) {
    cat("Running CV folds...\n")
    pb <- txtProgressBar(min = 0, max = nfold, style = 3)
  }

  # ============================================================================
  # DEFINE FOLD PROCESSING FUNCTION
  # ============================================================================

  process_fold <- function(i, fold_data, X_data, y_data, params) {
    # Split data
    tr <- fold_data$train
    te <- fold_data$test

    Xtr <- X_data[tr, , drop = FALSE]
    ytr <- y_data[tr]
    Xte <- X_data[te, , drop = FALSE]
    yte <- y_data[te]

    # ----------------------------------------------------------------------
    # FIT REGULARIZED MODEL
    # ----------------------------------------------------------------------

    fit <- fit_penalized_regression(
      X_train = Xtr,
      y_train = ytr,
      method = params$regularization_method,
      alpha = params$alpha,
      gene_groups = if (!is.null(params$gene_groups)) {
        params$gene_groups
      } else {
        NULL
      },
      seed = params$seed + i,
      use_lambda_1se = params$use_lambda_1se,
      inner_folds = params$inner_folds
    )

    if (is.null(fit)) {
      return(list(
        success = FALSE,
        auc = NA,
        selected = character(0),
        abs_coef = numeric(0),
        mi = setNames(rep(NA, ncol(X_data)), colnames(X_data)),
        lambda = NA
      ))
    }

    # ----------------------------------------------------------------------
    # COMPUTE MI
    # ----------------------------------------------------------------------

    mi <- compute_mi_per_gene(
      X = Xtr,
      y = ytr,
      method = params$mi_method,
      bins = params$mi_bins,
      adaptive = params$adaptive_bins
    )

    # ----------------------------------------------------------------------
    # PREDICT AND EVALUATE
    # ----------------------------------------------------------------------

    prob <- tryCatch({
      if (params$regularization_method == "group_lasso") {
        # gglasso predict
        as.numeric(predict(fit$model, newx = Xte, type = "link"))
        # Convert to probabilities
        1 / (1 + exp(-as.numeric(predict(fit$model, newx = Xte))))
      } else {
        # glmnet predict
        as.numeric(stats::predict(fit$model, newx = Xte, type = "response"))
      }
    }, error = function(e) NULL)

    if (is.null(prob)) {
      return(list(
        success = FALSE,
        auc = NA,
        selected = character(0),
        abs_coef = numeric(0),
        mi = mi,
        lambda = fit$lambda
      ))
    }

    auc <- tryCatch({
      auc_from_probs(yte, prob, pos_level = params$pos_level)
    }, error = function(e) NA)

    return(list(
      success = TRUE,
      auc = auc,
      selected = fit$selected,
      abs_coef = fit$abs_coef,
      mi = mi,
      lambda = fit$lambda
    ))
  }

  # ============================================================================
  # PREPARE PARAMETERS
  # ============================================================================

  cv_params <- list(
    regularization_method = regularization_method,
    alpha = alpha,
    gene_groups = gene_groups,
    mi_method = mi_method,
    mi_bins = mi_bins,
    adaptive_bins = adaptive_bins,
    seed = seed,
    use_lambda_1se = use_lambda_1se,
    inner_folds = inner_folds,
    pos_level = pos_level
  )

  # ============================================================================
  # RUN CV FOLDS
  # ============================================================================

  if (parallel) {
    if (verbose) cat("Running folds in parallel...\n")
    fold_results <- future.apply::future_lapply(
      seq_len(nfold),
      function(i) process_fold(i, folds[[i]], X, y, cv_params),
      future.seed = TRUE
    )
  } else {
    fold_results <- vector("list", nfold)
    for (i in seq_len(nfold)) {
      fold_results[[i]] <- process_fold(i, folds[[i]], X, y, cv_params)
      if (verbose) setTxtProgressBar(pb, i)
    }
    if (verbose) close(pb)
  }

  # ============================================================================
  # EXTRACT RESULTS
  # ============================================================================

  for (i in seq_len(nfold)) {
    result <- fold_results[[i]]
    fold_success[i] <- result$success
    aucs[i] <- result$auc
    selected_list[[i]] <- result$selected
    coef_list[[i]] <- result$abs_coef
    mi_list[[i]] <- result$mi
    lambdas[i] <- result$lambda
  }

  n_failed <- sum(!fold_success)
  if (n_failed > 0) warning(sprintf("%d/%d folds failed", n_failed, nfold))
  if (n_failed > nfold/2) stop("More than 50% of folds failed")

  if (verbose) {
    cat(sprintf("CV complete: %d/%d folds succeeded\n", sum(fold_success), nfold))
  }

  # ============================================================================
  # AGGREGATE Pi (SELECTION PROBABILITY)
  # ============================================================================

  if (verbose) cat("Aggregating results...\n")

  N <- sum(fold_success)
  selected_successful <- selected_list[fold_success]
  sel_counts <- table(unlist(selected_successful))

  pi_exact <- setNames(numeric(length(genes)), genes)
  pi_exact[names(sel_counts)] <- as.numeric(sel_counts) / N

  # ============================================================================
  # AGGREGATE u_coef
  # ============================================================================

  coef_sum <- setNames(numeric(length(genes)), genes)
  coef_n <- setNames(integer(length(genes)), genes)

  for (i in which(fold_success)) {
    cc <- coef_list[[i]]
    if (length(cc) > 0) {
      coef_sum[names(cc)] <- coef_sum[names(cc)] + cc
      coef_n[names(cc)] <- coef_n[names(cc)] + 1L
    }
  }

  u_coef <- coef_sum
  u_coef[coef_n > 0] <- coef_sum[coef_n > 0] / coef_n[coef_n > 0]
  u_coef[coef_n == 0] <- 0

  # ============================================================================
  # AGGREGATE u_mi
  # ============================================================================

  mi_successful <- mi_list[fold_success]
  mi_mat <- do.call(cbind, mi_successful)
  u_mi <- rowMeans(mi_mat, na.rm = TRUE)
  names(u_mi) <- genes

  # ============================================================================
  # COMBINE u
  # ============================================================================

  u_coef_p <- percentile01(u_coef)
  u_mi_p <- percentile01(u_mi)
  u <- (u_coef_p + u_mi_p) / 2

  # ============================================================================
  # DETERMINE CANDIDATE GENES FOR BIO SCORING
  # ============================================================================

  if (verbose) cat("Determining candidate genes for biological scoring...\n")

  cand1 <- names(pi_exact)[pi_exact > 0]
  ord_u <- order(u, decreasing = TRUE)
  top_u <- genes[ord_u][seq_len(min(bio_top_u, length(genes)))]
  genes_subset <- unique(c(cand1, top_u))

  if (length(genes_subset) > bio_cap) {
    keep_order <- genes[ord_u][genes[ord_u] %in% genes_subset]
    genes_subset <- keep_order[seq_len(bio_cap)]
  }

  if (verbose) {
    cat(sprintf("  Scoring %d genes for biological relevance\n",
                length(genes_subset)))
  }

  # ============================================================================
  # PREPARE GO SEMANTIC DATA
  # ============================================================================

  if (verbose) cat("Loading GO semantic similarity data...\n")

  if (!requireNamespace("GOSemSim", quietly = TRUE)) {
    stop("Package 'GOSemSim' required")
  }

  semData <- GOSemSim::godata(OrgDb, ont = "BP", computeIC = TRUE)

  # ============================================================================
  # COMPUTE BIOLOGICAL SCORE
  # ============================================================================

  if (verbose) cat("Computing biological relevance scores...\n")

  b <- compute_bio_score_go(
    genes = genes,
    genes_subset = genes_subset,
    mode = bio_mode,
    target_terms = if (bio_mode == "supervised") target_terms else NULL,
    X = if (bio_mode == "data_driven") X else NULL,
    y = if (bio_mode == "data_driven") y else NULL,
    OrgDb = OrgDb,
    keytype = keytype,
    ont = "BP",
    measure = bio_measure,
    combine = bio_combine,
    normalize = bio_normalize,
    semData = semData,
    verbose = verbose
  )

  # Extract discovered terms if data-driven
  discovered_terms <- attr(b, "discovered_terms")

  # ============================================================================
  # NORMALIZE COMPONENTS
  # ============================================================================

  pi_use <- pi_exact
  u_use <- u
  b_use <- b

  if (score_normalize) {
    pi_use <- percentile01(pi_use)
    u_use <- percentile01(u_use)
    b_use <- percentile01(b_use)
  }

  # ============================================================================
  # COMPUTE FINAL SCORE
  # ============================================================================

  if (verbose) {
    cat(sprintf("Computing final scores using '%s' formula...\n", score_formula))
  }

  # Normalize weights
  w <- score_weights / sum(score_weights)

  final_score <- switch(
    score_formula,

    # GEOMETRIC MEAN
    # Property: requires balance, penalizes zeros heavily
    "geometric" = {
      (pi_use^w[1] * u_use^w[2] * b_use^w[3])^(1/sum(w))
    },

    # ARITHMETIC MEAN
    # Property: more forgiving
    "arithmetic" = {
      w[1] * pi_use + w[2] * u_use + w[3] * b_use
    },

    # HARMONIC MEAN
    # Property: between geometric and arithmetic
    "harmonic" = {
      eps <- 1e-10  # Prevent division by zero
      3 / (w[1]/(pi_use + eps) + w[2]/(u_use + eps) + w[3]/(b_use + eps))
    },

    # MINIMUM
    # Property: gene limited by weakest component
    "minimum" = {
      pmin(pi_use, u_use, b_use)
    }
  )

  # ============================================================================
  # CREATE OUTPUT DATAFRAME
  # ============================================================================

  gene_scores <- data.frame(
    gene = genes,
    final_score = final_score,
    pi_exact = pi_exact,
    u_coef = u_coef_p,
    u_mi = u_mi_p,
    u = u,
    b = b,
    stringsAsFactors = FALSE
  )

  gene_scores <- gene_scores[order(-gene_scores$final_score), , drop = FALSE]

  # ============================================================================
  # SUMMARIZE RESULTS
  # ============================================================================

  aucs_valid <- aucs[fold_success]

  cv_summary <- data.frame(
    metric = "AUC",
    mean = mean(aucs_valid, na.rm = TRUE),
    sd = stats::sd(aucs_valid, na.rm = TRUE),
    median = stats::median(aucs_valid, na.rm = TRUE),
    iqr = stats::IQR(aucs_valid, na.rm = TRUE),
    min = min(aucs_valid, na.rm = TRUE),
    max = max(aucs_valid, na.rm = TRUE),
    n_outer = length(aucs_valid),
    n_failed = n_failed,
    stringsAsFactors = FALSE
  )

  lambdas_valid <- lambdas[fold_success]

  lambda_summary <- data.frame(
    lambda_mean = mean(lambdas_valid, na.rm = TRUE),
    lambda_sd = stats::sd(lambdas_valid, na.rm = TRUE),
    lambda_median = stats::median(lambdas_valid, na.rm = TRUE),
    lambda_min = min(lambdas_valid, na.rm = TRUE),
    lambda_max = max(lambdas_valid, na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  # ============================================================================
  # CLEANUP
  # ============================================================================

  if (parallel) {
    future::plan(future::sequential)
    if (verbose) cat("Parallel processing shut down\n")
  }

  # ============================================================================
  # FINAL MESSAGE
  # ============================================================================

  if (verbose) {
    cat("\nGeneSelectR 2.0 FINAL complete!\n")
    cat(sprintf("  Mean AUC: %.4f (±%.4f)\n", cv_summary$mean, cv_summary$sd))
    cat(sprintf("  Top gene: %s (score = %.4f)\n",
                gene_scores$gene[1], gene_scores$final_score[1]))
    cat(sprintf("  Biology mode: %s\n", bio_mode))
    if (bio_mode == "data_driven" && !is.null(discovered_terms)) {
      cat(sprintf("  Discovered %d enriched pathways\n", length(discovered_terms)))
    }
    cat("\n")
  }

  # ============================================================================
  # RETURN RESULTS
  # ============================================================================

  result <- list(
    cv_auc = aucs,
    cv_summary = cv_summary,
    lambda_summary = lambda_summary,
    gene_scores = gene_scores,

    # Methodology details
    methodology = list(
      bio_mode = bio_mode,
      target_terms = if (bio_mode == "supervised") target_terms else discovered_terms,
      discovered_terms = discovered_terms,
      score_formula = score_formula,
      score_weights = score_weights,
      regularization_method = regularization_method,
      mi_method = mi_method,
      gene_groups = if (regularization_method == "group_lasso") {
        gene_groups
      } else {
        NULL
      }
    ),

    details = list(
      folds = folds,
      selected = selected_list,
      abs_coef = coef_list,
      mi = mi_list,
      lambdas = lambdas,
      fold_success = fold_success,
      pos_level = pos_level,
      bio_genes_scored = genes_subset,
      semData = semData,
      alpha = alpha,
      n_cores = if (parallel) n_cores else NULL
    )
  )

  class(result) <- c("geneselectr", "list")
  return(result)
}
