#' Validate GeneSelectR Inputs
#'
#' @param X Expression matrix
#' @param y Outcome vector
#' @param gene_names Gene names
#' @param n_folds Number of CV folds
#' @keywords internal
validate_inputs <- function(X, y, gene_names = NULL, n_folds = 5) {
  if (!is.matrix(X)) stop("X must be a matrix")
  if (!is.factor(y)) stop("y must be a factor")
  if (nlevels(y) != 2) stop("y must have exactly 2 levels (binary)")
  if (nrow(X) != length(y)) stop("Number of samples mismatch")
  if (is.null(colnames(X))) stop("X must have column names (gene symbols)")
  if (any(duplicated(colnames(X)))) stop("Duplicate gene names found")

  n_per_class <- table(y)
  if (any(n_per_class < n_folds)) {
    stop(sprintf("Insufficient samples: need at least %d per class for %d-fold CV", n_folds, n_folds))
  }

  if (any(is.na(X))) warning("X contains NA values")
  if (any(is.infinite(X))) stop("X contains infinite values")

  zero_var <- apply(X, 2, sd, na.rm = TRUE) == 0
  if (any(zero_var)) {
    warning(sprintf("%d genes have zero variance", sum(zero_var)))
  }

  invisible(TRUE)
}

#' Percentile Normalization
#'
#' Normalizes values to [0,1] range using percentile ranks, handling zeros.
#'
#' @param x Numeric vector
#' @return Normalized vector in [0,1]
#'
#' @examples
#' x <- c(0, 1, 2, 3, 4, 5)
#' percentile01(x)
#'
#' @export
percentile01 <- function(x) {
  if (length(unique(x[!is.na(x)])) <= 1) return(rep(0, length(x)))

  non_zero_mask <- x > 0 & !is.na(x)
  if (!any(non_zero_mask)) return(x * 0)

  result <- numeric(length(x))
  result[non_zero_mask] <- rank(x[non_zero_mask], ties.method = "average") / sum(non_zero_mask)

  return(result)
}

#' Adaptive Bin Count
#'
#' @param n Sample size
#' @param default_bins Maximum bins
#' @return Optimal number of bins
#' @keywords internal
adaptive_bin_count <- function(n, default_bins = 5) {
  optimal <- floor(sqrt(n))
  return(max(3, min(default_bins, optimal)))
}

#' Discretize Continuous Values
#'
#' @param x Numeric vector
#' @param n_bins Number of bins
#' @param adaptive Use adaptive binning
#' @return Integer vector of bin assignments
#' @keywords internal
discretize_continuous <- function(x, n_bins = 5, adaptive = TRUE) {
  unique_vals <- length(unique(x[!is.na(x)]))
  if (unique_vals < 2) return(rep(0L, length(x)))

  if (adaptive) {
    n_bins <- adaptive_bin_count(length(x[!is.na(x)]), default_bins = n_bins)
  }

  quantiles <- seq(0, 1, length.out = n_bins + 1)
  breaks <- quantile(x, probs = quantiles, na.rm = TRUE)
  breaks <- unique(breaks)

  if (length(breaks) < 3) return(rep(0L, length(x)))

  binned <- cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE)
  binned[is.na(binned)] <- 0L

  return(as.integer(binned))
}

#' Compute Mutual Information
#'
#' Computes mutual information between continuous x and binary y.
#'
#' @param x Numeric vector (gene expression)
#' @param y Factor or integer (binary outcome)
#' @param method "discrete" or "continuous"
#' @param n_bins Number of bins for discrete method
#' @param adaptive Use adaptive binning
#' @return Mutual information in nats
#'
#' @examples
#' \dontrun{
#' x <- rnorm(100)
#' y <- factor(rep(c(0, 1), each = 50))
#' mi <- compute_mutual_information(x, y)
#' }
#'
#' @export
compute_mutual_information <- function(x, y, method = "discrete", n_bins = 5, adaptive = TRUE) {
  y <- as.integer(as.factor(y)) - 1

  if (method == "discrete") {
    x_binned <- discretize_continuous(x, n_bins = n_bins, adaptive = adaptive)

    if (length(unique(x_binned)) < 2) return(0)

    tbl <- table(x_binned, y)
    n <- sum(tbl)

    p_xy <- tbl / n
    p_x <- rowSums(p_xy)
    p_y <- colSums(p_xy)

    mi <- 0
    for (i in seq_along(p_x)) {
      for (j in seq_along(p_y)) {
        if (p_xy[i, j] > 0) {
          mi <- mi + p_xy[i, j] * log(p_xy[i, j] / (p_x[i] * p_y[j]))
        }
      }
    }

    return(mi)

  } else if (method == "continuous") {
    warning("Continuous MI not fully implemented, using discrete")
    return(compute_mutual_information(x, y, method = "discrete", n_bins, adaptive))
  }
}

#' Compute Mutual Information (Vectorized)
#'
#' @param X Matrix (n_samples x n_genes)
#' @param y Binary outcome
#' @param method MI estimation method
#' @param n_bins Number of bins
#' @param adaptive Adaptive binning
#' @return Vector of MI scores (n_genes)
#' @keywords internal
compute_mi_vectorized <- function(X, y, method = "discrete", n_bins = 5, adaptive = TRUE) {
  n_genes <- ncol(X)

  mi_scores <- vapply(1:n_genes, function(j) {
    compute_mutual_information(X[, j], y, method = method, n_bins = n_bins, adaptive = adaptive)
  }, FUN.VALUE = numeric(1))

  return(mi_scores)
}

#' Create Cross-Validation Folds
#'
#' Creates stratified K-fold CV with R repeats.
#'
#' @param y Factor, outcome variable
#' @param K Number of folds
#' @param R Number of repeats
#' @param random_seed Random seed
#' @return List of train/test indices
#'
#' @examples
#' \dontrun{
#' y <- factor(rep(c("A", "B"), each = 50))
#' folds <- create_cv_folds(y, K = 5, R = 10)
#' }
#'
#' @export
create_cv_folds <- function(y, K = 5, R = 20, random_seed = 123) {
  set.seed(random_seed)

  n <- length(y)
  folds <- list()
  fold_counter <- 1

  for (r in 1:R) {
    class_indices <- split(1:n, y)

    fold_assignments <- list()
    for (class in names(class_indices)) {
      idx <- class_indices[[class]]
      n_class <- length(idx)

      idx <- sample(idx)
      fold_assignments[[class]] <- split(idx, cut(1:n_class, breaks = K, labels = FALSE))
    }

    for (k in 1:K) {
      test_idx <- unlist(lapply(fold_assignments, function(x) x[[k]]))
      train_idx <- setdiff(1:n, test_idx)

      folds[[fold_counter]] <- list(
        train = train_idx,
        test = test_idx,
        fold = k,
        repeat_num = r
      )
      fold_counter <- fold_counter + 1
    }
  }

  return(folds)
}

#' Fit Regularized Logistic Regression
#'
#' Supports lasso, elastic net (via glmnet), and group lasso (via grpreg).
#' Returns a model-agnostic predict_fn so the caller doesn't need to know
#' which package was used.
#'
#' @param X_train Training feature matrix (n_train x n_genes)
#' @param y_train Training labels (factor with 2 levels)
#' @param method "lasso", "elastic_net", or "group_lasso"
#' @param alpha Elastic net mixing parameter (ignored for group_lasso)
#' @param cv_folds CV folds for lambda selection
#' @param gene_groups Integer vector of length ncol(X_train) assigning each gene
#'   to a group (required for group_lasso; ignored for lasso/elastic_net).
#'   If NULL and method is "group_lasso", groups are auto-generated from
#'   correlation clustering.
#' @param group_penalty "grLasso" (default), "grMCP", or "grSCAD" for group_lasso
#' @return List with: selected (gene indices), coefficients (|beta|), lambda,
#'   n_selected, cv_fit (model object), predict_fn (function: X_test -> probabilities)
#'
#' @importFrom glmnet glmnet cv.glmnet coef.glmnet
#'
#' @keywords internal
fit_regularized_model <- function(X_train, y_train, method = "elastic_net",
                                  alpha = 0.5, cv_folds = 5,
                                  gene_groups = NULL, group_penalty = "grLasso") {

  y_numeric <- as.numeric(y_train) - 1
  n_genes <- ncol(X_train)

  # =========================================================================
  # Lasso / Elastic Net (glmnet)
  # =========================================================================
  if (method %in% c("lasso", "elastic_net")) {
    if (method == "lasso") alpha <- 1.0

    cv_fit <- glmnet::cv.glmnet(
      X_train, y_numeric,
      family = "binomial",
      alpha = alpha,
      nfolds = cv_folds,
      type.measure = "auc"
    )

    coef_vec <- as.vector(glmnet::coef.glmnet(cv_fit, s = "lambda.min"))[-1]
    selected <- which(coef_vec != 0)
    coefs <- abs(coef_vec[selected])
    lambda_opt <- cv_fit$lambda.min

    # Model-agnostic prediction function (returns probabilities)
    predict_fn <- function(X_new) {
      as.numeric(predict(cv_fit, newx = X_new, s = "lambda.min", type = "response"))
    }

    # =========================================================================
    # Group Lasso (grpreg)
    # =========================================================================
  } else if (method == "group_lasso") {

    if (!requireNamespace("grpreg", quietly = TRUE)) {
      stop("Group lasso requires the 'grpreg' package.\n",
           "Install with: install.packages('grpreg')")
    }

    # --- Determine gene groups ---
    if (is.null(gene_groups)) {
      # Auto-generate groups via hierarchical clustering on the
      # correlation matrix. This creates ~sqrt(p) groups where
      # correlated genes land in the same group.
      gene_groups <- auto_generate_groups(X_train)
    }

    # Validate group vector
    if (length(gene_groups) != n_genes) {
      stop(sprintf("gene_groups length (%d) must equal ncol(X_train) (%d)",
                   length(gene_groups), n_genes))
    }

    # cv.grpreg fits group-penalized logistic regression across a
    # grid of lambda values and selects the best via CV.
    # penalty options: "grLasso" (group lasso), "grMCP", "grSCAD"
    cv_fit <- grpreg::cv.grpreg(
      X = X_train,
      y = y_numeric,
      group = gene_groups,
      family = "binomial",
      penalty = group_penalty,
      nfolds = cv_folds
    )

    # Extract coefficients at optimal lambda
    # grpreg returns intercept + p coefficients; drop the intercept [-1]
    lambda_opt <- cv_fit$lambda.min
    coef_vec <- as.numeric(coef(cv_fit, lambda = lambda_opt))[-1]

    selected <- which(coef_vec != 0)
    coefs <- abs(coef_vec[selected])

    # Model-agnostic prediction function
    predict_fn <- function(X_new) {
      as.numeric(predict(cv_fit, X = X_new, lambda = lambda_opt, type = "response"))
    }

  } else {
    stop("Unknown regularization method: ", method,
         ". Choose 'lasso', 'elastic_net', or 'group_lasso'.")
  }

  return(list(
    selected = selected,          # Integer vector: which gene columns have nonzero coefs
    coefficients = coefs,         # |beta| for the selected genes
    lambda = lambda_opt,          # Optimal regularization strength
    n_selected = length(selected),
    cv_fit = cv_fit,              # The raw model object (glmnet or grpreg)
    predict_fn = predict_fn       # Function: X_test -> predicted probabilities
  ))
}


#' Auto-generate Gene Groups from Correlation Clustering
#'
#' Uses hierarchical clustering on the gene correlation matrix to assign
#' each gene to a group. Genes with similar expression patterns (correlated)
#' end up in the same group. The number of groups is set to approximately
#' sqrt(p) where p = number of genes, capped at 500 for computational
#' feasibility.
#'
#' @param X Training expression matrix (n_samples x n_genes)
#' @param max_groups Maximum number of groups (default: 500)
#' @param cor_method Correlation method: "pearson" or "spearman"
#' @return Integer vector of group assignments (length = ncol(X))
#'
#' @keywords internal
auto_generate_groups <- function(X, max_groups = 500, cor_method = "pearson") {

  n_genes <- ncol(X)

  # Target number of groups: sqrt(p), capped
  n_groups <- min(max_groups, max(10, floor(sqrt(n_genes))))

  # For very large gene sets (>5000), computing the full correlation matrix
  # is expensive. Use a subsample of samples for speed.
  if (nrow(X) > 200) {
    set.seed(1)
    X_sub <- X[sample(nrow(X), 200), ]
  } else {
    X_sub <- X
  }

  # Compute correlation matrix and convert to a distance
  # cor = 1 → distance = 0 (identical); cor = -1 → distance = 2 (opposite)
  # For very wide matrices (>10K genes), this is the bottleneck.
  # We use a chunked approach for memory efficiency.
  if (n_genes <= 10000) {
    cor_mat <- cor(X_sub, method = cor_method, use = "pairwise.complete.obs")
    dist_mat <- as.dist(1 - cor_mat)
  } else {
    # For >10K genes, full cor matrix is ~800MB+. Use a fast approximate
    # approach: random projection to reduce dimensionality, then cluster.
    set.seed(2)
    n_proj <- min(500, nrow(X_sub))
    proj_matrix <- matrix(rnorm(nrow(X_sub) * n_proj), nrow = nrow(X_sub))
    X_proj <- t(crossprod(proj_matrix, X_sub))  # n_genes x n_proj
    dist_mat <- dist(X_proj, method = "euclidean")
  }

  # Hierarchical clustering (Ward's method minimizes within-group variance)
  hc <- hclust(dist_mat, method = "ward.D2")

  # Cut the dendrogram into n_groups clusters
  groups <- cutree(hc, k = n_groups)

  return(groups)
}


#' Create Gene Groups from GO Pathway Membership
#'
#' Assigns genes to groups based on their Gene Ontology Biological Process
#' annotations. Genes sharing the same most-specific GO term are grouped
#' together. Genes with no GO annotation get their own singleton group.
#'
#' @param gene_names Character vector of gene symbols
#' @param go_cache Named list mapping genes to GO term vectors (from load_go_cache)
#' @param min_group_size Minimum genes per group (smaller groups are merged)
#' @return Integer vector of group assignments (length = length(gene_names))
#'
#' @export
create_pathway_groups <- function(gene_names, go_cache = NULL, min_group_size = 3) {

  n_genes <- length(gene_names)

  # Load GO annotations if not provided
  if (is.null(go_cache)) {
    if (requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
      go_cache <- load_go_cache(organism = "human")
    } else {
      warning("No GO data available, falling back to correlation-based groups")
      return(NULL)
    }
  }

  # For each gene, find its most specific (least common) GO term
  # That term defines its initial group
  all_terms <- unique(unlist(go_cache[gene_names]))
  if (length(all_terms) == 0) {
    warning("No GO annotations found, falling back to correlation-based groups")
    return(NULL)
  }

  # Count term frequency to identify the most specific term per gene
  term_freq <- table(unlist(go_cache[gene_names]))

  # Assign each gene to its rarest GO term (most specific annotation)
  group_labels <- character(n_genes)
  for (i in seq_along(gene_names)) {
    gene <- gene_names[i]
    terms <- go_cache[[gene]]
    if (is.null(terms) || length(terms) == 0) {
      group_labels[i] <- paste0("singleton_", i)
    } else {
      # Pick the term with the lowest frequency (most specific)
      freqs <- term_freq[terms]
      group_labels[i] <- names(which.min(freqs))
    }
  }

  # Convert to integer groups
  group_factor <- as.factor(group_labels)
  groups <- as.integer(group_factor)

  # Merge small groups into a catch-all group
  group_sizes <- table(groups)
  small_groups <- as.integer(names(group_sizes[group_sizes < min_group_size]))
  if (length(small_groups) > 0) {
    catchall_id <- max(groups) + 1L
    groups[groups %in% small_groups] <- catchall_id
    # Re-number to be contiguous 1..K
    groups <- as.integer(as.factor(groups))
  }

  cat(sprintf("  Pathway groups: %d groups from %d genes (range: %d-%d genes/group)\n",
              length(unique(groups)), n_genes,
              min(table(groups)), max(table(groups))))

  return(groups)
}

#' Aggregate CV Results (Vectorized)
#'
#' Aggregates selection frequency, coefficient magnitudes, and MI scores
#' across all CV folds.
#'
#' @param cv_results List of CV fold results
#' @param n_genes Total number of genes
#' @return List with pi_exact, u_coef, u_mi
#' @keywords internal
aggregate_cv_results <- function(cv_results, n_genes) {
  n_folds <- length(cv_results)

  selection_matrix <- matrix(FALSE, nrow = n_genes, ncol = n_folds)
  coef_matrix <- matrix(0, nrow = n_genes, ncol = n_folds)

  # FIX: Also aggregate MI scores computed per-fold on training data only.
  # This prevents the leakage that occurred when MI was computed once on the
  # full dataset (including test labels).
  mi_matrix <- matrix(0, nrow = n_genes, ncol = n_folds)

  for (fold_idx in 1:n_folds) {
    result <- cv_results[[fold_idx]]
    selected <- result$selected

    if (length(selected) > 0) {
      selection_matrix[selected, fold_idx] <- TRUE
      coef_matrix[selected, fold_idx] <- result$coefficients
    }

    # MI scores are available for all genes (computed on training fold)
    if (!is.null(result$mi_scores) && length(result$mi_scores) == n_genes) {
      mi_matrix[, fold_idx] <- result$mi_scores
    }
  }

  # Selection stability: fraction of folds each gene was selected
  pi_exact <- rowMeans(selection_matrix)

  # Coefficient utility: mean |coef| when selected
  coef_sums <- rowSums(coef_matrix)
  coef_counts <- rowSums(selection_matrix)
  u_coef_raw <- coef_sums / pmax(coef_counts, 1)
  u_coef <- percentile01(u_coef_raw)

  # MI utility: mean MI across folds (each fold used only training data)
  u_mi_raw <- rowMeans(mi_matrix)
  u_mi <- percentile01(u_mi_raw)

  return(list(
    pi_exact = pi_exact,
    u_coef = u_coef,
    u_mi = u_mi,
    selection_matrix = selection_matrix,
    coef_matrix = coef_matrix,
    mi_matrix = mi_matrix
  ))
}

#' Compute AUC from Predictions
#'
#' @param y_true True labels
#' @param y_pred Predicted probabilities
#' @return AUC score
#'
#' @importFrom pROC roc auc
#'
#' @keywords internal
compute_auc <- function(y_true, y_pred) {
  y_numeric <- as.numeric(as.factor(y_true)) - 1

  tryCatch({
    roc_obj <- pROC::roc(y_numeric, y_pred, quiet = TRUE)
    return(as.numeric(pROC::auc(roc_obj)))
  }, error = function(e) {
    warning("AUC computation failed: ", e$message)
    return(NA_real_)
  })
}

#' Combine Scores into Final Score
#'
#' @param pi Stability scores
#' @param u Utility scores
#' @param b Biology scores
#' @param formula Combination formula
#' @param weights Weights for (pi, u, b)
#' @return Final combined scores
#' @keywords internal
combine_scores <- function(pi, u, b, formula = "geometric", weights = c(1, 1, 1)) {
  w <- weights / sum(weights)
  eps <- 1e-10

  if (formula == "geometric") {
    final <- ((pi + eps)^w[1] * (u + eps)^w[2] * (b + eps)^w[3])^(1/sum(w))
  } else if (formula == "arithmetic") {
    final <- w[1] * pi + w[2] * u + w[3] * b
  } else if (formula == "harmonic") {
    final <- sum(w) / (w[1]/(pi + eps) + w[2]/(u + eps) + w[3]/(b + eps))
  } else if (formula == "minimum") {
    final <- pmin(pi, u, b)
  } else {
    stop("Unknown formula: ", formula)
  }

  return(final)
}
