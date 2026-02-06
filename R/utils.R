# GeneSelectR 2.0 (core utilities) - FINAL VERSION
#
# ==============================================================================
# INPUT VALIDATION
# ==============================================================================

validate_geneselectr_inputs <- function(X, y, K = 5, R = 20) {
  if (!is.matrix(X)) stop("X must be a matrix")
  if (!is.factor(y)) stop("y must be a factor")
  if (nlevels(y) != 2) stop("y must have exactly 2 levels (binary)")
  if (nrow(X) != length(y)) stop("Number of samples mismatch")
  if (is.null(colnames(X))) stop("X must have column names (gene symbols)")
  if (any(duplicated(colnames(X)))) stop("Duplicate gene names found")

  n_per_class <- table(y)
  if (any(n_per_class < K)) {
    stop(sprintf("Insufficient samples: need at least %d per class for %d-fold CV", K, K))
  }

  if (any(is.na(X))) warning("X contains NA values")
  if (any(is.infinite(X))) stop("X contains infinite values")

  zero_var <- apply(X, 2, sd, na.rm = TRUE) == 0
  if (any(zero_var)) {
    warning(sprintf("%d genes have zero variance", sum(zero_var)))
  }

  invisible(TRUE)
}

# ==============================================================================
# PERCENTILE NORMALIZATION
# ==============================================================================

percentile01 <- function(x) {
  if (length(unique(x[!is.na(x)])) <= 1) return(rep(0, length(x)))

  non_zero <- x > 0
  if (!any(non_zero)) return(x)

  result <- numeric(length(x))
  ranks <- rank(x[non_zero], ties.method = "average", na.last = "keep")
  result[non_zero] <- ranks / max(ranks, na.rm = TRUE)

  return(result)
}

# ==============================================================================
# MUTUAL INFORMATION - MULTIPLE METHODS (POINT 9)
# ==============================================================================

#' Adaptive bin count based on sample size
adaptive_bin_count <- function(n, default_bins = 5) {
  # Sample size based: sqrt(n) bounded between 3 and default_bins
  optimal <- max(3, min(default_bins, floor(sqrt(n))))
  return(optimal)
}

#' Quantile-based binning
qbin <- function(x, bins = 5, adaptive = TRUE) {
  unique_vals <- length(unique(x[!is.na(x)]))
  if (unique_vals < 2) return(rep.int(1L, length(x)))

  if (adaptive) {
    n <- length(x[!is.na(x)])
    bins <- adaptive_bin_count(n, default_bins = bins)
  }

  qs <- stats::quantile(x, probs = seq(0, 1, length.out = bins + 1),
                        na.rm = TRUE, type = 7)
  qs <- unique(qs)

  if (length(qs) < 3) return(rep.int(1L, length(x)))

  binned <- as.integer(cut(x, breaks = qs, include.lowest = TRUE, labels = FALSE))
  return(binned)
}

#' Discrete mutual information
mi_discrete <- function(xd, yd) {
  tab <- table(xd, yd)
  pxy <- tab / sum(tab)
  px <- rowSums(pxy)
  py <- colSums(pxy)

  nz <- pxy > 0
  rr <- row(pxy)[nz]
  cc <- col(pxy)[nz]

  sum(pxy[nz] * log(pxy[nz] / (px[rr] * py[cc])))
}

#' Continuous MI estimator using kernel density
#' Requires 'entropy' package
mi_continuous <- function(x, y) {
  if (!requireNamespace("entropy", quietly = TRUE)) {
    warning("Package 'entropy' not available. Falling back to discrete MI.")
    return(mi_discrete(qbin(x, bins = 5), as.integer(y)))
  }

  # Use entropy package's empirical MI estimator
  tryCatch({
    entropy::mi.empirical(cbind(x, as.numeric(y)))
  }, error = function(e) {
    # Fallback to discrete
    mi_discrete(qbin(x, bins = 5), as.integer(y))
  })
}

#' Distance correlation (alternative dependence measure)
#' Requires 'energy' package
distance_correlation <- function(x, y) {
  if (!requireNamespace("energy", quietly = TRUE)) {
    warning("Package 'energy' not available. Falling back to discrete MI.")
    return(mi_discrete(qbin(x, bins = 5), as.integer(y)))
  }

  tryCatch({
    energy::dcor(x, as.numeric(y))
  }, error = function(e) {
    mi_discrete(qbin(x, bins = 5), as.integer(y))
  })
}

#' Compute MI per gene with multiple methods (POINT 9 - OPTION 4)
#'
#' @param X numeric matrix samples x genes
#' @param y factor with 2 levels
#' @param method character: "discrete" (default), "continuous", "dcor"
#' @param bins integer for discrete method
#' @param adaptive logical for adaptive binning
#' @return named numeric vector MI per gene
compute_mi_per_gene <- function(X, y,
                                method = c("discrete", "continuous", "dcor"),
                                bins = 5,
                                adaptive = TRUE) {
  # --------------------------------------------------------------------------
  # VALIDATE INPUTS
  # --------------------------------------------------------------------------
  stopifnot(is.matrix(X), is.factor(y), nlevels(y) == 2)

  method <- match.arg(method)

  # --------------------------------------------------------------------------
  # SELECT MI COMPUTATION METHOD
  # --------------------------------------------------------------------------

  yd <- as.integer(y)  # Convert to 1, 2
  p <- ncol(X)
  mi_scores <- numeric(p)

  if (method == "discrete") {
    # -----------------------------------------------------------------------
    # METHOD 1: DISCRETE (original - fast, works well)
    # -----------------------------------------------------------------------
    for (j in seq_len(p)) {
      gene_binned <- qbin(X[, j], bins = bins, adaptive = adaptive)
      mi_scores[j] <- mi_discrete(gene_binned, yd)
    }

  } else if (method == "continuous") {
    # -----------------------------------------------------------------------
    # METHOD 2: CONTINUOUS (kernel density - slower, more accurate)
    # -----------------------------------------------------------------------
    for (j in seq_len(p)) {
      mi_scores[j] <- mi_continuous(X[, j], yd)
    }

  } else if (method == "dcor") {
    # -----------------------------------------------------------------------
    # METHOD 3: DISTANCE CORRELATION (different measure)
    # -----------------------------------------------------------------------
    for (j in seq_len(p)) {
      mi_scores[j] <- distance_correlation(X[, j], yd)
    }
  }

  names(mi_scores) <- colnames(X)
  return(mi_scores)
}

# ==============================================================================
# CROSS-VALIDATION FOLDS
# ==============================================================================

make_repeated_stratified_folds <- function(y, K = 5, R = 20, seed = 1) {
  stopifnot(is.factor(y), nlevels(y) == 2)
  set.seed(seed)

  n <- length(y)
  folds <- vector("list", K * R)

  levs <- levels(y)
  idx0 <- which(y == levs[1])
  idx1 <- which(y == levs[2])

  k <- 1L
  for (r in seq_len(R)) {
    idx0_sh <- sample(idx0)
    idx1_sh <- sample(idx1)

    parts0 <- split(idx0_sh, cut(seq_along(idx0_sh), K, labels = FALSE))
    parts1 <- split(idx1_sh, cut(seq_along(idx1_sh), K, labels = FALSE))

    for (f in seq_len(K)) {
      test_idx <- c(parts0[[f]], parts1[[f]])
      train_idx <- setdiff(seq_len(n), test_idx)
      folds[[k]] <- list(train = train_idx, test = test_idx)
      k <- k + 1L
    }
  }

  return(folds)
}

# ==============================================================================
# LASSO/ELASTIC NET/GROUP LASSO FITTING (POINT 8)
# ==============================================================================

#' Fit penalized regression (lasso/elastic net/group lasso)
#'
#' @param X_train training features
#' @param y_train training labels
#' @param method "lasso", "elastic_net", or "group_lasso"
#' @param alpha elastic net parameter (1=lasso, 0.5=elastic net)
#' @param gene_groups for group lasso: integer vector of group assignments
#' @param seed random seed
#' @param use_lambda_1se use conservative lambda
#' @param inner_folds inner CV folds
#' @return list with model, selected genes, coefficients, lambda
fit_penalized_regression <- function(
    X_train, y_train,
    method = c("lasso", "elastic_net", "group_lasso"),
    alpha = 1,
    gene_groups = NULL,
    seed = 1,
    use_lambda_1se = FALSE,
    inner_folds = 5
) {
  # --------------------------------------------------------------------------
  # VALIDATE
  # --------------------------------------------------------------------------
  stopifnot(is.matrix(X_train), is.factor(y_train), nlevels(y_train) == 2)
  method <- match.arg(method)

  set.seed(seed)
  y01 <- as.integer(y_train) - 1L

  # --------------------------------------------------------------------------
  # METHOD 1 & 2: LASSO OR ELASTIC NET (using glmnet)
  # --------------------------------------------------------------------------

  if (method %in% c("lasso", "elastic_net")) {
    if (!requireNamespace("glmnet", quietly = TRUE)) {
      stop("Package 'glmnet' required")
    }

    # Fit with CV to select lambda
    cvfit <- tryCatch({
      glmnet::cv.glmnet(
        x = X_train,
        y = y01,
        family = "binomial",
        alpha = alpha,
        nfolds = inner_folds,
        type.measure = "deviance"
      )
    }, error = function(e) {
      warning("cv.glmnet failed: ", e$message)
      return(NULL)
    })

    if (is.null(cvfit)) return(NULL)

    # Select lambda
    lam <- if (use_lambda_1se) cvfit$lambda.1se else cvfit$lambda.min

    # Refit at selected lambda
    fit <- tryCatch({
      glmnet::glmnet(X_train, y01, family = "binomial",
                     alpha = alpha, lambda = lam)
    }, error = function(e) NULL)

    if (is.null(fit)) return(NULL)

    # Extract coefficients
    beta <- as.matrix(stats::coef(fit))[, 1]
    beta <- beta[names(beta) != "(Intercept)"]
    selected <- names(beta)[beta != 0]
    abs_coef <- abs(beta[selected])

    return(list(
      model = fit,
      selected = selected,
      abs_coef = abs_coef,
      lambda = lam,
      method = method,
      alpha = alpha
    ))
  }

  # --------------------------------------------------------------------------
  # METHOD 3: GROUP LASSO (using gglasso)
  # --------------------------------------------------------------------------

  if (method == "group_lasso") {
    if (is.null(gene_groups)) {
      stop("gene_groups required for group_lasso method")
    }

    if (!requireNamespace("gglasso", quietly = TRUE)) {
      stop("Package 'gglasso' required. Install: install.packages('gglasso')")
    }

    # Fit group lasso with CV
    cvfit <- tryCatch({
      gglasso::cv.gglasso(
        x = X_train,
        y = y01,
        group = gene_groups,
        loss = "logit",
        nfolds = inner_folds
      )
    }, error = function(e) {
      warning("cv.gglasso failed: ", e$message)
      return(NULL)
    })

    if (is.null(cvfit)) return(NULL)

    # Select lambda
    lam_idx <- if (use_lambda_1se) {
      cvfit$lambda.1se.index
    } else {
      cvfit$lambda.min.index
    }

    lam <- cvfit$lambda[lam_idx]

    # Refit at selected lambda
    fit <- tryCatch({
      gglasso::gglasso(
        x = X_train,
        y = y01,
        group = gene_groups,
        loss = "logit",
        lambda = lam
      )
    }, error = function(e) NULL)

    if (is.null(fit)) return(NULL)

    # Extract coefficients
    beta <- as.vector(fit$beta)
    names(beta) <- colnames(X_train)
    selected <- names(beta)[beta != 0]
    abs_coef <- abs(beta[selected])

    return(list(
      model = fit,
      selected = selected,
      abs_coef = abs_coef,
      lambda = lam,
      method = "group_lasso",
      gene_groups = gene_groups
    ))
  }
}

# ==============================================================================
# GENE GROUPING FOR GROUP LASSO (POINT 8)
# ==============================================================================

#' Create gene groups based on correlation for group lasso
#'
#' @param X expression matrix
#' @param cor_threshold correlation threshold for grouping
#' @param method correlation method
#' @return integer vector of group assignments
create_gene_groups <- function(X, cor_threshold = 0.8, method = "pearson") {
  # --------------------------------------------------------------------------
  # COMPUTE CORRELATION MATRIX
  # --------------------------------------------------------------------------
  cor_matrix <- cor(X, method = method, use = "pairwise.complete.obs")
  cor_matrix_abs <- abs(cor_matrix)
  diag(cor_matrix_abs) <- 0

  # --------------------------------------------------------------------------
  # HIERARCHICAL CLUSTERING
  # --------------------------------------------------------------------------
  # Distance = 1 - |correlation|
  dist_matrix <- as.dist(1 - cor_matrix_abs)
  hclust_result <- hclust(dist_matrix, method = "complete")

  # Cut tree at height corresponding to correlation threshold
  # height = 1 - cor_threshold
  gene_groups <- cutree(hclust_result, h = 1 - cor_threshold)

  return(gene_groups)
}

# ==============================================================================
# AUC CALCULATION
# ==============================================================================

auc_from_probs <- function(y_true, prob_pos, pos_level = NULL) {
  stopifnot(is.factor(y_true), nlevels(y_true) == 2)

  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Package 'pROC' required")
  }

  if (is.null(pos_level)) pos_level <- levels(y_true)[2]

  roc_obj <- pROC::roc(
    response = y_true,
    predictor = prob_pos,
    levels = levels(y_true),
    direction = "<",
    quiet = TRUE
  )

  as.numeric(pROC::auc(roc_obj))
}
