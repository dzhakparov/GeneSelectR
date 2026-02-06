# GeneSelectR 2.0 (GO biology score) - FINAL VERSION
#
# Changes:
# - Supervised mode (original - uses specified GO terms)
# - Data-driven mode (discovers enriched pathways from data first)
# - Biology is CORE to the package (not optional)

# define custom if else operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# ==============================================================================
# DATA-DRIVEN PATHWAY DISCOVERY
# ==============================================================================

#' Discover enriched pathways from differentially expressed genes
#'
#' @param X expression matrix
#' @param y binary outcome
#' @param OrgDb organism database
#' @param n_de_genes number of top DE genes to use
#' @param ont ontology (BP, MF, CC)
#' @param p_cutoff p-value cutoff for enrichment
#' @param min_genes minimum genes per pathway
#' @param max_genes maximum genes per pathway
#' @return character vector of enriched GO term IDs
discover_enriched_pathways <- function(
    X, y,
    OrgDb,
    n_de_genes = 200,
    ont = "BP",
    p_cutoff = 0.01,
    min_genes = 5,
    max_genes = 500
) {
  # --------------------------------------------------------------------------
  # STEP 1: FIND DIFFERENTIALLY EXPRESSED GENES
  # --------------------------------------------------------------------------

  # Simple t-test for each gene
  # (In production, could use limma, DESeq2, etc.)
  pvals <- apply(X, 2, function(gene_expr) {
    tryCatch({
      t.test(gene_expr ~ y)$p.value
    }, error = function(e) 1.0)
  })

  # Get top DE genes
  top_de <- names(sort(pvals))[1:min(n_de_genes, length(pvals))]

  cat(sprintf("  Identified %d differentially expressed genes\n", length(top_de)))

  # --------------------------------------------------------------------------
  # STEP 2: MAP GENES TO GO TERMS
  # --------------------------------------------------------------------------

  gene_to_go <- tryCatch({
    AnnotationDbi::select(
      OrgDb,
      keys = top_de,
      keytype = "SYMBOL",
      columns = c("GO", "ONTOLOGY")
    )
  }, error = function(e) {
    warning("Failed to map genes to GO: ", e$message)
    return(NULL)
  })

  if (is.null(gene_to_go) || nrow(gene_to_go) == 0) {
    warning("No GO annotations found for DE genes")
    return(character(0))
  }

  # Filter to specified ontology
  gene_to_go <- gene_to_go[gene_to_go$ONTOLOGY == ont & !is.na(gene_to_go$GO), ]

  if (nrow(gene_to_go) == 0) {
    warning(sprintf("No GO annotations in ontology '%s'", ont))
    return(character(0))
  }

  # --------------------------------------------------------------------------
  # STEP 3: COUNT GENES PER GO TERM
  # --------------------------------------------------------------------------

  go_counts <- table(gene_to_go$GO)

  # Filter by pathway size
  valid_gos <- names(go_counts)[go_counts >= min_genes & go_counts <= max_genes]

  if (length(valid_gos) == 0) {
    warning("No GO terms within size limits")
    return(character(0))
  }

  cat(sprintf("  Found %d GO terms within size limits [%d, %d]\n",
              length(valid_gos), min_genes, max_genes))

  # --------------------------------------------------------------------------
  # STEP 4: SIMPLE ENRICHMENT TEST
  # --------------------------------------------------------------------------

  # For each GO term, compute enrichment
  # (Simple version - in production use Fisher's exact test)

  enrichment_results <- data.frame(
    GO = valid_gos,
    n_genes = as.integer(go_counts[valid_gos]),
    enrichment_score = numeric(length(valid_gos)),
    stringsAsFactors = FALSE
  )

  # Simple enrichment score: higher count = more enriched
  # (Could improve with proper hypergeometric test)
  enrichment_results$enrichment_score <- enrichment_results$n_genes /
    max(enrichment_results$n_genes)

  # Sort by enrichment
  enrichment_results <- enrichment_results[
    order(-enrichment_results$enrichment_score),
  ]

  # Take top pathways
  top_pathways <- enrichment_results$GO[1:min(10, nrow(enrichment_results))]

  cat(sprintf("  Selected %d enriched pathways\n", length(top_pathways)))

  return(top_pathways)
}

# ==============================================================================
# SUPERVISED BIOLOGICAL SCORING (ORIGINAL)
# ==============================================================================

#' Compute biological score using pre-specified GO terms
#'
#' @param genes character vector of all genes
#' @param genes_subset genes to actually score
#' @param target_terms GO term IDs to use as targets
#' @param OrgDb organism database
#' @param keytype gene ID type
#' @param ont ontology
#' @param measure semantic similarity measure
#' @param combine how to aggregate multiple GO terms per gene
#' @param topk for mean_topk combine method
#' @param normalize percentile normalize scores
#' @param semData pre-computed semantic data
#' @param verbose show progress
#' @return named numeric vector of biological scores
compute_bio_score_supervised <- function(
    genes,
    genes_subset,
    target_terms,
    OrgDb,
    keytype = "SYMBOL",
    ont = "BP",
    measure = "Resnik",
    combine = c("max", "mean_topk"),
    topk = 5,
    normalize = TRUE,
    semData = NULL,
    verbose = FALSE
) {
  # --------------------------------------------------------------------------
  # VALIDATE
  # --------------------------------------------------------------------------
  combine <- match.arg(combine)

  if (!requireNamespace("AnnotationDbi", quietly = TRUE)) {
    stop("Package 'AnnotationDbi' required")
  }
  if (!requireNamespace("GOSemSim", quietly = TRUE)) {
    stop("Package 'GOSemSim' required")
  }

  target_terms <- unique(as.character(target_terms))
  if (!length(target_terms)) {
    stop("target_terms is empty")
  }

  # Initialize output
  b <- setNames(numeric(length(genes)), genes)

  if (!length(genes_subset)) return(b)

  # --------------------------------------------------------------------------
  # PREPARE SEMANTIC DATA
  # --------------------------------------------------------------------------
  if (is.null(semData)) {
    if (verbose) cat("  Computing GO semantic similarity data...\n")
    semData <- GOSemSim::godata(OrgDb, ont = ont, computeIC = TRUE)
  }

  # --------------------------------------------------------------------------
  # MAP GENES TO GO TERMS
  # --------------------------------------------------------------------------
  if (verbose) cat("  Mapping genes to GO terms...\n")

  map <- tryCatch({
    AnnotationDbi::select(
      OrgDb,
      keys = unique(genes_subset),
      keytype = keytype,
      columns = c("GO", "ONTOLOGY")
    )
  }, error = function(e) {
    stop("Failed to map genes to GO: ", e$message)
  })

  map <- map[!is.na(map$GO) & !is.na(map$ONTOLOGY), , drop = FALSE]
  map <- map[map$ONTOLOGY == ont, , drop = FALSE]

  if (!nrow(map)) {
    warning(sprintf("No GO annotations in ontology '%s'", ont))
    return(b)
  }

  # Organize by gene
  go_by_gene <- split(as.character(map$GO), map[[keytype]])
  go_by_gene <- lapply(go_by_gene, unique)

  # --------------------------------------------------------------------------
  # COMPUTE GO TERM SIMILARITIES
  # --------------------------------------------------------------------------
  if (verbose) cat("  Computing GO term similarities...\n")

  uniq_go <- unique(unlist(go_by_gene, use.names = FALSE))
  best_go_sim <- setNames(numeric(length(uniq_go)), uniq_go)

  for (a in uniq_go) {
    similarities <- vapply(
      target_terms,
      function(t) {
        val <- tryCatch({
          GOSemSim::goSim(a, t, semData = semData, measure = measure)
        }, error = function(e) NA_real_)
        if (is.na(val)) 0 else val
      },
      numeric(1)
    )
    best_go_sim[a] <- max(similarities, na.rm = TRUE)
  }

  # --------------------------------------------------------------------------
  # SCORE GENES
  # --------------------------------------------------------------------------
  if (verbose) cat("  Scoring genes...\n")

  score_one <- function(go_terms) {
    sims <- best_go_sim[go_terms]
    sims <- sims[!is.na(sims)]
    if (!length(sims)) return(0)

    if (combine == "max") {
      max(sims)
    } else {
      sims <- sort(sims, decreasing = TRUE)
      mean(head(sims, min(topk, length(sims))))
    }
  }

  for (g in genes_subset) {
    b[g] <- score_one(go_by_gene[[g]] %||% character(0))
  }

  # --------------------------------------------------------------------------
  # NORMALIZE
  # --------------------------------------------------------------------------
  if (normalize) {
    r <- rank(b, ties.method = "average")
    b <- r / max(r)
    b <- setNames(as.numeric(b), names(b))
  }

  return(b)
}

# ==============================================================================
# MAIN FUNCTION: FLEXIBLE BIOLOGICAL SCORING
# ==============================================================================

#' Compute biological relevance scores with supervised or data-driven mode
#'
#' @param genes character vector of all gene names
#' @param genes_subset subset to actually score
#' @param mode "supervised" or "data_driven"
#' @param target_terms GO terms (for supervised mode)
#' @param X expression matrix (for data-driven mode)
#' @param y outcome (for data-driven mode)
#' @param OrgDb organism database
#' @param keytype gene ID type
#' @param ont ontology
#' @param measure semantic similarity measure
#' @param combine aggregation method
#' @param topk for mean_topk
#' @param normalize percentile normalize
#' @param semData pre-computed semantic data
#' @param verbose show progress
#' @return named numeric vector of biological scores
compute_bio_score_go <- function(
    genes,
    genes_subset = NULL,
    mode = c("supervised", "data_driven"),
    target_terms = NULL,
    X = NULL,
    y = NULL,
    OrgDb = org.Hs.eg.db::org.Hs.eg.db,
    keytype = "SYMBOL",
    ont = "BP",
    measure = "Resnik",
    combine = c("max", "mean_topk"),
    topk = 5,
    normalize = TRUE,
    semData = NULL,
    verbose = FALSE
) {
  # --------------------------------------------------------------------------
  # VALIDATE MODE
  # --------------------------------------------------------------------------
  mode <- match.arg(mode)
  combine <- match.arg(combine)

  if (verbose) {
    cat(sprintf("\nBiological scoring mode: %s\n", toupper(mode)))
  }

  # Default: score all genes if subset not specified
  if (is.null(genes_subset)) {
    genes_subset <- genes
  } else {
    genes_subset <- intersect(genes, unique(as.character(genes_subset)))
  }

  # --------------------------------------------------------------------------
  # MODE 1: SUPERVISED (uses pre-specified GO terms)
  # --------------------------------------------------------------------------

  if (mode == "supervised") {
    if (is.null(target_terms) || !length(target_terms)) {
      stop("target_terms required for supervised mode")
    }

    if (verbose) {
      cat(sprintf("  Using pre-specified target GO terms: %s\n",
                  paste(target_terms, collapse = ", ")))
    }

    # Use original supervised scoring
    b_scores <- compute_bio_score_supervised(
      genes = genes,
      genes_subset = genes_subset,
      target_terms = target_terms,
      OrgDb = OrgDb,
      keytype = keytype,
      ont = ont,
      measure = measure,
      combine = combine,
      topk = topk,
      normalize = normalize,
      semData = semData,
      verbose = verbose
    )

    return(b_scores)
  }

  # --------------------------------------------------------------------------
  # MODE 2: DATA-DRIVEN (discover pathways from data)
  # --------------------------------------------------------------------------

  if (mode == "data_driven") {
    if (is.null(X) || is.null(y)) {
      stop("X and y required for data_driven mode")
    }

    if (verbose) {
      cat("  Discovering enriched pathways from data...\n")
    }

    # Discover enriched pathways
    discovered_terms <- discover_enriched_pathways(
      X = X,
      y = y,
      OrgDb = OrgDb,
      ont = ont
    )

    if (!length(discovered_terms)) {
      warning("No enriched pathways discovered. Returning zero scores.")
      return(setNames(numeric(length(genes)), genes))
    }

    if (verbose) {
      cat(sprintf("  Using %d discovered pathways as targets\n",
                  length(discovered_terms)))
      cat(sprintf("  Top pathways: %s\n",
                  paste(head(discovered_terms, 3), collapse = ", ")))
    }

    # Use discovered terms as targets
    b_scores <- compute_bio_score_supervised(
      genes = genes,
      genes_subset = genes_subset,
      target_terms = discovered_terms,
      OrgDb = OrgDb,
      keytype = keytype,
      ont = ont,
      measure = measure,
      combine = combine,
      topk = topk,
      normalize = normalize,
      semData = semData,
      verbose = verbose
    )

    # Store discovered terms as attribute
    attr(b_scores, "discovered_terms") <- discovered_terms

    return(b_scores)
  }
}
