# =============================================================================
# biological_scorer.R — Gene Ontology-based biological relevance scoring
#
# This file contains:
#   1. biological_scorer()             — top-level API (all parameters exposed)
#   2. compute_supervised_scores()     — score genes vs user-specified GO terms
#   3. compute_data_driven_scores()    — score genes vs auto-detected enriched terms
#   4. compute_semantic_similarity()   — dispatcher for all similarity metrics
#   5. sim_resnik / sim_lin / sim_jiang / sim_rel — individual metric functions
#   6. filter_go_cache_by_ontology()   — restrict to BP/MF/CC subsets
#   7. get_go_ancestors()              — retrieve GO DAG ancestors
#   8. load_ancestor_map()             — pre-build and cache ancestor lookup
#   9. compute_information_content()   — IC scores from annotation frequency
#  10. test_go_enrichment()            — Fisher's exact test for enrichment
#
# CUSTOMIZABLE PARAMETERS:
#   ontology     — "BP", "MF", "CC", or any combination (e.g., c("BP","MF"))
#   sim_method   — "resnik", "lin", "jiang", "rel" (Schlicker relevance)
#   enrich_fdr   — FDR threshold for enriched term selection (data-driven mode)
#   min_term_freq — minimum annotation frequency for a term to be a candidate
#   max_enriched_terms — cap on the number of enriched target terms
#   n_top_sims   — how many top pairwise similarities to average per gene
#   ic_quantile  — specificity filter: keep terms with IC above this quantile
#
# SIMILARITY METRICS:
#   All metrics use the Most Informative Common Ancestor (MICA) of two terms.
#   IC = Information Content = -log(frequency) of a term in the annotation corpus.
#
#   Resnik (normalized):
#     sim = IC(MICA) / max(IC(t1), IC(t2))
#     Range: [0, 1]. Simple, robust. Best for small gene sets.
#
#   Lin:
#     sim = 2 * IC(MICA) / (IC(t1) + IC(t2))
#     Range: [0, 1]. Symmetric normalization. Most commonly used.
#
#   Jiang-Conrath (converted to similarity):
#     distance = IC(t1) + IC(t2) - 2 * IC(MICA)
#     sim = 1 / (1 + distance)
#     Range: (0, 1]. Emphasizes differences more than Resnik/Lin.
#
#   Relevance (Schlicker et al.):
#     sim = Lin(t1, t2) * (1 - p(MICA))
#     where p(MICA) = annotation probability of MICA = exp(-IC(MICA))
#     Range: [0, 1). Penalizes shallow common ancestors.
#     Best for discriminating between closely-related terms.
#
# =============================================================================


#' Compute Biological Relevance Scores
#'
#' Scores genes based on GO semantic similarity to target terms (supervised)
#' or auto-detected enriched terms (data-driven). Fully customizable: ontology
#' selection, similarity metric, enrichment parameters, and aggregation.
#'
#' @param genes Character vector of gene symbols (genes to SCORE)
#' @param mode "supervised" or "data_driven"
#' @param target_terms GO term IDs for supervised mode (e.g., "GO:0006955")
#' @param ontology Character vector of GO ontologies to use. Any subset of
#'   c("BP", "MF", "CC"). Default: "BP".
#' @param sim_method Semantic similarity metric. One of: "resnik", "lin",
#'   "jiang", "rel".
#' @param enrich_fdr FDR threshold for Fisher enrichment in data-driven mode.
#' @param min_term_freq Minimum annotation frequency for candidate terms.
#' @param max_enriched_terms Maximum number of enriched terms used as targets.
#' @param n_top_sims Top-k similarities to average per gene.
#' @param ic_quantile Specificity filter quantile for IC.
#' @param enrichment_genes Character vector of gene symbols used ONLY to
#'   discover enriched GO terms in data-driven mode. If NULL, defaults to `genes`.
#'   This lets you learn targets from a candidate subset but score all genes.
#' @param go_cache Pre-loaded GO cache (gene -> GO terms). If NULL, loaded.
#' @param use_cache Use disk/memory caching.
#' @param organism Organism name.
#'
#' @return Numeric vector of biological scores (one per gene, same order as input),
#'   percentile-normalized.
#'
#' @export
biological_scorer <- function(
    genes,
    mode = "supervised",
    target_terms = NULL,
    ontology = "BP",
    sim_method = "resnik",
    enrich_fdr = 0.05,
    min_term_freq = NULL,
    max_enriched_terms = 100,
    n_top_sims = 5,
    ic_quantile = 0.5,
    enrichment_genes = NULL,
    go_cache = NULL,
    use_cache = TRUE,
    organism = "human"
) {

  # --- Validate parameters ---
  ontology <- match.arg(ontology, c("BP", "MF", "CC"), several.ok = TRUE)
  sim_method <- match.arg(sim_method, c("resnik", "lin", "jiang", "rel"))
  stopifnot(is.numeric(enrich_fdr), enrich_fdr > 0, enrich_fdr <= 1)
  stopifnot(is.numeric(n_top_sims), n_top_sims >= 1)
  stopifnot(is.numeric(ic_quantile), ic_quantile >= 0, ic_quantile < 1)
  stopifnot(is.numeric(max_enriched_terms), max_enriched_terms >= 1)
  if (!is.null(min_term_freq)) stopifnot(is.numeric(min_term_freq), min_term_freq >= 1)

  #if not provided, discover targets from the same set you score
  if (is.null(enrichment_genes)) enrichment_genes <- genes

  # --- Load GO annotations ---
  if (is.null(go_cache)) {
    if (use_cache) {
      go_cache <- load_go_cache(organism = organism)
    } else {
      go_cache <- download_go_annotations(organism)
    }
  }

  annotated_genes <- intersect(genes, names(go_cache))

  if (length(annotated_genes) == 0) {
    warning("No GO annotations found for any genes. Setting all b scores to 1.0")
    return(rep(1, length(genes)))
  }

  if (length(annotated_genes) < length(genes) * 0.5) {
    warning(sprintf(
      "Only %.1f%% of genes have GO annotations (%d/%d)",
      100 * length(annotated_genes) / length(genes),
      length(annotated_genes),
      length(genes)
    ))
  }

  # --- Filter GO cache to selected ontologies ---
  go_cache_filtered <- filter_go_cache_by_ontology(go_cache, ontology)

  # Recheck after filtering (for scoring genes)
  annotated_after <- intersect(genes, names(go_cache_filtered))
  if (length(annotated_after) == 0) {
    warning(sprintf(
      "No genes have GO annotations in ontology [%s]. Setting all b = 1.0",
      paste(ontology, collapse = ", ")
    ))
    return(rep(1, length(genes)))
  }

  # --- Compute IC from the ontology-filtered annotations ---
  ic_scores <- compute_information_content(go_cache_filtered)

  # --- Load ancestor map and create similarity cache ---
  ancestor_map <- load_ancestor_map(organism = organism, use_cache = use_cache)
  similarity_cache <- create_similarity_cache()

  # --- Dispatch to scoring mode ---
  if (mode == "supervised") {

    if (is.null(target_terms) || length(target_terms) == 0) {
      stop("target_terms required for supervised mode")
    }

    scores <- compute_supervised_scores(
      genes = genes,
      target_terms = target_terms,
      go_cache = go_cache_filtered,
      ic_scores = ic_scores,
      similarity_cache = similarity_cache,
      ancestor_map = ancestor_map,
      sim_method = sim_method,
      n_top_sims = n_top_sims
    )

  } else if (mode == "data_driven") {

    scores <- compute_data_driven_scores(
      genes = genes,                        # SCORE these
      enrichment_genes = enrichment_genes,  # DISCOVER targets from these
      go_cache = go_cache_filtered,
      ic_scores = ic_scores,
      similarity_cache = similarity_cache,
      ancestor_map = ancestor_map,
      sim_method = sim_method,
      enrich_fdr = enrich_fdr,
      min_term_freq = min_term_freq,
      max_enriched_terms = max_enriched_terms,
      n_top_sims = n_top_sims,
      ic_quantile = ic_quantile
    )

  } else {
    stop("mode must be 'supervised' or 'data_driven'")
  }

  # Percentile-normalize to [0, 1]
  scores <- percentile01(scores)
  return(scores)
}



# =============================================================================
# ONTOLOGY FILTERING
# =============================================================================

#' Filter GO Cache by Ontology
#'
#' Removes GO terms that don't belong to the specified ontologies.
#' Requires GO.db package. If unavailable, returns the cache unmodified
#' with a warning.
#'
#' @param go_cache Named list of gene -> GO terms
#' @param ontology Character vector of ontologies to keep ("BP", "MF", "CC")
#' @importFrom AnnotationDbi select
#' @return Filtered go_cache (genes with zero remaining terms are dropped)
#' @keywords internal
filter_go_cache_by_ontology <- function(go_cache, ontology = "BP") {

  if (length(ontology) == 3 &&
      all(c("BP", "MF", "CC") %in% ontology)) {
    # All ontologies selected — no filtering needed
    return(go_cache)
  }

  if (!requireNamespace("GO.db", quietly = TRUE)) {
    warning(
      "GO.db package not available. Cannot filter by ontology. ",
      "Using all GO terms. Install with: BiocManager::install('GO.db')"
    )
    return(go_cache)
  }

  # Get all unique terms across all genes
  all_terms <- unique(unlist(go_cache))
  if (length(all_terms) == 0) return(go_cache)

  # Look up ontology for each term
  tryCatch({
    term_info <- AnnotationDbi::select(
      GO.db::GO.db,
      keys = all_terms,
      columns = c("GOID", "ONTOLOGY"),
      keytype = "GOID"
    )
    keep_terms <- term_info$GOID[term_info$ONTOLOGY %in% ontology]

    # Filter each gene's terms
    go_cache_out <- lapply(go_cache, function(terms) {
      intersect(terms, keep_terms)
    })

    # Remove genes with no remaining terms
    go_cache_out <- go_cache_out[lengths(go_cache_out) > 0]

    if (getOption("geneselectr.verbose", FALSE)) {
      message(sprintf(
        "  Ontology filter [%s]: %d -> %d terms, %d -> %d annotated genes",
        paste(ontology, collapse = "+"),
        length(all_terms), length(keep_terms),
        length(go_cache), length(go_cache_out)
      ))
    }

    return(go_cache_out)
  }, error = function(e) {
    warning("GO.db ontology lookup failed: ", e$message,
            ". Using all terms.")
    return(go_cache)
  })
}


# =============================================================================
# SCORING FUNCTIONS
# =============================================================================

#' Compute Supervised Scores
#'
#' Scores each gene by its mean-of-top-k semantic similarity to a set of
#' user-specified target GO terms.
#'
#' @param genes Gene symbols
#' @param target_terms Target GO term IDs
#' @param go_cache GO annotations (ontology-filtered)
#' @param ic_scores Information content scores
#' @param similarity_cache Cache environment for similarities
#' @param ancestor_map Named list of GO term -> ancestors
#' @param sim_method Similarity metric name
#' @param n_top_sims Number of top similarities to average
#' @return Numeric vector of similarity scores (one per gene)
#' @keywords internal
compute_supervised_scores <- function(genes, target_terms, go_cache, ic_scores,
                                      similarity_cache, ancestor_map = NULL,
                                      sim_method = "resnik",
                                      n_top_sims = 5) {
  n_genes <- length(genes)
  scores <- numeric(n_genes)

  for (i in seq_len(n_genes)) {
    gene <- genes[i]
    gene_terms <- go_cache[[gene]]

    if (is.null(gene_terms) || length(gene_terms) == 0) {
      scores[i] <- 0
      next
    }

    # Compute all pairwise similarities between gene's terms and targets
    all_sims <- numeric(0)
    for (gene_term in gene_terms) {
      for (target_term in target_terms) {
        sim <- get_or_compute_similarity(
          gene_term, target_term, ic_scores, similarity_cache,
          ancestor_map, sim_method
        )
        all_sims <- c(all_sims, sim)
      }
    }

    if (length(all_sims) == 0) {
      scores[i] <- 0
    } else {
      all_sims <- sort(all_sims, decreasing = TRUE)
      top_k <- min(n_top_sims, length(all_sims))
      scores[i] <- mean(all_sims[1:top_k])
    }
  }

  return(scores)
}

#' Compute data-driven biological relevance scores from GO annotations
#'
#' @param genes Character vector of gene identifiers to score.
#' @param enrichment_genes Optional character vector of genes used to drive term selection.
#' @param go_cache Named list mapping gene -> GO terms.
#' @param ic_scores Named numeric vector of GO term information content.
#' @param similarity_cache Cache used for semantic similarity computation.
#' @param ancestor_map Optional GO ancestor mapping.
#' @param sim_method Semantic similarity method (e.g. "resnik").
#' @param enrich_fdr Adjusted p-value threshold for enrichment.
#' @param min_term_freq Minimum term frequency threshold.
#' @param max_enriched_terms Maximum number of target terms retained.
#' @param n_top_sims Number of top similarities averaged per gene.
#' @param ic_quantile IC quantile threshold for specificity filtering.
#'
#' @return Numeric vector of biological relevance scores.
#'
#' @importFrom stats quantile
#' @keywords internal
#'
compute_data_driven_scores <- function(genes,
                                       enrichment_genes,
                                       go_cache,
                                       ic_scores,
                                       similarity_cache,
                                       ancestor_map = NULL,
                                       sim_method = "resnik",
                                       enrich_fdr = 0.05,
                                       min_term_freq = NULL,
                                       max_enriched_terms = 100,
                                       n_top_sims = 5,
                                       ic_quantile = 0.5) {

  n_genes <- length(genes)

  # Default
  if (is.null(enrichment_genes)) enrichment_genes <- genes

  # Only annotated enrichment genes can drive enrichment
  enrichment_genes <- intersect(enrichment_genes, names(go_cache))
  if (length(enrichment_genes) < 10) {
    warning("Too few annotated enrichment_genes for data-driven mode (<10). Setting b = 0.")
    return(rep(0, n_genes))
  }

  # --- Step 1: IC-based specificity filter (BASED ON enrichment_genes) ---
  all_terms <- unique(unlist(go_cache[enrichment_genes]))
  if (length(all_terms) == 0) {
    warning("No GO annotations found for enrichment_genes after ontology filter. Setting b = 0.")
    return(rep(0, n_genes))
  }

  ic_available <- ic_scores[names(ic_scores) %in% all_terms]
  if (length(ic_available) > 0 && ic_quantile > 0) {
    ic_threshold <- quantile(ic_available, probs = ic_quantile, na.rm = TRUE)
    specific_terms <- names(ic_available[ic_available >= ic_threshold])

    go_cache_specific <- lapply(go_cache, function(terms) intersect(terms, specific_terms))
    go_cache_specific <- go_cache_specific[lengths(go_cache_specific) > 0]
  } else {
    go_cache_specific <- go_cache
  }

  # --- Step 2: Identify candidate enriched terms ---
  annotated_enrich <- intersect(enrichment_genes, names(go_cache_specific))
  if (length(annotated_enrich) < 10) {
    warning("Too few annotated enrichment_genes after specificity filter (<10). Setting b = 0.")
    return(rep(0, n_genes))
  }

  term_counts <- table(unlist(go_cache_specific[annotated_enrich]))
  n_annotated <- length(annotated_enrich)

  effective_min_freq <- if (is.null(min_term_freq)) {
    max(5, ceiling(0.01 * n_annotated))
  } else {
    min_term_freq
  }

  frequent_terms <- names(term_counts[term_counts >= effective_min_freq])

  if (length(frequent_terms) == 0) {
    frequent_terms <- names(sort(term_counts, decreasing = TRUE))[1:min(20, length(term_counts))]
  }

  # --- Step 2b: Fisher enrichment (selected = annotated_enrich) ---
  enriched_terms <- frequent_terms

  all_annotated <- names(go_cache_specific)
  background_genes <- setdiff(all_annotated, annotated_enrich)

  if (length(annotated_enrich) >= 30 && length(background_genes) >= 50) {
    enrich_results <- test_go_enrichment(
      selected_genes = annotated_enrich,
      background_genes = background_genes,
      go_cache = go_cache_specific
    )

    if (nrow(enrich_results) > 0) {
      sig_terms <- enrich_results$term[enrich_results$p_adj < enrich_fdr]
      if (length(sig_terms) >= 5) enriched_terms <- sig_terms
      # else fall back to frequent_terms
    }
  }

  # Cap at max_enriched_terms
  if (length(enriched_terms) > max_enriched_terms) {
    enriched_freqs <- term_counts[enriched_terms]
    enriched_terms <- names(sort(enriched_freqs, decreasing = TRUE))[1:max_enriched_terms]
  }

  if (length(enriched_terms) == 0) return(rep(0, n_genes))

  # --- Step 3: Score EACH gene in `genes` ---
  scores <- numeric(n_genes)

  for (i in seq_len(n_genes)) {
    gene <- genes[i]
    gene_terms <- go_cache_specific[[gene]]

    if (is.null(gene_terms) || length(gene_terms) == 0) {
      scores[i] <- 0
      next
    }

    # Leave-one-out protection still applied per scored gene
    target_for_gene <- setdiff(enriched_terms, gene_terms)

    if (length(target_for_gene) == 0) {
      scores[i] <- min(length(gene_terms) / length(enriched_terms), 0.5)
      next
    }

    all_sims <- numeric(0)
    for (gene_term in gene_terms) {
      for (target_term in target_for_gene) {
        sim <- get_or_compute_similarity(
          gene_term, target_term, ic_scores, similarity_cache,
          ancestor_map, sim_method
        )
        all_sims <- c(all_sims, sim)
      }
    }

    if (length(all_sims) == 0) {
      scores[i] <- 0
    } else {
      all_sims <- sort(all_sims, decreasing = TRUE)
      top_k <- min(n_top_sims, length(all_sims))
      scores[i] <- mean(all_sims[1:top_k])
    }
  }

  return(scores)
}



# =============================================================================
# SEMANTIC SIMILARITY METRICS
# =============================================================================

#' Compute Semantic Similarity Between Two GO Terms
#'
#' Dispatcher that calls the appropriate metric function.
#' All metrics are based on the Most Informative Common Ancestor (MICA):
#'   the shared ancestor of the two terms that has the highest IC.
#'
#' @param term1 First GO term ID
#' @param term2 Second GO term ID
#' @param ic_scores Named vector of Information Content scores
#' @param ancestor_map Named list of GO term -> ancestor vectors
#' @param method One of "resnik", "lin", "jiang", "rel"
#' @return Numeric similarity score between 0 and 1
#' @keywords internal
compute_semantic_similarity <- function(term1, term2, ic_scores,
                                        ancestor_map = NULL,
                                        method = "resnik") {

  # Identical terms have similarity 1.0 under all normalized metrics.
  # (For all four methods: the MICA is the term itself, and the formulas
  # all reduce to 1.0 when IC(MICA) = IC(t1) = IC(t2).)
  if (term1 == term2) return(1.0)

  # --- Get ancestors for both terms ---
  ancestors1 <- get_go_ancestors(term1, ancestor_map)
  ancestors2 <- get_go_ancestors(term2, ancestor_map)

  # --- Find MICA (Most Informative Common Ancestor) ---
  common_ancestors <- intersect(ancestors1, ancestors2)
  if (length(common_ancestors) == 0) return(0.0)

  common_with_ic <- common_ancestors[common_ancestors %in% names(ic_scores)]
  if (length(common_with_ic) == 0) return(0.0)

  mica_ic <- max(ic_scores[common_with_ic], na.rm = TRUE)

  # --- Get IC of the two query terms ---
  ic1 <- ic_scores[term1]
  ic2 <- ic_scores[term2]
  if (is.na(ic1) || is.na(ic2)) return(0.0)
  if (ic1 == 0 && ic2 == 0) return(0.0)

  # --- Dispatch to metric ---
  sim <- switch(method,
                resnik = sim_resnik(mica_ic, ic1, ic2),
                lin    = sim_lin(mica_ic, ic1, ic2),
                jiang  = sim_jiang(mica_ic, ic1, ic2),
                rel    = sim_rel(mica_ic, ic1, ic2),
                stop(sprintf("Unknown similarity method: '%s'", method))
  )

  # Clamp to [0, 1]
  return(max(0, min(sim, 1.0)))
}


#' Resnik Similarity (max-normalized)
#'
#' sim = IC(MICA) / max(IC(t1), IC(t2))
#' Range: (0, 1)
#'
#' @keywords internal
sim_resnik <- function(mica_ic, ic1, ic2) {
  max_ic <- max(ic1, ic2)
  if (is.infinite(max_ic) || max_ic == 0) return(0.0)
  mica_ic / max_ic
}

#' Lin Similarity
#'
#' sim = 2 * IC(MICA) / (IC(t1) + IC(t2))
#' Range: (0, 1)
#'
#' @keywords internal
sim_lin <- function(mica_ic, ic1, ic2) {
  denom <- ic1 + ic2
  if (is.infinite(denom) || denom == 0) return(0.0)
  2 * mica_ic / denom
}

#' Jiang-Conrath Similarity
#'
#' distance = IC(t1) + IC(t2) - 2 * IC(MICA)
#' sim = 1 / (1 + distance)
#' Range: (0, 1)
#'
#' @keywords internal
sim_jiang <- function(mica_ic, ic1, ic2) {
  distance <- ic1 + ic2 - 2 * mica_ic
  distance <- max(0, distance)  # Clamp for numerical safety
  1 / (1 + distance)
}

#' Relevance Similarity (Schlicker et al.)
#'
#' sim = Lin(t1, t2) * (1 - p(MICA))
#' where p(MICA) = exp(-IC(MICA)) is the annotation probability.
#' Penalizes broad common ancestors.
#' Range: (0, 1).
#'
#' @keywords internal
sim_rel <- function(mica_ic, ic1, ic2) {
  lin_val <- sim_lin(mica_ic, ic1, ic2)
  p_mica <- exp(-mica_ic)
  lin_val * (1 - p_mica)
}


# =============================================================================
# GO DAG TRAVERSAL
# =============================================================================

#' Get GO Ancestors
#'
#' Retrieves all ancestor terms of a GO term by traversing the GO DAG.
#' Uses a pre-built ancestor map if available, otherwise falls back to GO.db.
#'
#' @param term GO term ID (e.g., "GO:0006955")
#' @param ancestor_map Named list mapping GO terms to their ancestor vectors.
#' @importFrom AnnotationDbi select
#' @importFrom AnnotationDbi as.list
#' @importFrom GO.db GOBPANCESTOR
#' @importFrom GO.db GOMFANCESTOR
#' @importFrom GO.db GOCCANCESTOR
#' @return Character vector of ancestor terms (always includes the term itself)
#' @keywords internal
get_go_ancestors <- function(term, ancestor_map = NULL) {

  # 1. Use pre-built ancestor map if available (fastest)
  if (!is.null(ancestor_map) && term %in% names(ancestor_map)) {
    ancestors <- ancestor_map[[term]]
    return(unique(c(term, ancestors)))
  }

  # 2. Fall back to GO.db if installed
  if (requireNamespace("GO.db", quietly = TRUE) &&
      requireNamespace("AnnotationDbi", quietly = TRUE)) {

    tryCatch({
      ontology <- tryCatch({
        AnnotationDbi::select(GO.db::GO.db, keys = term,
                              columns = "ONTOLOGY", keytype = "GOID")$ONTOLOGY
      }, error = function(e) NA)

      if (is.na(ontology)) return(term)

      ancestor_env <- switch(ontology,
                             "BP" = if (exists("GOBPANCESTOR", where = "package:GO.db"))
                               AnnotationDbi::as.list(GO.db::GOBPANCESTOR) else NULL,
                             "MF" = if (exists("GOMFANCESTOR", where = "package:GO.db"))
                               AnnotationDbi::as.list(GO.db::GOMFANCESTOR) else NULL,
                             "CC" = if (exists("GOCCANCESTOR", where = "package:GO.db"))
                               AnnotationDbi::as.list(GO.db::GOCCANCESTOR) else NULL,
                             NULL
      )

      if (!is.null(ancestor_env) && term %in% names(ancestor_env)) {
        ancestors <- ancestor_env[[term]]
        ancestors <- ancestors[ancestors != "all"]
        return(unique(c(term, ancestors)))
      }
    }, error = function(e) {
      # Fall through to default
    })
  }

  # 3. Last resort: return just the term (similarity only for identical terms)
  return(term)
}


#' Build Ancestor Map from GO.db
#'
#' Pre-computes ancestor relationships for all GO terms to avoid repeated
#' lookups. Results are cached to disk.
#'
#' @param organism Character, organism name
#' @param use_cache Logical, use disk caching
#' @importFrom AnnotationDbi as.list
#' @return Named list mapping each GO term to its ancestor terms
#' @keywords internal
load_ancestor_map <- function(organism = "human", use_cache = TRUE) {

  cache_key <- "go_ancestor_map"

  if (use_cache && exists(cache_key, envir = .geneselectr_cache)) {
    return(get(cache_key, envir = .geneselectr_cache))
  }

  if (use_cache) {
    cache_file <- file.path(get_cache_dir(), paste0(cache_key, ".rds"))
    if (file.exists(cache_file)) {
      ancestor_map <- readRDS(cache_file)
      assign(cache_key, ancestor_map, envir = .geneselectr_cache)
      return(ancestor_map)
    }
  }

  ancestor_map <- list()

  if (requireNamespace("GO.db", quietly = TRUE) &&
      requireNamespace("AnnotationDbi", quietly = TRUE)) {

    if (getOption("geneselectr.verbose", TRUE)) {
      message("Building GO ancestor map from GO.db (one-time operation)...")
    }

    for (ont_name in c("GOBPANCESTOR", "GOMFANCESTOR", "GOCCANCESTOR")) {
      tryCatch({
        ont_data <- AnnotationDbi::as.list(
          get(ont_name, envir = asNamespace("GO.db"))
        )
        for (term in names(ont_data)) {
          ancestors <- ont_data[[term]]
          ancestors <- ancestors[ancestors != "all"]
          ancestor_map[[term]] <- ancestors
        }
      }, error = function(e) {
        warning("Could not load ", ont_name, ": ", e$message)
      })
    }

    if (getOption("geneselectr.verbose", TRUE)) {
      message(sprintf("  Loaded ancestors for %d GO terms", length(ancestor_map)))
    }
  } else {
    warning(
      "GO.db package not available. Biological scoring will be limited. ",
      "Install with: BiocManager::install('GO.db')"
    )
  }

  if (use_cache && length(ancestor_map) > 0) {
    cache_file <- file.path(get_cache_dir(), paste0(cache_key, ".rds"))
    saveRDS(ancestor_map, cache_file)
    assign(cache_key, ancestor_map, envir = .geneselectr_cache)
  }

  return(ancestor_map)
}


# =============================================================================
# INFORMATION CONTENT AND ENRICHMENT
# =============================================================================

#' Compute Information Content
#'
#' IC(term) = -log( (count(term) + 1) / (n_genes + 1) )
#' Laplace-smoothed to avoid log(0).
#'
#' @param go_cache GO annotations (gene -> terms), already ontology-filtered
#' @return Named numeric vector of IC scores
#' @keywords internal
compute_information_content <- function(go_cache) {
  all_terms <- unique(unlist(go_cache))

  if (length(all_terms) == 0) {
    return(numeric(0))
  }

  term_counts <- table(unlist(go_cache))
  n_genes <- length(go_cache)

  ic_scores <- -log((term_counts + 1) / (n_genes + 1))

  return(ic_scores)
}


#' Test GO Enrichment via Fisher's Exact Test
#'
#' For each GO term annotated to any selected gene, tests whether the term
#' is over-represented in the selected set vs the background.
#'
#' @param selected_genes Character vector of selected gene names
#' @param background_genes Character vector of background gene names
#' @param go_cache GO annotations
#' @importFrom stats fisher.test
#' @importFrom stats p.adjust
#' @return Data frame with columns: term, p_value, odds_ratio, n_selected,
#'   n_background, p_adj (BH-adjusted)
#' @keywords internal
test_go_enrichment <- function(selected_genes, background_genes, go_cache) {

  selected_terms <- unique(unlist(go_cache[selected_genes]))

  if (length(selected_terms) == 0) {
    return(data.frame(
      term = character(0),
      p_value = numeric(0),
      odds_ratio = numeric(0),
      n_selected = integer(0),
      n_background = integer(0),
      p_adj = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  n_sel <- length(selected_genes)
  n_bg <- length(background_genes)

  # Use vapply instead of sapply for type safety
  results <- lapply(selected_terms, function(term) {
    n_sel_with <- sum(vapply(go_cache[selected_genes],
                             function(x) term %in% x, logical(1)))
    n_bg_with <- sum(vapply(go_cache[background_genes],
                            function(x) term %in% x, logical(1)))

    contingency <- matrix(c(
      n_sel_with, n_sel - n_sel_with,
      n_bg_with,  n_bg - n_bg_with
    ), nrow = 2)

    test_result <- fisher.test(contingency, alternative = "greater")

    data.frame(
      term = term,
      p_value = test_result$p.value,
      odds_ratio = as.numeric(test_result$estimate),
      n_selected = n_sel_with,
      n_background = n_bg_with,
      stringsAsFactors = FALSE
    )
  })

  results_df <- do.call(rbind, results)
  results_df$p_adj <- p.adjust(results_df$p_value, method = "BH")
  results_df <- results_df[order(results_df$p_value), ]

  return(results_df)
}
