# =============================================================================
# test-biological_scorer.R — Tests for GO-based biological relevance scoring
# =============================================================================

# ---------------------------------------------------------------------------
# Helpers: synthetic GO cache and IC scores for unit tests
# ---------------------------------------------------------------------------

# Minimal fake GO cache (no Bioconductor dependency)
mock_go_cache <- function() {
  list(
    TP53  = c("GO:0006915", "GO:0008283", "GO:0006281"),
    BRCA1 = c("GO:0006281", "GO:0006974", "GO:0008283"),
    EGFR  = c("GO:0007169", "GO:0008283", "GO:0042127"),
    MYC   = c("GO:0008283", "GO:0006355"),
    ACTB  = c("GO:0007015", "GO:0030832"),
    GAPDH = c("GO:0006006", "GO:0006096"),
    IL6   = c("GO:0006955", "GO:0006954"),
    TNF   = c("GO:0006955", "GO:0006915", "GO:0006954"),
    CD8A  = c("GO:0006955", "GO:0042110"),
    FOXP3 = c("GO:0006955", "GO:0045066")
  )
}

# Mock IC scores keyed by GO term
mock_ic_scores <- function(go_cache, ancestor_map = NULL) {
  all_terms <- unique(unlist(go_cache))
  # Assign reproducible IC values based on term frequency
  term_counts <- table(unlist(go_cache))
  n_genes <- length(go_cache)
  ic <- -log((term_counts + 1) / (n_genes + 1))

  # Also assign IC to ancestor-only terms (shared parents in the mock DAG)
  # so that MICA lookups succeed
  if (!is.null(ancestor_map)) {
    anc_terms <- unique(unlist(ancestor_map))
    missing <- setdiff(anc_terms, names(ic))
    if (length(missing) > 0) {
      # Give ancestors a low IC (they are broad / common)
      broad_ic <- setNames(rep(0.5, length(missing)), missing)
      ic <- c(ic, broad_ic)
    }
  }

  return(ic)
}

# Simple ancestor map: each term is its own ancestor (no DAG traversal)
mock_ancestor_map <- function(go_cache) {
  all_terms <- unique(unlist(go_cache))
  amap <- setNames(lapply(all_terms, function(t) t), all_terms)
  # Add a shared root for immune terms to allow non-trivial MICA
  immune_terms <- c("GO:0006955", "GO:0006954", "GO:0042110", "GO:0045066")
  for (t in immune_terms) {
    amap[[t]] <- c(t, "GO:0002376")
  }
  # DNA repair terms share a parent
  dna_terms <- c("GO:0006281", "GO:0006974")
  for (t in dna_terms) {
    amap[[t]] <- c(t, "GO:0006259")
  }
  # Cell proliferation terms
  prolif_terms <- c("GO:0008283", "GO:0042127")
  for (t in prolif_terms) {
    amap[[t]] <- c(t, "GO:0008283")
  }
  return(amap)
}


# ---------------------------------------------------------------------------
# compute_information_content
# ---------------------------------------------------------------------------
test_that("compute_information_content returns named numeric vector", {
  gc <- mock_go_cache()
  ic <- compute_information_content(gc)

  expect_type(ic, "double")
  expect_true(all(ic > 0))
  expect_true(length(ic) == length(unique(unlist(gc))))
})

test_that("IC is higher for rare terms than common terms", {
  gc <- mock_go_cache()
  ic <- compute_information_content(gc)

  # GO:0008283 (cell proliferation) appears in 4 genes — should have lower IC

  # GO:0007015 (actin filament) appears in 1 gene — should have higher IC
  expect_gt(ic["GO:0007015"], ic["GO:0008283"])
})

test_that("compute_information_content handles empty cache", {
  ic <- compute_information_content(list())
  expect_length(ic, 0)
})


# ---------------------------------------------------------------------------
# Semantic similarity metrics
# ---------------------------------------------------------------------------
test_that("sim_resnik returns value in [0, 1]", {
  s <- sim_resnik(mica_ic = 2.0, ic1 = 3.0, ic2 = 2.5)
  expect_gte(s, 0)
  expect_lte(s, 1)
})

test_that("sim_resnik equals 1 when MICA IC equals max term IC", {
  s <- sim_resnik(mica_ic = 4.0, ic1 = 4.0, ic2 = 3.0)
  expect_equal(s, 1.0)
})

test_that("sim_resnik handles zero IC gracefully", {
  s <- sim_resnik(mica_ic = 0, ic1 = 0, ic2 = 0)
  expect_equal(s, 0)
})

test_that("sim_lin returns value in [0, 1]", {
  s <- sim_lin(mica_ic = 2.0, ic1 = 3.0, ic2 = 2.5)
  expect_gte(s, 0)
  expect_lte(s, 1)
})

test_that("sim_lin equals 1 when terms are identical (MICA = both)", {
  s <- sim_lin(mica_ic = 3.0, ic1 = 3.0, ic2 = 3.0)
  expect_equal(s, 1.0)
})

test_that("sim_jiang returns value in (0, 1]", {
  s <- sim_jiang(mica_ic = 2.0, ic1 = 3.0, ic2 = 2.5)
  expect_gt(s, 0)
  expect_lte(s, 1)
})

test_that("sim_jiang equals 1 when distance is 0", {
  # distance = ic1 + ic2 - 2*mica = 3+3-6 = 0
  s <- sim_jiang(mica_ic = 3.0, ic1 = 3.0, ic2 = 3.0)
  expect_equal(s, 1.0)
})

test_that("sim_rel penalises shallow common ancestors", {
  # Shallow MICA → p(MICA) is high → penalty is large
  shallow_mica_ic <- 0.5  # exp(-0.5) ≈ 0.61
  deep_mica_ic    <- 3.0  # exp(-3.0) ≈ 0.05

  s_shallow <- sim_rel(shallow_mica_ic, ic1 = 3.0, ic2 = 3.0)
  s_deep    <- sim_rel(deep_mica_ic, ic1 = 3.0, ic2 = 3.0)

  expect_gt(s_deep, s_shallow)
})


# ---------------------------------------------------------------------------
# compute_semantic_similarity (dispatcher)
# ---------------------------------------------------------------------------
test_that("compute_semantic_similarity returns 1.0 for identical terms", {
  gc <- mock_go_cache()
  amap <- mock_ancestor_map(gc)
  ic <- mock_ic_scores(gc, amap)

  for (method in c("resnik", "lin", "jiang", "rel")) {
    s <- compute_semantic_similarity("GO:0006955", "GO:0006955", ic, amap, method)
    expect_equal(s, 1.0, info = paste("method:", method))
  }
})

test_that("compute_semantic_similarity returns 0 for unrelated terms with no common ancestor", {
  gc <- mock_go_cache()
  amap <- mock_ancestor_map(gc)
  ic <- mock_ic_scores(gc, amap)

  # GO:0007015 (actin) and GO:0006006 (glucose) share no ancestor in our mock
  s <- compute_semantic_similarity("GO:0007015", "GO:0006006", ic, amap, "resnik")
  expect_equal(s, 0)
})

test_that("compute_semantic_similarity returns >0 for related terms", {
  gc <- mock_go_cache()
  amap <- mock_ancestor_map(gc)
  ic <- mock_ic_scores(gc, amap)

  # Both immune terms share GO:0002376 as ancestor
  s <- compute_semantic_similarity("GO:0006955", "GO:0006954", ic, amap, "resnik")
  expect_gt(s, 0)
})

test_that("unknown similarity method raises error", {
  gc <- mock_go_cache()
  amap <- mock_ancestor_map(gc)
  ic <- mock_ic_scores(gc, amap)
  # Must pass ancestor_map so terms share a common ancestor and code
  # reaches the switch() dispatcher instead of returning 0 early
  expect_error(
    compute_semantic_similarity("GO:0006955", "GO:0006954", ic, amap, method = "cosine"),
    "Unknown similarity"
  )
})


# ---------------------------------------------------------------------------
# get_go_ancestors
# ---------------------------------------------------------------------------
test_that("get_go_ancestors returns term itself at minimum", {
  anc <- get_go_ancestors("GO:0006955", ancestor_map = NULL)
  expect_true("GO:0006955" %in% anc)
})

test_that("get_go_ancestors uses ancestor_map when provided", {
  amap <- list("GO:0006955" = c("GO:0002376", "GO:0008150"))
  anc <- get_go_ancestors("GO:0006955", ancestor_map = amap)
  expect_true("GO:0002376" %in% anc)
  expect_true("GO:0006955" %in% anc)
})


# ---------------------------------------------------------------------------
# test_go_enrichment
# ---------------------------------------------------------------------------
test_that("test_go_enrichment returns correct structure", {
  gc <- mock_go_cache()

  selected <- c("IL6", "TNF", "CD8A", "FOXP3")
  background <- c("TP53", "BRCA1", "EGFR", "MYC", "ACTB", "GAPDH")

  result <- test_go_enrichment(selected, background, gc)

  expect_true(is.data.frame(result))
  expect_true(all(c("term", "p_value", "odds_ratio", "n_selected",
                    "n_background", "p_adj") %in% names(result)))
})

test_that("test_go_enrichment finds immune terms enriched in immune gene set", {
  gc <- mock_go_cache()

  selected <- c("IL6", "TNF", "CD8A", "FOXP3")
  background <- c("TP53", "BRCA1", "EGFR", "MYC", "ACTB", "GAPDH")

  result <- test_go_enrichment(selected, background, gc)

  # GO:0006955 (immune response) should be highly enriched
  immune_row <- result[result$term == "GO:0006955", ]
  expect_true(nrow(immune_row) > 0)
  expect_lt(immune_row$p_value, 0.05)
  expect_equal(immune_row$n_selected, 4)  # all 4 selected genes have this term
})

test_that("test_go_enrichment handles empty selected terms", {
  gc <- list(
    GENE1 = character(0),
    GENE2 = c("GO:0000001")
  )
  result <- test_go_enrichment("GENE1", "GENE2", gc)
  expect_equal(nrow(result), 0)
})


# ---------------------------------------------------------------------------
# compute_supervised_scores
# ---------------------------------------------------------------------------
test_that("compute_supervised_scores returns one score per gene", {
  gc <- mock_go_cache()
  amap <- mock_ancestor_map(gc)
  ic <- mock_ic_scores(gc, amap)
  sim_cache <- create_similarity_cache()

  genes <- names(gc)
  targets <- c("GO:0006955")  # immune response

  scores <- compute_supervised_scores(
    genes, targets, gc, ic, sim_cache, amap,
    sim_method = "resnik", n_top_sims = 3
  )

  expect_length(scores, length(genes))
  expect_true(all(scores >= 0))
})

test_that("supervised scores are higher for genes annotated to target terms", {
  gc <- mock_go_cache()
  amap <- mock_ancestor_map(gc)
  ic <- mock_ic_scores(gc, amap)
  sim_cache <- create_similarity_cache()

  genes <- c("IL6", "TNF", "ACTB", "GAPDH")
  targets <- c("GO:0006955")

  scores <- compute_supervised_scores(
    genes, targets, gc, ic, sim_cache, amap,
    sim_method = "resnik", n_top_sims = 3
  )

  # IL6 and TNF are annotated to GO:0006955, ACTB and GAPDH are not
  expect_gt(scores[1], scores[3])  # IL6 > ACTB
  expect_gt(scores[2], scores[4])  # TNF > GAPDH
})


# ---------------------------------------------------------------------------
# compute_data_driven_scores
# ---------------------------------------------------------------------------
test_that("compute_data_driven_scores returns one score per gene", {
  gc <- mock_go_cache()
  amap <- mock_ancestor_map(gc)
  ic <- mock_ic_scores(gc, amap)
  sim_cache <- create_similarity_cache()

  genes <- names(gc)
  enrich_genes <- c("IL6", "TNF", "CD8A", "FOXP3", "TP53", "BRCA1",
                    "EGFR", "MYC", "ACTB", "GAPDH")

  scores <- compute_data_driven_scores(
    genes = genes,
    enrichment_genes = enrich_genes,
    go_cache = gc,
    ic_scores = ic,
    similarity_cache = sim_cache,
    ancestor_map = amap,
    sim_method = "resnik",
    enrich_fdr = 0.2,
    max_enriched_terms = 50,
    n_top_sims = 3,
    ic_quantile = 0.0  # no IC filter for this small example
  )

  expect_length(scores, length(genes))
  expect_true(all(scores >= 0))
})

test_that("data_driven_scores returns zeros when too few annotated genes", {
  gc <- list(GENE1 = c("GO:0000001"))  # only 1 annotated gene
  ic <- compute_information_content(gc)
  sim_cache <- create_similarity_cache()

  scores <- compute_data_driven_scores(
    genes = "GENE1",
    enrichment_genes = "GENE1",
    go_cache = gc,
    ic_scores = ic,
    similarity_cache = sim_cache
  )

  expect_equal(scores, 0)
})


# ---------------------------------------------------------------------------
# biological_scorer (top-level API)
# ---------------------------------------------------------------------------
test_that("biological_scorer returns neutral scores when no annotations found", {
  # Genes not in any cache
  genes <- c("FAKE1", "FAKE2", "FAKE3")
  gc <- mock_go_cache()

  scores <- suppressWarnings(
    biological_scorer(genes, mode = "supervised",
                      target_terms = "GO:0006955",
                      go_cache = gc)
  )

  expect_length(scores, 3)
  expect_true(all(scores == 1))
})

test_that("biological_scorer supervised mode requires target_terms", {
  gc <- mock_go_cache()
  expect_error(
    biological_scorer(names(gc), mode = "supervised",
                      target_terms = NULL, go_cache = gc),
    "target_terms required"
  )
})

test_that("biological_scorer rejects invalid mode", {
  gc <- mock_go_cache()
  expect_error(
    biological_scorer(names(gc), mode = "auto", go_cache = gc),
    "must be"
  )
})

test_that("biological_scorer validates numeric parameters", {
  gc <- mock_go_cache()
  expect_error(
    biological_scorer(names(gc), mode = "data_driven",
                      enrich_fdr = -0.1, go_cache = gc)
  )
  expect_error(
    biological_scorer(names(gc), mode = "data_driven",
                      n_top_sims = 0, go_cache = gc)
  )
  expect_error(
    biological_scorer(names(gc), mode = "data_driven",
                      ic_quantile = 1.5, go_cache = gc)
  )
})

test_that("biological_scorer supervised mode returns percentile-normalised scores", {
  gc <- mock_go_cache()
  genes <- names(gc)

  scores <- biological_scorer(
    genes, mode = "supervised",
    target_terms = c("GO:0006955", "GO:0006954"),
    go_cache = gc,
    use_cache = FALSE
  )

  expect_length(scores, length(genes))
  expect_true(all(scores >= 0 & scores <= 1))
})


# ---------------------------------------------------------------------------
# Similarity cache
# ---------------------------------------------------------------------------
test_that("get_or_compute_similarity caches results", {
  gc <- mock_go_cache()
  amap <- mock_ancestor_map(gc)
  ic <- mock_ic_scores(gc, amap)
  cache <- create_similarity_cache()

  s1 <- get_or_compute_similarity("GO:0006955", "GO:0006954", ic, cache, amap, "resnik")
  s2 <- get_or_compute_similarity("GO:0006955", "GO:0006954", ic, cache, amap, "resnik")
  expect_equal(s1, s2)

  # Cache key includes method — different method should give different result
  s3 <- get_or_compute_similarity("GO:0006955", "GO:0006954", ic, cache, amap, "lin")
  # s3 may or may not equal s1, but both are valid
  expect_true(is.numeric(s3))
})

test_that("similarity cache is symmetric", {
  gc <- mock_go_cache()
  amap <- mock_ancestor_map(gc)
  ic <- mock_ic_scores(gc, amap)
  cache <- create_similarity_cache()

  s_ab <- get_or_compute_similarity("GO:0006955", "GO:0006954", ic, cache, amap, "lin")
  s_ba <- get_or_compute_similarity("GO:0006954", "GO:0006955", ic, cache, amap, "lin")
  expect_equal(s_ab, s_ba)
})
