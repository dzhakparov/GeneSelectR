# =============================================================================
# test-cache.R — Tests for GeneSelectR caching system
# =============================================================================

# ---------------------------------------------------------------------------
# get_cache_dir
# ---------------------------------------------------------------------------
test_that("get_cache_dir returns a valid path under R_user_dir", {
  dir <- get_cache_dir()
  expect_type(dir, "character")
  expect_true(grepl("GeneSelectR", dir))
})

test_that("get_cache_dir creates the directory if missing", {
  withr::with_envvar(
    c(R_USER_CACHE_DIR = tempdir()),
    {
      dir <- get_cache_dir()
      expect_true(dir.exists(dir))
    }
  )
})

# ---------------------------------------------------------------------------
# .geneselectr_cache environment
# ---------------------------------------------------------------------------
test_that(".geneselectr_cache is an environment with empty parent", {
  expect_true(is.environment(.geneselectr_cache))
  expect_identical(parent.env(.geneselectr_cache), emptyenv())
})

# ---------------------------------------------------------------------------
# set_cache_options
# ---------------------------------------------------------------------------
test_that("set_cache_options sets verbose option", {
  old <- getOption("geneselectr.verbose")
  on.exit(options(geneselectr.verbose = old))

  set_cache_options(verbose = FALSE)
  expect_false(getOption("geneselectr.verbose"))

  set_cache_options(verbose = TRUE)
  expect_true(getOption("geneselectr.verbose"))
})

test_that("set_cache_options warns about unimplemented max_cache_size", {
  expect_warning(set_cache_options(max_cache_size = 100), "not yet implemented")
})

# ---------------------------------------------------------------------------
# create_similarity_cache
# ---------------------------------------------------------------------------
test_that("create_similarity_cache returns clean environment", {
  cache <- create_similarity_cache()
  expect_true(is.environment(cache))
  expect_length(ls(envir = cache), 0)
})

# ---------------------------------------------------------------------------
# load_go_cache — memory cache round-trip
# ---------------------------------------------------------------------------
test_that("load_go_cache memory cache stores and retrieves", {
  test_data <- list(TP53 = c("GO:0006915"), MYC = c("GO:0008283"))

  cache_key <- "go_test_organism"
  assign(cache_key, test_data, envir = .geneselectr_cache)
  on.exit(rm(list = cache_key, envir = .geneselectr_cache), add = TRUE)

  cached <- get(cache_key, envir = .geneselectr_cache)
  expect_equal(cached, test_data)
})

# ---------------------------------------------------------------------------
# load_ic_cache — round-trip
# ---------------------------------------------------------------------------
test_that("load_ic_cache computes and returns IC scores from GO cache", {
  gc <- list(
    GENE_A = c("GO:0001", "GO:0002"),
    GENE_B = c("GO:0001", "GO:0003"),
    GENE_C = c("GO:0002", "GO:0003", "GO:0004")
  )

  if (exists("ic_scores", envir = .geneselectr_cache)) {
    rm("ic_scores", envir = .geneselectr_cache)
  }

  withr::with_tempdir({
    withr::with_options(list(geneselectr.verbose = FALSE), {
      ic <- load_ic_cache(gc, force_reload = TRUE)
    })
  })

  expect_type(ic, "double")
  expect_true(all(ic > 0))
  expect_gt(ic["GO:0004"], ic["GO:0001"])
})

# ---------------------------------------------------------------------------
# clear_cache
# ---------------------------------------------------------------------------
test_that("clear_cache clears memory cache", {
  assign("test_key", "test_value", envir = .geneselectr_cache)
  expect_true(exists("test_key", envir = .geneselectr_cache))

  suppressMessages(clear_cache(confirm = FALSE))
  expect_false(exists("test_key", envir = .geneselectr_cache))
})

# ---------------------------------------------------------------------------
# cache_info
# ---------------------------------------------------------------------------
test_that("cache_info runs without error", {
  expect_output(cache_info(), "GeneSelectR Cache")
})

# ---------------------------------------------------------------------------
# download_go_annotations — unsupported organism
# ---------------------------------------------------------------------------
test_that("download_go_annotations warns for unsupported organism", {
  expect_warning(
    result <- download_go_annotations("zebrafish"),
    "Unsupported organism"
  )
  expect_equal(result, list())
})
