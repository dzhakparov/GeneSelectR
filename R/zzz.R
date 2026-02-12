#' Package Startup Message
#'
#' @param libname Library name
#' @param pkgname Package name
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n",
    "===================================================\n",
    "  GeneSelectR 2.0 - Gene Selection with Biology  \n",
    "===================================================\n",
    "\n",
    "Quick start:\n",
    "  result <- geneselectr2_fit(X, y)\n",
    "\n",
    "Help:\n",
    "  ?geneselectr2_fit\n",
    "  vignette('introduction', package = 'GeneSelectR')\n",
    "\n",
    "Caching enabled by default for faster runs!\n",
    "  - cache_info() to view cache\n",
    "  - clear_cache() to reset\n",
    "\n"
  )
}

#' Package Load Hook
#'
#' @param libname Library name
#' @param pkgname Package name
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Set package options
  op <- options()
  op.geneselectr <- list(
    geneselectr.verbose = TRUE
  )
  toset <- !(names(op.geneselectr) %in% names(op))
  if (any(toset)) options(op.geneselectr[toset])

  invisible()
}
