#' @title Check if Python Modules are Available
#' @description This helper function checks if a list of Python modules are available. If any are not, it skips the tests.
#' @param module_names A vector of names of the Python modules to check.
#' @return Nothing is returned explicitly, but if a specified module is not available, the function invokes `testthat::skip` to skip the tests that require that module.
#' @importFrom reticulate py_module_available
#' @importFrom testthat skip
#' @export
skip_if_no_modules <- function(module_names) {
  lapply(module_names, function(module_name) {
    have_module <- reticulate::py_module_available(module_name)
    if (!have_module)
      testthat::skip(paste0(module_name, " not available for testing"))
  })
}
