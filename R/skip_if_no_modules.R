#' @title Check if Python Modules are Available
#' @description This helper function checks if a list of Python modules are available. If any are not, it skips the tests.
#' @param module_names A vector of names of the Python modules to check.
#' @return Nothing is returned explicitly, but if a specified module is not available, the function invokes `testthat::skip` to skip the tests that require that module.
#' @importFrom reticulate py_module_available
#' @importFrom testthat skip
#' @examples
#' \donttest{
#' # Example usage within a test file:
#' module_names <- c("numpy", "pandas", "sklearn")
#' skip_if_no_modules(module_names)
#'}
#'
#' @export
skip_if_no_modules <- function(module_names) {
  lapply(module_names, function(module_name) {
    have_module <- reticulate::py_module_available(module_name)
    if (!have_module)
      testthat::skip(paste0(module_name, " not available for testing"))
  })
}


#' Check Python Module Availability for Examples
#'
#' Checks if the specified Python modules are available and returns TRUE if all are available,
#' otherwise returns FALSE.
#'
#' @param module_names Character vector of Python module names to check.
#' @return Logical TRUE if all modules are available, FALSE otherwise.
#' @importFrom reticulate py_module_available
check_python_modules_available <- function(module_names) {
  requireNamespace("reticulate", quietly = TRUE)
  all(sapply(module_names, reticulate::py_module_available))
}
