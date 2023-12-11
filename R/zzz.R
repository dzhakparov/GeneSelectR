#' @title Global references to Python modules
#'
#' @description
#' These are global references to Python modules that will be initialized in .onLoad.
#'
#' @details
#' The following Python modules are referenced:
#' \itemize{
#' \item sklearn
#' \item pandas
#' \item numpy
#' \item lightgbm
#' \item xgboost
#' \item boruta
#' \item sys
#' \item multiprocessing
#' }
#'
#' @name python-modules
#' @keywords internal
sklearn <- NULL
pandas <- NULL
numpy <- NULL
boruta <- NULL
sys <- NULL
multiprocessing <- NULL
skopt <- NULL

#' @title Load Python Modules
#'
#' @description
#' This function imports necessary Python modules with delayed loading.
#'
#' @details
#' Imports the following Python modules with `delay_load` set to TRUE:
#' \itemize{
#'   \item sklearn
#'   \item pandas
#'   \item numpy
#'   \item boruta
#'   \item sys
#'   \item multiprocessing
#'   \item skopt
#' }
#'
#' @return
#' If successful, the global variables for each Python module are set.
#' Otherwise, it will return an error message.
#'
#' @keywords internal
load_python_packages <- function() {
  # Import the Python modules with delay_load = TRUE
  modules <- list(
    sklearn = reticulate::import("sklearn", delay_load = TRUE),
    pandas = reticulate::import("pandas", delay_load = TRUE),
    numpy = reticulate::import("numpy", delay_load = TRUE),
    boruta = reticulate::import("boruta", delay_load = TRUE),
    sys = reticulate::import("sys", delay_load = TRUE),
    multiprocessing = reticulate::import("multiprocessing", delay_load = TRUE),
    skopt = reticulate::import('skopt', delay_load = TRUE)
  )
  return(modules)
  #message("Imported Python libraries with delay_load = TRUE")
}

#' @title Package Attachment Function
#'
#' @description
#' This function is called when the package is attached. It checks for the availability of essential Bioconductor packages and alerts if any are missing.
#'
#' @param libname The name of the library.
#' @param pkgname The name of the package.
#'
#' @details
#' The function checks for the presence of specific Bioconductor packages that are essential for the package's functionality. If any required packages are missing, it displays a startup message advising the user to install the missing packages using `BiocManager::install()`.
#'
#' Instead of stopping the package loading process, it alerts the user about any missing dependencies, recommending their installation for full functionality.
#'
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  bioconductor_packages <- c("clusterProfiler", "GO.db", "simplifyEnrichment")

  missing_packages <- bioconductor_packages[!sapply(bioconductor_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    packageStartupMessage("Warning: The following Bioconductor packages are required for full functionality of ",
                          pkgname, " but are not installed: ", paste(missing_packages, collapse = ", "),
                          ". Please install them using BiocManager::install().")
  }
}










