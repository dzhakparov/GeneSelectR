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
#' This function is called when the package is attached. It checks for the availability of essential Bioconductor packages.
#'
#' @param libname The name of the library.
#' @param pkgname The name of the package.
#'
#' @details
#' The function verifies the presence of specific Bioconductor packages required for the package's full functionality. If any of these packages are missing, the function will stop and provide a message to the user to install the missing packages using `BiocManager::install()`.
#'
#' If essential Bioconductor packages are missing, the function halts the package loading process and informs the user about the missing packages, recommending their installation via `BiocManager::install()`.
#'
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  bioconductor_packages <- c("clusterProfiler", "GO.db", "simplifyEnrichment")

  missing_packages <- bioconductor_packages[!sapply(bioconductor_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop("The following Bioconductor packages are required for full functionality of ",
         pkgname, " but are not installed: ", paste(missing_packages, collapse = ", "),
         ". Please install them using BiocManager::install().", call. = FALSE)
  }
}








