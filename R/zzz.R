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
  sklearn <- reticulate::import("sklearn", delay_load = TRUE)
  pandas <- reticulate::import("pandas", delay_load = TRUE)
  numpy <- reticulate::import("numpy", delay_load = TRUE)
  boruta <- reticulate::import("boruta", delay_load = TRUE)
  sys <- reticulate::import("sys", delay_load = TRUE)
  multiprocessing <- reticulate::import("multiprocessing", delay_load = TRUE)
  skopt <- reticulate::import('skopt', delay_load = TRUE)

  #message("Imported Python libraries with delay_load = TRUE")
}

#' @title Package Load Function
#'
#' @description
#' This function is called when the package is loaded. It checks if Conda is installed and calls `load_python_packages` to import Python modules.
#'
#' @param libname The name of the library.
#' @param pkgname The name of the package.
#'
#' @details
#' If Conda is not installed, the function will stop and prompt the user to install Conda. If Conda is installed, it will proceed to import Python modules.
#'
#' @return
#' If not in interactive mode, the function will return NULL. Otherwise, it returns a message indicating that the import was successful.
#'
#' @name onLoad
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  load_python_packages()
}

#'@importFrom utils install.packages menu
.onAttach <- function(libname, pkgname) {
  bioconductor_packages <- c("clusterProfiler", "GO.db", "simplifyEnrichment")

  missing_packages <- bioconductor_packages[!sapply(bioconductor_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    packageStartupMessage("The following Bioconductor packages are required for full functionality of ",
            pkgname, ": ", paste(missing_packages, collapse = ", "), ".")

    if (interactive()) {
      response <- menu(c("Yes", "No"), title = "Do you want to install them now?")
      if (response == 1) {
        if (!requireNamespace("BiocManager", quietly = TRUE)) {
          install.packages("BiocManager")
        }
        BiocManager::install(missing_packages)
      }
    } else {
      packageStartupMessage("Run BiocManager::install(c(", paste(sQuote(missing_packages), collapse = ", "), ")) to install missing packages.")
    }
  }
}







