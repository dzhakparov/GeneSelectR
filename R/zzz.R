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
xgboost <- NULL
boruta <- NULL
sys <- NULL
multiprocessing <- NULL

#' @title Load Python Modules
#'
#' @description
#' This function is called when the package is loaded. It checks if Conda is installed and imports necessary Python modules.
#'
#' @param libname The name of the library.
#' @param pkgname The name of the package.
#'
#' @details
#' If Conda is not installed, the function will stop and prompt the user to install Conda. If Conda is installed, the function will import the Python modules with delay_load set to TRUE.
#'
#' @return
#' If not in interactive mode, the function will return NULL. Otherwise, it will import the Python modules and return a message indicating that the import was successful.
#'
#' @name onLoad
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Check if Conda is installed
  if (is.null(reticulate::conda_version())) {
    stop("Conda is not installed on your system. Please install Conda before proceeding.")
  } else {
    message("Conda is installed.")
  }

  # Check if the conda environment "GeneSelectR_env" exists
  envs <- reticulate::conda_list()
  if ("GeneSelectR_env" %in% envs$name) {
    message("The conda environment 'GeneSelectR_env' already exists. Skipping environment creation and package installation.")
  } else {
    # Ask the user if they want to create the environment
    if (interactive() &&
        !utils::menu(c("yes", "no"),
                     title = "The conda environment 'GeneSelectR_env' does not exist. Do you want to create it?") == 1) {
      stop("The conda environment 'GeneSelectR_env' was not created.")
    }
    # Create the conda environment
    reticulate::conda_create("GeneSelectR_env")

  # Ask the user if they want to install the necessary Python packages
  if (interactive() &&
      !utils::menu(c("yes", "no"),
                   title = "Do you want to install the necessary Python packages to the 'GeneSelectR_env' environment?") == 1) {
    stop("The necessary Python packages were not installed.")
  }

  # Install the necessary Python packages
  python_packages = c("scikit-learn",
                      "pandas",
                      "numpy <= 1.19",
                      "lightgbm",
                      "py-xgboost",
                      "boruta_py")

  reticulate::conda_install(packages = python_packages,
                            envname = 'GeneSelectR_env',
                            channel = 'conda-forge')
}
  # Use the conda environment
  reticulate::use_condaenv("GeneSelectR_env")

  # Import the Python modules with delay_load = TRUE
  sklearn <<- reticulate::import("sklearn", delay_load = TRUE)
  pandas <<- reticulate::import("pandas", delay_load = TRUE)
  numpy <<- reticulate::import("numpy", delay_load = TRUE)
  xgboost <<- reticulate::import("xgboost", delay_load = TRUE)
  boruta <<- reticulate::import("boruta", delay_load = TRUE)
  sys <<- reticulate::import("sys", delay_load = TRUE)
  multiprocessing <<- reticulate::import("multiprocessing", delay_load = TRUE)

  # # Update executable path in multiprocessing module only if the operating system is Windows
  # if (Sys.info()["sysname"] == "Windows") {
  #   exe <- file.path(sys$exec_prefix, "pythonw.exe")
  #   sys$executable <- exe
  #   sys$`_base_executable` <- exe
  #   multiprocessing$set_executable(exe)
  # }
  message("Imported Python libraries with delay_load = TRUE")
}



