#' @title Load the package and set up the Conda environment
#' @description This function is automatically run when the package is loaded. It checks if Conda is installed, creates a Conda environment if it doesn't exist, sets the R session to use the Conda environment if it's not already using it, and imports the Python libraries.
#' @importFrom reticulate conda_version conda_list conda_create use_condaenv import
#' @param libname The name of the package.
#' @param pkgname The path to the library where the package is installed.
#' @details The name of the Conda environment is obtained from the "FitSelect.conda_env" option or the "FITSELECT_CONDA_ENV" environment variable. If neither of these is set, a default name of "FitSelectEnv" is used. The Conda environment is created if it doesn't already exist. The R session is set to use the Conda environment if the "RETICULATE_PYTHON" environment variable is not set to the Python interpreter in the Conda environment. The Python libraries "sklearn", "pandas", "numpy", "lightgbm", "xgboost", and "boruta" are imported.
.onLoad <- function(libname, pkgname) {
  # Get the name of the Conda environment from an option or environment variable, or use a default name
  conda_env <- getOption("FitSelect.conda_env", Sys.getenv("FITSELECT_CONDA_ENV", "FitSelectEnv"))

  # Check if Conda is installed
  if (is.null(reticulate::conda_version())) {
    stop("Conda is not installed on your system. Please install Conda before proceeding.")
  } else {
    message("Conda is installed.")

    # Check if the Conda environment already exists
    if (!conda_env %in% reticulate::conda_list()$name) {
      # Create the Conda environment
      message("Creating Conda environment: ", conda_env)
      reticulate::conda_create(envname = conda_env)
    } else {
      message("Conda environment ", conda_env, " already exists.")
    }

    # Get the path of the Python interpreter in the Conda environment
    python_path <- system2("conda", c("env", "list"), stdout = TRUE)
    python_path <- sub(".*\\s", "", python_path[grepl(conda_env, python_path)])

    # Check if the RETICULATE_PYTHON environment variable is set to the Python interpreter in the Conda environment
    if (Sys.getenv("RETICULATE_PYTHON") != python_path) {
      # Set the RETICULATE_PYTHON environment variable
      Sys.setenv(RETICULATE_PYTHON = python_path)

      # Use the Conda environment in the R session
      message("Setting R session to use Conda environment: ", conda_env)
      reticulate::use_condaenv(conda_env, required = TRUE)
    } else {
      message("R session is already using Conda environment: ", conda_env)
    }
  }

  # Import the Python libraries
  message("Importing Python libraries.")
  sklearn <- reticulate::import("sklearn")
  pandas <- reticulate::import("pandas")
  numpy <- reticulate::import("numpy")
  lightgbm <- reticulate::import("lightgbm")
  xgboost <- reticulate::import("xgboost")
  boruta <- reticulate::import("boruta")

  # enable multiprocess
  sys <- reticulate::import("sys")
  exe <- file.path(sys$exec_prefix, "pythonw.exe")
  sys$executable <- exe
  sys$`_base_executable` <- exe

  # update executable path in multiprocessing module
  multiprocessing <- reticulate::import("multiprocessing")
  multiprocessing$set_executable(exe)
}
