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
    stop(paste0("😿 Conda is not installed on your system. Please install Conda before proceeding."))
  } else {
    message(paste0("😺 Conda is installed."))

    # Check if the Conda environment already exists
    if (!conda_env %in% reticulate::conda_list()$name) {
      # Create the Conda environment
      message(paste0("Creating Conda environment: ", conda_env, " ... 😸"))
      reticulate::conda_create(envname = conda_env)
      message(paste0("😻 Created Conda environment: ", conda_env))
    } else {
      message(paste0("😽 Conda environment ", conda_env, " already exists."))
    }

    # List of Python packages to be installed
    python_packages <- c("scikit-learn", "pandas", "numpy", "lightgbm", "xgboost", "boruta")

    # Check if the Python packages are already installed
    packages_to_install <- python_packages[!sapply(python_packages, reticulate::py_module_available)]

    if (length(packages_to_install) > 0) {
      # Install the necessary Python packages in the Conda environment
      message(paste0("Installing Python packages in Conda environment: ", conda_env, " ... 😼"))
      reticulate::conda_install(envname = conda_env, packages = packages_to_install)
      message(paste0("😹 Installed Python packages in Conda environment: ", conda_env))
    } else {
      message(paste0("😾 All necessary Python packages are already installed in Conda environment: ", conda_env))
    }

    # Use the Conda environment in the R session
    message(paste0("Setting R session to use Conda environment: ", conda_env, " ... 😿"))
    if (is.null(Sys.getenv("RETICULATE_PYTHON"))) {
      reticulate::use_condaenv(conda_env, required = TRUE)
    }
    message(paste0("😽 Set R session to use Conda environment: ", conda_env))

    # Import the Python libraries
    message(paste0("Importing Python libraries ... 😼"))
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

    message(paste0("😻 Imported Python libraries"))
  }
}



