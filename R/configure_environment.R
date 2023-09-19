#' Configure Python Environment for GeneSelectR
#'
#' This function checks if Conda is installed, creates a new Conda environment (if it does not already exist),
#' installs necessary Python packages into the environment, and sets it as the active environment for reticulate.
#'
#' @param env_name The name of the Conda environment to be created. Defaults to "GeneSelectR_env".
#'
#' @return A message indicating the status of the environment configuration. If the configuration is successful,
#' the function will prompt the user to restart their R session for the changes to take effect.
#'
#' @examples
#' \dontrun{
#' # Configure the default environment
#' configure_environment()
#'
#' # Configure a custom environment
#' configure_environment("my_env_name")
#' }
#'
#' @export
configure_environment <- function(env_name = "GeneSelectR_env") {
  # Check if Conda is installed
  if (is.null(reticulate::conda_version())) {
    stop("Conda is not installed on your system. Please install Conda before proceeding.")
  } else {
    message("Conda is installed.")
  }

  # Check if the conda environment exists
  envs <- reticulate::conda_list()
  if (env_name %in% envs$name) {
    message(paste("The conda environment", env_name, "already exists. Skipping environment creation and package installation."))
  } else {
    # Ask the user if they want to create the environment
    if (interactive() &&
        !utils::menu(c("yes", "no"),
                     title = paste("The conda environment", env_name, "does not exist. Do you want to create it?")) == 1) {
      stop(paste("The conda environment", env_name, "was not created."))
    }
    # Create the conda environment
    reticulate::conda_create(env_name)

    # Ask the user if they want to install the necessary Python packages
    if (interactive() &&
        !utils::menu(c("yes", "no"),
                     title = "Do you want to install the necessary Python packages to the environment?") == 1) {
      stop("The necessary Python packages were not installed.")
    }

    # Install the necessary Python packages
    python_packages = c("scikit-learn",
                        "pandas",
                        "numpy <= 1.19",
                        "boruta_py",
                        'scikit-optimize')

    reticulate::conda_install(packages = python_packages,
                              envname = env_name,
                              channel = 'conda-forge')
  }

  message("Please restart your R session for the changes to take effect.")
}

#' Set RETICULATE_PYTHON for the Current Session
#'
#' This function sets the RETICULATE_PYTHON environment variable to the path of the Python interpreter
#' in the specified Conda environment, but only for the current R session. The change will not persist
#' after the R session is closed.
#'
#' @param env_name The name of the Conda environment. Default is 'GeneSelectR_env'.
#'
#' @details
#' This function checks if the specified Conda environment exists. If it does, the function sets the
#' RETICULATE_PYTHON environment variable to the path of the Python interpreter in that environment.
#' If the environment does not exist, the function stops with an error message.
#'
#' Users need to run this function in every new R session where they want to use your package. Also,
#' they should run this function before loading your package with library(), because the RETICULATE_PYTHON
#' environment variable needs to be set before reticulate is loaded.
#'
#' @return
#' This function does not return a value. It sets the RETICULATE_PYTHON environment variable for the
#' current R session and prints a message indicating the new value of RETICULATE_PYTHON.
#'
#' @examples
#' \dontrun{
#' set_reticulate_python('GeneSelectR_env')
#' }
#' @export
set_reticulate_python <- function(env_name = 'GeneSelectR_env') {
  # Check if the conda environment exists
  envs <- reticulate::conda_list()
  if (!(env_name %in% envs$name)) {
    stop(paste("The conda environment", env_name, "does not exist. Please create it before proceeding."))
  }

  # Get the path to the Python interpreter in the environment
  python_path <- envs[envs$name == env_name, "python"]

  # Set the RETICULATE_PYTHON environment variable for the current session
  Sys.setenv(RETICULATE_PYTHON = python_path)

  message(paste("Set RETICULATE_PYTHON to", python_path, "for the current R session."))
}

