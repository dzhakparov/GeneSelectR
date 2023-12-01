#' Create a specific Conda environment
#'
#' @description This function creates a Conda environment if it doesn't already exist.
#' @param conda_env The name of the Conda environment to create.
#' @return description
#' @importFrom reticulate conda_create conda_list
#' @keywords hidden
create_conda_env <- function(conda_env = "GeneSelectR_env") {
  # Check if the Conda environment already exists
  if (!conda_env %in% reticulate::conda_list()$name) {
    # Create the Conda environment
    reticulate::conda_create(envname = conda_env)
    message(paste0("Created Conda environment: ", conda_env))
  } else {
    message(paste0("Conda environment ", conda_env, " already exists."))
  }
}


#' Install necessary Python packages in a specific Conda environment
#'
#' @description This function installs the necessary Python packages in a specific Conda environment.
#' @param conda_env The name of the Conda environment to use.
#' @return description
#' @importFrom reticulate py_install
#' @export
#'
install_python_packages <- function(conda_env = "GeneSelectR_env") {
  # List of Python packages to be installed
  python_packages <- c("scikit-learn <= 0.22.1",
                       "pandas <= 1.2.3",
                       "numpy <= 1.19",
                       "lightgbm",
                       "xgboost",
                       "boruta_py",
                       'scikit-optimize')

  # Install the Python packages in the Conda environment
  reticulate::conda_install(packages = python_packages, envname = conda_env)
}
