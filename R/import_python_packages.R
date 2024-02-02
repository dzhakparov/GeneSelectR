#' @title Import Python Libraries
#' @description This function imports the necessary Python libraries for the package.
#' @return A list containing references to imported Python libraries used in the package:
#'   - @field sklearn: Scikit-learn machine learning library.
#'   - @field pandas: Data manipulation and analysis library.
#'   - @field numpy: Library for numerical computing.
#'   - @field lightgbm: Gradient boosting framework.
#'   - @field xgboost: Optimized distributed gradient boosting library.
#'   - @field boruta: Feature selection algorithm.
#'   - @field sys: Python system-specific parameters and functions.
#'   - @field multiprocessing: Support for concurrent execution using processes.
#' @importFrom reticulate import
import_python_packages <- function() {
  sklearn <- reticulate::import("sklearn", delay_load = TRUE)
  pandas <- reticulate::import("pandas", delay_load = TRUE)
  numpy <- reticulate::import("numpy", delay_load = TRUE)
  lightgbm <- reticulate::import("lightgbm", delay_load = TRUE)
  xgboost <- reticulate::import("xgboost", delay_load = TRUE)
  boruta <- reticulate::import("boruta", delay_load = TRUE)
  sys <- reticulate::import("sys", delay_load = TRUE)
  multiprocessing <- reticulate::import('multiprocessing', delay_load = TRUE)

  list(
    sklearn = sklearn,
    pandas = pandas,
    numpy = numpy,
    lightgbm = lightgbm,
    xgboost = xgboost,
    boruta = boruta,
    sys = sys,
    multiprocessing = multiprocessing
  )
}
