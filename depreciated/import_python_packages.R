#' @title Import Python Libraries
#' @description This function imports the necessary Python libraries for the package.
#' @return A list of imported Python libraries.
#' @importFrom reticulate import
#' @export
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
