#' @title Import Python Libraries
#' @description This function imports the necessary Python libraries for the package.
#' @return A list of imported Python libraries.
#' @importFrom reticulate import
#' @export
import_python_packages <- function() {
  sklearn <- reticulate::import("sklearn")
  pandas <- reticulate::import("pandas")
  numpy <- reticulate::import("numpy")
  lightgbm <- reticulate::import("lightgbm")
  xgboost <- reticulate::import("xgboost")
  boruta <- reticulate::import("boruta")

  list(
    sklearn = sklearn,
    pandas = pandas,
    numpy = numpy,
    lightgbm = lightgbm,
    xgboost = xgboost,
    boruta = boruta
  )
}
