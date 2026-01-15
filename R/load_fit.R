#' Load an fpemplus fit. This fit should be saved in a folder that contains
#' a file named `fit.rds` with the fpemplus object as well as csv files produced
#' by cmdstanr as a result of the estimation process.
#'
#' @param path path to the folder containing the model fit
#'
#' @return object of class fpemplus
#'
#' @export
load_fit <- function(path) {
  fit <- readRDS(Sys.glob(file.path(path, "fit*.rds"))[1]) # just take first fit object
  sample_files <- Sys.glob(file.path(path, "*.csv"))
  sample_files <- sample_files[!grepl("diagnostic", sample_files)]
  fit$samples <- cmdstanr::as_cmdstan_fit(sample_files)

  return(fit)
}
