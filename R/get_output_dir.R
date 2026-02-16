#' get directory of output, located in parent of wd
#'
#' @param ... character string subdirectories of the bayestransition_output
#' folder
#'
#' @return absolute path to the folder where output is saved
#'
#' @keywords internal
get_output_dir <- function(...) {
  do.call(file.path,
          c(list(here::here() %>% dirname(), "bayestransition_output"), ...))
}



#' Get relative output directory path
#'
#' Constructs a relative path to a subfolder inside `bayestransition_output`.
#'
#' @param folder_name Character. The name of the subfolder inside `bayestransition_output`.
#'
#'
#' @export
get_relative_output_dir <- function(folder_name) {
  file.path("..", "bayestransition_output", folder_name)
}

#' Rename output folder and directory in fit object
#'
#' Renames an existing folder in `bayestransition_output`and updates the `output_dir` in the model fit object.
#'
#' @param indicator Character. Indicator name.
#' @param run_step Character. Run step - either 1a, 1b, local_national.
#' @param old_folder_name Character. The current folder name (inside `bayestransition_output`).
#' @param new_folder_name Character. The new name to rename the folder to.
#'
#'
#' @export
rename_output_folder <- function(indicator, run_step, old_folder_name, new_folder_name) {

  old_dir <- get_relative_output_dir(old_folder_name)
  new_dir <- get_relative_output_dir(new_folder_name)

  if (!dir.exists(old_dir)) stop("Old directory doesn't exist: ", old_dir)
  if (dir.exists(new_dir)) stop("New directory already exists: ", new_dir)

  success <- file.rename(old_dir, new_dir)
  if (!success) stop("Failed to rename the folder.")

  # load and update fit object
  fit_path <- file.path(new_dir, paste0(indicator, "_fit_wpostsumm.rds"))
  fit <- readRDS(fit_path)
  fit$output_dir <- new_dir
  saveRDS(fit, fit_path)

  # if summary exists, update
  summary_path <- file.path(new_dir, paste0(indicator, "_summary", run_step, ".rds"))
  if (file.exists(summary_path)) {
    summary <- readRDS(summary_path)
    summary$output_dir <- new_dir
    saveRDS(summary, summary_path)
  }
}


