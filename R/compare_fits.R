#' Compare multiple model fits in a single plot
#'
#' This function takes run names, loads the corresponding fits, and plots them
#' together using plot_estimates_local_all.
#'
#' @param runnames Character vector of run names (2-4 names). Run names follow the
#'   pattern: {indicator}_{runstep}_{folder_suffix}, e.g., "anc4_step1b_run_alldata_jan16"
#' @param modelnames Character vector of display names for the legend (same length as runnames).
#'   If NULL, uses the runnames.
#' @param iso_codes Optional character vector of ISO codes to filter plots. If NULL,
#'   returns plots for all countries.
#' @param dat_routine Optional routine data to overlay on plots
#' @param indicator_name Custom title for y-axis (if NULL, pulled from fit$data)
#' @param save_plots Boolean to save plots to PDF
#' @param output_folder Override directory for saving
#' @param add_caption Boolean to add explanatory caption
#' @param add_estimates Boolean to show model estimates/ribbons
#' @param use_for_facetting Boolean to adjust plot titles for facetted output
#'
#' @return List of ggplot objects, one per geographic unit (filtered by iso_codes if provided)
#' @export
#'
#' @examples
#' \dontrun{
#' # Compare step1a and step1b for anc4
#' plots <- compare_fits(
#'   runnames = c("anc4_step1b_run_alldata_jan16", "anc4_step1a_run_alldata_jan16"),
#'   modelnames = c("Step 1b", "Step 1a")
#' )
#'
#' # Compare validation and non-validation fits for specific countries
#' plots <- compare_fits(
#'   runnames = c("vdpt_step1b_run_alldata_jan16", "vdpt_step1b_val_alldata_jan16"),
#'   modelnames = c("Full model", "Validation"),
#'   iso_codes = c("ETH", "GHA", "KEN", "NGA", "UGA")
#' )
#' }
compare_fits <- function(runnames,
                         modelnames = NULL,
                         iso_codes = NULL,
                         dat_routine = NULL,
                         indicator_name = NULL,
                         save_plots = FALSE,
                         output_folder = NULL,
                         add_caption = FALSE,
                         add_estimates = TRUE,
                         use_for_facetting = FALSE) {

  # Validate input
  n_fits <- length(runnames)
  if (n_fits < 2 || n_fits > 4) {
    stop("compare_fits requires 2-4 run names. Got ", n_fits, ".")
  }

  # Set default model names if not provided
  if (is.null(modelnames)) {
    modelnames <- runnames
  }

  if (length(modelnames) != n_fits) {
    stop("Length of modelnames (", length(modelnames), ") must match length of runnames (", n_fits, ").")
  }

  # Pad modelnames to length 4 (required by plot_estimates_local_all)
  modelnames_padded <- c(modelnames, rep("", 4 - n_fits))[1:4]

  # Parse run names and load fits
  fits <- lapply(runnames, function(rn) {
    parsed <- parse_runname(rn)
    get_fit(
      indicator = parsed$indicator,
      runstep = parsed$runstep,
      folder_suffix = parsed$folder_suffix
    )
  })

  # Call plot_estimates_local_all with the appropriate number of fits
  plots <- plot_estimates_local_all(
    results = fits[[1]],
    results2 = if (n_fits >= 2) fits[[2]] else NULL,
    results3 = if (n_fits >= 3) fits[[3]] else NULL,
    results4 = if (n_fits >= 4) fits[[4]] else NULL,
    modelnames = modelnames_padded,
    iso_codes = iso_codes,
    dat_routine = dat_routine,
    indicator_name = indicator_name,
    save_plots = save_plots,
    output_folder = output_folder,
    add_caption = add_caption,
    add_estimates = add_estimates,
    use_for_facetting = use_for_facetting
  )

  return(plots)
}


#' Parse a run name into its components
#'
#' @param runname Character string of format {indicator}_{runstep}_{folder_suffix}
#' @return List with indicator, runstep, and folder_suffix
#' @keywords internal
parse_runname <- function(runname) {
  # Known runstep values (order matters - check longer patterns first)
  known_runsteps <- c(
    "local_subnational",
    "local_national",
    "step1ab",
    "step1a",
    "step1b"
  )

  # Known indicators
  known_indicators <- c("anc4", "ideliv", "vdpt")

  # Split on first underscore to get indicator
  parts <- strsplit(runname, "_", fixed = TRUE)[[1]]
  if (length(parts) < 3) {
    stop("Invalid runname format: ", runname,
         "\nExpected format: {indicator}_{runstep}_{folder_suffix}")
  }

  indicator <- parts[1]
  if (!(indicator %in% known_indicators)) {
    warning("Indicator '", indicator, "' not in known indicators: ",
            paste(known_indicators, collapse = ", "))
  }

  # Find which runstep matches
  remainder <- paste(parts[-1], collapse = "_")
  runstep <- NULL
  folder_suffix <- NULL

  for (rs in known_runsteps) {
    if (startsWith(remainder, paste0(rs, "_"))) {
      runstep <- rs
      folder_suffix <- substring(remainder, nchar(rs) + 2)  # +2 for underscore
      break
    }
  }

  if (is.null(runstep)) {
    stop("Could not identify runstep in: ", runname,
         "\nKnown runsteps: ", paste(known_runsteps, collapse = ", "))
  }

  list(
    indicator = indicator,
    runstep = runstep,
    folder_suffix = folder_suffix
  )
}
