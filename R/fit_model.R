

#' Fit the transition model to data.
#'
#' @param survey_df tibble with survey data
#' @param routine_df tibble with routine data
#' @param y column name of outcome.
#' @param se column name of outcome standard error.
#' @param year column name of outcome year.
#' @param source column name of data source.
#' @param area column name of the area of each observation
#'
#' for subnational:
#' @param population_data a data frame with yearly population counts for
#'   subnational regions. It should have columns matching the names specified
#'   for \code{year} and \code{area}. This data frame is only required if the
#'   primary \code{data} set contains a mix of observations at national and
#'   subnational levels.
#'
#' @param runstep type of run, currently one of "step1a", "step1b", "local_national" (see Details).
#' @param global_fit optional object of class `"fpemplus"`, used to obtain fixed
#'   values to use for some parameters in the current fit (see Details).
#' @param iso_select ISO code to use for local national run
#'
#' @param start_year start year of estimates.
#' @param end_year end year of estimates.
#'
#' Settings for global model fit 1a
#' @param t_star reference year used in model.
#' @param num_knots number of spline knots.
#' @param spline_degree spline degree. Degree 2 or 3 is supported.
#' @param hierarchical_asymptote vector specifying hierarchical structure for
#'   asymptote (see Details).
#' @param hierarchical_level vector specifying hierarchical structure for the
#'   level in reference year (see Details).
#' @param hierarchical_splines vector specifying hierarchical structure for
#'   spline coefficients (see Details).
#' @param Betas_upper_bound upper bound for the splines parameters
#' @param Betas_lower_bound lower bound for the splines parameters
#' @param Ptilde_low lower bound for the asymptote Ptilde
#' @param add_dataoutliers boolean indicator of whether to include data outliers in 1b
#'
#' @param extra_stan_data list of additional data to pass to Stan model

#' @param get_posteriors boolean indicator of whether to return posterior samples
#'
#' @param held_out binary vector indicating which observations are held out. Set to FALSE to hold out no observations.
#' @param validation_cutoff_year year to use for out-of-sample validation, overwrites held_out (to confirm it does)
#' @param validation_run boolean indicator of whether it's a validation model run or not
#'
#' @param generate_quantities binary vector indicating whether to simulate data from the fitted model
#'
#' Setting for where to save things
#' @param create_runname_and_outputdir boolean indicator of whether to create a runname and output directory
#' @param runnumber number to add to runname
#' @param rungroup group to add to runname
#' @param runname name to use for run
#'
#' Settings for sampling
#' @param add_inits boolean indicator of whether to add initial values to the Stan model
#' @param chains number of chains to run
#' @param iter_sampling number of posterior samples to draw
#' @param iter_warmup number of warmup iterations
#' @param add_sample boolean indicator of whether to return samples
#' @param compile_model boolean indicator of whether to compile the Stan model
#' @param force_recompile boolean indicator of whether to force recompilation of the Stan model
#' @param seed random seed
#' @param refresh number of iterations between progress updates
#' @param adapt_delta target acceptance rate for the No-U-Turn Sampler
#' @param max_treedepth maximum tree depth for the No-U-Turn Sampler
#' @param variational boolean indicator of whether to use variational inference (not yet tested)
#' @param nthreads_variational number of threads to use for variational inference
#'
#' @return fpemplus object.
#'
#' @details
#' The `fit_model` function fits the transition model to data. The model is
#' fitted using Stan, and the function returns an object of class `"fpemplus"`.
#' The argument \code{runstep} determines the type of run to perform. The
#' following run steps are supported:
#' - "step1a": Fit the model without the smoothing term, to estimate longer term trends.
#' - "step1b": Fit the model with smoothing terms, using a fit from step1a,
#' to estimate all smoothing and data model parameters.
#' - "local_national": Fit the model to data from a single country, using a 1b fit.
#' This is also explained in the documentation folder.
#'
#' Details on hierarchical set ups used
#' Several area-specific parameters of the fpemplus model have hierarchical priors
#' assigned to them so that information can be shared between areas.
#' The package allows the structure of the hierarchical prior to be configured by the user
#' through the \code{hierarchical_asymptote}, \code{hierarchical_level}, and \code{hierarchical_splines} arguments.
#' These arguments expect a character vector that specifies a nesting hierarchical structure.
#' Each element of the vector must be either "intercept" or a column name in the dataset, where
#' "intercept" will add a global intercept for the parameter.
#' The vector must be in descending order in terms of the hierarchy: that is, it starts with
#' "intercept" and proceeds down the hierarchy.
#'
#' For example, suppose we are fitting country-level data, where the dataset has columns
#' "name_country", "name_sub_region", and "name_region" containing the name of the country,
#' sub-region, and region that each observation belongs to. To specify that the spline coefficients
#' should be fitted with a hierarchical model in which countries are nested within sub-regions within regions within world,
#' we would use the argument
#' \code{hierarchical_splines = c("intercept", "name_region", "name_sub_region", "name_country")}.
#'
#' Optionally, model parameters can be fixed to values from a previous model fit
#' provided via the \code{global_fit} argument. In a typical use case, the
#' \code{global_fit} will have been fit to data from many geographic units
#' (e.g., all countries), while the current fit uses data from a smaller number
#' of locations.
#' Note: remainder of details is currently determined by the runstep
#' To use a global fit to fix parameter values, a number of settings must be the
#' same in the global fit and the current call to `fpemplus`:
#' - For any of the hierarchical settings, e.g.
#'   \code{hierarchical_asymptote}, all of the hierarchical levels
#'   used for that parameter in the global fit must also be used in the current
#'   fit. For instance, if the `global_fit` used
#'   \code{hierarchical_asymptote = c("intercept", "name_region", "name_country")},
#'   then in the current fit it is valid to use
#'   \code{hierarchical_asymptote = c("intercept", "name_region", "name_country")} again or
#'   \code{hierarchical_asymptote = c("intercept", "name_region", "name_country", "name_subnational")},
#'   but it is not valid to use
#'   \code{hierarchical_asymptote = c("intercept", "name_region", "name_subnational")}.
#' - All hierarchical levels for terms and sigmas to fix in the current fit are
#'   contained in the levels in the hierarchy used for the corresponding
#'   parameters in the global fit. For example, if
#'   \code{hierarchical_asymptote_sigmas_fixed = c("intercept", "name_region", "name_country")}
#'   then the global fit must have included at least "intercept", "name_region",
#'   and "name_country" for \code{hierarchical_asymptote}.
#' - Any hierarchical levels to fix in the current fit are at the highest levels
#'   of the hierarchical structure. For example, if
#'   \code{hierarchical_asymptote = c("intercept", "name_region", "name_country")},
#'   then it is valid to use
#'   \code{hierarchical_asymptote_sigmas_fixed = c("intercept", "name_region")},
#'   but it is not valid to use
#'   \code{hierarchical_asymptote_sigmas_fixed = c("intercept", "name_country")}.
#' - It is not valid to fix terms at a given hierarchy level without also fixing
#'   the sigma estimate at that hierarchy level. For example, we cannot specify
#'   \code{hierarchical_asymptote_sigmas_fixed = c("intercept")} and
#'   \code{hierarchical_asymptote_terms_fixed = c("intercept", "name_region")}
#' - It is only valid to set `fix_smoothing = TRUE` if also `smoothing = TRUE`.
#' - All settings for the arguments `model`, `t_star`, `smoothing`, `tau_prior`,
#'   and `rho_prior` must be the same in the `global_fit` and the current fit.
#' - If `hierarchical_splines_sigmas_fixed` or
#'   `hierarchical_splines_terms_fixed` include any hierarchical levels (i.e.,
#'   if either is different from the empty vector `c()`), all settings for
#'   `num_knots` and `spline_degree` must be the same in the `global_fit` and
#'   the current fit.
#' - All geographic units that appear in the `data` for the current fit at
#'   hierarchical levels for which any parameter is fixed must have also been
#'   included in the `data` used for the `global_fit`.
#' - If `fix_nonse = TRUE`, all data `source`s that appear in the `data` for the
#'   current fit must also have been included in the `data` used for the
#'   `global_fit`.
#'
#' @importFrom cmdstanr cmdstan_model write_stan_file
#' @importFrom tibble tibble
#' @importFrom splines bs
#' @import dplyr
#' @importFrom readr read_file
#' @importFrom stringr str_replace_all
#'
#' @export
#'
#'
#'
fit_model <- function(
    # survey data and columns used
  ## defaults added for our cd survey database
  survey_df,
  national_dat_df = NULL, # relevant only for subnat runs, national data to be considered
  # needs to be filtered to iso_select
  routine_df = NULL,
  mean_log_sigma = NULL, # if provided, overwrites the stored parameter
  popweights  = NULL, #a tibble with columns iso, admin1, year, prop
  #population_data = NULL,
  y = "invprobit_indicator",
  se = "se_invprobit_indicator",
  year = "year",
  source = "data_series_type",
  # for national level
  area = "iso",
  iso_select  = NULL, # used for local national run
  routine_data = NULL,
  # years to produce estimates for
  start_year = 2000,
  end_year = 2030,

  # type of run is defined by runstep:
  runstep, # type of run, step or localnat or localsubnat
  # step1a =  get subcluster parameters, fit w/o ar
  # step1b =  fix subcluster info and get ar and data outlier parameters and across country sigmas
  # local_national =  get local results only
  ## not implemented currently: step1: step1a  and step1b combined
  ## in progress: global_subnational, local_subnational
  global_fit = NULL, # eventually, read in from data_raw if needed but NULL

  ## arguments that are relevant for first global run only (follow from global_fit in other cases)
  t_star = 2010,   # t_star needs to be within the estimation period
  spline_degree = 2,
  num_knots = 8, # temporarily reduced from 8 for testing, to do!
  Betas_upper_bound = 0.5,
  Betas_lower_bound = 0.01,
  Ptilde_low = 0,
  add_dataoutliers = TRUE,
  # note: if we want to add many more arguments, can also consider adding to extra_stan_data
  extra_stan_data = list(),
  # for hierarchical parameters, with defaults based on
  # what we've been using for global runs so far (based on data used)
  # for subnational, one level is added
  hierarchical_level     = c("intercept", "subcluster", "iso"),
  hierarchical_splines   = c("intercept", "cluster", "iso"),
  hierarchical_asymptote = c("intercept", "cluster", "iso"),
  add_subnational_hierarchy = "admin1", # this is what's added to the hierarchy for subnational
  use_globalsubnat_fromnat = FALSE, # use when fitting a new country in local_subnational

  # Out-of-sample validation
  # to do: check that combi of held_out and validation_cutoff_year are still used correctly
  held_out = FALSE,
  validation_cutoff_year = NULL, # if not NULL, should be a year and is used to define/overwrite held_out set
  validation_run = FALSE,

  save_post_summ = FALSE, # added to be able to save results in a local_national run
  # Model checks
  generate_quantities = TRUE,
  # misc
  get_posteriors = TRUE,

  # outputdir
  ## minor to do: this is automated, consider updating default
  create_runname_and_outputdir = TRUE,
  runnumber = 1, # used if create_runname_and_outputdir , added to runname, increased automatically if directory exists
  rungroup = NULL,
  runname = NULL, # uswd if !create_runname_and_outputdir

  # settings for sampling
  chains = 4, # probably need more for final model
  iter_sampling = 200,
  iter_warmup = 150,
  add_sample = TRUE, # if FALSE, returns fit w/o samples
  # minor to do: check settings when finalizing stan model block/decision re instantiate
  compile_model  = TRUE, force_recompile = FALSE,
  seed = 1234,
  refresh = 10,
  adapt_delta = 0.9,
  max_treedepth = 14,
  # # settings for variational inference
  # not tested yet
  variational = FALSE,
  # if variational, just compile model with threading support and pass back model and stan_data
  ##variational = variational | !add_sample, # no sampling when TRUE
  nthreads_variational = 8, #40, # 8
  # max_lbfgs_iters = 1000, # default is 1000
  # num_psis_draws = 1000,

  add_inits = TRUE
  # # Stan settings
  # ...

) {

  data <- survey_df # for now, just to keep the same name as in fpem
  indicator <- data %>% pull(indic) %>% unique()

  add_aggregates <- ifelse(runstep == "local_subnational", TRUE, FALSE)

  # if we need aggregates, process national data
  if (add_aggregates & !is.null(national_dat_df)){
    out <- process_natdata_forsubnatfitting(national_dat_df, data)
    ##national_dat_df <- out$national_dat_df ##to confirm: we don't need national_dat_df
    national_dat_forfitting <- out$national_dat_forfitting
    data <- out$data
  }

  ###### What type of run is this, and what settings do we need? #####
  # if runstep is step1a, we use the defaults
  # to do: finish subnational, settings here
  subnational = FALSE
  population_data = NULL
  area = "iso"
  correlated_smoothing = FALSE
  correlated_smoothing_group = "iso"
  fix_subnat_corr = FALSE

  if (!runstep %in% c("step1a", "step1b", "local_national", "global_subnational", "local_subnational")){
    stop("runstep not yet implemented!")
  }
  if(runstep %in% c("step1a", "step1b")){
    get_posteriors = TRUE
  }
  if (runstep %in% c("step1a")){
    # first global fit, get subcluster parameters
    # don't include ar1
    print("We fit w/o ar.")
    smoothing <- FALSE # no smoothing
    print("We don't fix anything")
    hierarchical_asymptote_sigmas_fixed = c()
    hierarchical_asymptote_terms_fixed = c()
    hierarchical_splines_sigmas_fixed = c()
    hierarchical_splines_terms_fixed = c()
    hierarchical_level_sigmas_fixed = c()
    hierarchical_level_terms_fixed = c()
    fix_smoothing = FALSE
    fix_nonse = FALSE
    # # related to subnational estimation (currently outside loop)
    # subnational = FALSE
    # population_data = NULL
    # area = "iso"
    # correlated_smoothing = FALSE
    # correlated_smoothing_group = "iso"
    # fix_subnat_corr = FALSE
  } else {
    # all other runs, NOT 1a
    if (is.null(global_fit)){
      globalstepname <- dplyr::case_when(
        runstep == "step1b" ~ "1a",
        runstep == "local_national" ~ "1b",
        runstep == "global_subnational" ~ "1b",
        TRUE ~ "global_subnational"
      )
      # global_fit <- readRDS(file = paste0(
      #   here::here("data_raw/internal/"), indicator, "_summary",
      #   globalstepname,
      #   ".rds"))
      global_fit <- get(paste0(indicator, "_summary", globalstepname))
    }

    print("We use a global fit, and take selected settings from there.")
    # minor to do: print these settings?
    print("settings for the spline_degree and num_knots taken from global run")
    spline_degree <- global_fit$spline_degree
    num_knots <- global_fit$num_knots
    print("settings for Ptilde_low taken from global run")
    Ptilde_low <- global_fit$Ptilde_low
    Betas_upper_bound <- global_fit$Betas_upper_bound
    Betas_lower_bound <- global_fit$Betas_lower_bound
    print("Setting for tstar taken from global run")
    t_star <- global_fit$t_star
    # consider hierarchical set up
    if (!runstep %in% c("global_subnational")){
      print("Settings for hierarchical settings taken from global run")
      hierarchical_level <- global_fit$hierarchical_level
      hierarchical_splines <- global_fit$hierarchical_splines
      hierarchical_asymptote <- global_fit$hierarchical_asymptote
    } else {
      print("For subnational global run, we add a level for subnational hierarchical settings ")
      hierarchical_splines <- c(global_fit$hierarchical_splines, add_subnational_hierarchy)
      hierarchical_level <- c(global_fit$hierarchical_level, add_subnational_hierarchy)
      hierarchical_asymptote <- c(global_fit$hierarchical_asymptote, add_subnational_hierarchy)
      #print("except for the asymptote for subnational global")
      #if ...
      #hierarchical_asymptote <- c(global_fit$hierarchical_asymptote)
    }
    if (use_globalsubnat_fromnat & runstep == "local_subnational"){
      # same as global subnational, to add a level
      print("For subnational local run using nat global fit, we add a level for subnational hierarchical settings ")
      hierarchical_splines <- c(global_fit$hierarchical_splines, add_subnational_hierarchy)
      hierarchical_level <- c(global_fit$hierarchical_level, add_subnational_hierarchy)
      hierarchical_asymptote <- c(global_fit$hierarchical_asymptote, add_subnational_hierarchy)
    }
    # consider what to fix in hierarchical set up
    # for mean terms, we always fit up to the 2nd lowest level
    print("For hierarchical terms, we fix things up to the 2nd-lowest level.")
    # could consider for subnational global, to not fit country means
    hierarchical_asymptote_terms_fixed = hierarchical_asymptote[1:(length(hierarchical_asymptote)-1)]
    hierarchical_splines_terms_fixed = hierarchical_splines[1:(length(hierarchical_splines)-1)]
    hierarchical_level_terms_fixed = hierarchical_level[1:(length(hierarchical_level)-1)]
    # for sigma, differs between local and not local
    if (runstep %in% c("local_national", "local_subnational")){
      print("We fix all sigmas of hierarchical models.")
      hierarchical_asymptote_sigmas_fixed = hierarchical_asymptote[1:(length(hierarchical_asymptote))]
      hierarchical_splines_sigmas_fixed = hierarchical_splines[1:(length(hierarchical_splines))]
      hierarchical_level_sigmas_fixed = hierarchical_level[1:(length(hierarchical_level))]
    } else {
      print("For sigma terms in hierarchical models, we fix things up to the 2nd-lowest level.")
      hierarchical_asymptote_sigmas_fixed = hierarchical_asymptote[1:(length(hierarchical_asymptote)-1)]
      hierarchical_splines_sigmas_fixed = hierarchical_splines[1:(length(hierarchical_splines)-1)]
      hierarchical_level_sigmas_fixed = hierarchical_level[1:(length(hierarchical_level)-1)]
    }

    # data model and smoothing
    print("We take the data model setting from global fit.")
    add_dataoutliers <- global_fit$add_dataoutliers
    print("We add smoothing")
    smoothing <- TRUE
    if (runstep %in% c("step1b")){
      print("In 1b, we don't fix smoothing nor data model parameters.")
      fix_smoothing <- FALSE
      fix_nonse <- FALSE
    } else{
      print("We fix smoothing and data model parameters.")
      fix_smoothing <- TRUE
      fix_nonse <- TRUE
    }

    # subnational
    if (runstep %in% c("local_subnational", "global_subnational")){
      print("TMP solution: we do a subnational run but we set subational to false to not deal with aggregates ")
      subnational = FALSE #TRUE
      print("We use subnational data.")
      area = "admin1" #"region_code"
      print("We do NOT add subnational correlation.")
      correlated_smoothing = FALSE #TRUE
      # if (runstep == "step3"){
      #   print("We estimate subnational correlation.")
      #   fix_subnat_corr = FALSE
      # }
      #  if (runstep %in% c("local_subnational")){
      #    print("We fix subnat correlation.")
      fix_subnat_corr = FALSE # not fixing else we look for it in global fit
      # }
    }
  } # end not 1a


  ##### Data processing  #####
  # keep the names of the original data
  names_original_data <- names(data)
  print("We filter data to be inside estimation period")
  data <- data %>%
    filter(year >= start_year, year <= end_year)

  if (!(runstep %in% c("global_subnational", "local_subnational"))){
    print("We use national data")
    if (runstep == "local_national" & !is.null(iso_select)){
      print(paste("We only use data for", iso_select))
      data <- data %>%
        filter(iso == iso_select)
    }
  } else {
    print("We use subnational data (but you already knew that)")
    if (runstep == "local_subnational"& !is.null(iso_select)){
      print(paste("We only use data for", iso_select))
      data <- data %>%
        filter(iso == iso_select)
    }
  }

  if (runstep == "step1a"){
    print("We do give nonSE to DHS (by temporarily renaming DHS into DHS0)")
    data <- data %>%
      mutate(data_series_type = ifelse(data_series_type == "DHS", "DHS0", data_series_type))
  }

  # what validation data to use?
  if (!is.null(validation_cutoff_year)){
    print("Setting held_out based on validation_cutoff_year")
    held_out <- as.numeric(data[["start_date"]] >= validation_cutoff_year)
    validation_run <- TRUE
    print(held_out)
  }
  if(length(held_out) == 1 && held_out == FALSE) {
    held_out = rep(0, nrow(data))
  }  else {
    if(length(held_out) != nrow(data)) stop(glue::glue("held_out (length {length(held_out)}) must be same size as dataset ({nrow(data)} rows)."))
    held_out = as.numeric(held_out)
  }
  data$held_out <- held_out


  ##### Initial argument checks #####
  #stopifnot(is.character(tau_prior))
  #stopifnot(is.character(rho_prior))
  stopifnot(is.numeric(spline_degree))
  stopifnot(is.numeric(num_knots))
  stopifnot(is.logical(smoothing))

  if(!(spline_degree %in% c(2, 3))) {
    stop("spline_degree must be either 2 or 3.")
  }

  if(num_knots <= 0) {
    stop("num_knots must be greater than zero.")
  }

  if(nrow(data) == 0) {
    stop("Data has no rows.")
  }

  if(start_year > end_year) {
    stop("start_year must be less than end year")
  }

  if(length(hierarchical_asymptote) == 0) {
    stop("No hierarchical structure supplied for the asymptote. See the hierarchical_asymptote argument.")
  }

  if(length(hierarchical_level) == 0) {
    stop("No hierarchical structure supplied for the level in reference year. See the hierarchical_level argument.")
  }

  if(length(hierarchical_splines) == 0) {
    stop("No hierarchical structure supplied for the spline coefficients. See the hierarchical_splines argument.")
  }

  # if a global fit was provided, check for consistency between
  # settings for the global fit and settings for the current fit
  # note: less relevant now that we take settings from global fit
  if (!is.null(global_fit)) {
    # If anything related to splines is fixed, check that spline_degree and
    # num_knots match the settings for those parameters used for the global fit
    if (length(hierarchical_splines_sigmas_fixed) > 0 ||
        length(hierarchical_splines_terms_fixed) > 0) {
      if (spline_degree != global_fit$spline_degree || num_knots != global_fit$num_knots) {
        stop("If fixing any parameter estimates for splines, the spline degree and number of knots must be the same as the values used for the `global_fit`.")
      }
    }

    # check that levels in the hierarchies used in the global fit are all
    # contained in the levels in the hierarchies used in this fit, and the
    # first entries of the hierarchy to use for this fit match the hierarchical
    # levels used for the global fit.
    if (!all(global_fit$hierarchical_asymptote %in% hierarchical_asymptote)) {
      stop("hierarchical_asymptote must contain all levels that were used for the global fit.")
    }
    if (!all(global_fit$hierarchical_asymptote == hierarchical_asymptote[seq_along(global_fit$hierarchical_asymptote)])) {
      stop("The top levels of hierarchical_asymptote must match the levels that were used for the global fit.")
    }
    if (!all(global_fit$hierarchical_level %in% hierarchical_level)) {
      stop("hierarchical_level must contain all levels that were used for the global fit.")
    }
    if (!all(global_fit$hierarchical_level == hierarchical_level[seq_along(global_fit$hierarchical_level)])) {
      stop("The top levels of hierarchical_level must match the levels that were used for the global fit.")
    }
    if (!all(global_fit$hierarchical_splines %in% hierarchical_splines)) {
      stop("hierarchical_splines must contain all levels that were used for the global fit.")
    }
    if (!all(global_fit$hierarchical_splines == hierarchical_splines[seq_along(global_fit$hierarchical_splines)])) {
      stop("The top levels of hierarchical_splines must match the levels that were used for the global fit.")
    }

    # check that hierarchical terms and sigmas to fix are all contained at the
    # top of the levels in the hierarchy used in the global fit
    # to do: need to update checks...
    # if (#!all(hierarchical_asymptote_sigmas_fixed %in% global_fit$hierarchical_asymptote) ||
    #     !all(hierarchical_asymptote_terms_fixed %in% global_fit$hierarchical_asymptote)) {
    #   stop("Hierarchical estimates to fix for hierarchical_asymptote were not estimated in the global fit.")
    # }
    # if (#!all(hierarchical_asymptote_sigmas_fixed == global_fit$hierarchical_asymptote[seq_along(hierarchical_asymptote_sigmas_fixed)]) ||
    #     !all(hierarchical_asymptote_terms_fixed == global_fit$hierarchical_asymptote[seq_along(hierarchical_asymptote_terms_fixed)])) {
    #   stop("Hierarchical estimates to fix for hierarchical_asymptote do not match the highest hierarchical_asymptote levels estimated in the global fit.")
    # }
    #
    # if (#!all(hierarchical_level_sigmas_fixed %in% global_fit$hierarchical_level) ||
    #     !all(hierarchical_level_terms_fixed %in% global_fit$hierarchical_level)) {
    #   stop("Hierarchical estimates to fix for hierarchical_level were not estimated in the global fit.")
    # }
    # if (#!all(hierarchical_level_sigmas_fixed == global_fit$hierarchical_level[seq_along(hierarchical_level_sigmas_fixed)]) ||
    #     !all(hierarchical_level_terms_fixed == global_fit$hierarchical_level[seq_along(hierarchical_level_terms_fixed)])) {
    #   stop("Hierarchical estimates to fix for hierarchical_level do not match the highest hierarchical_level levels estimated in the global fit.")
    # }
    #
    # if (#!all(hierarchical_splines_sigmas_fixed %in% global_fit$hierarchical_splines) ||
    #     !all(hierarchical_splines_terms_fixed %in% global_fit$hierarchical_splines)) {
    #   stop("Hierarchical estimates to fix for hierarchical_splines were not estimated in the global fit.")
    # }
    # if (#!all(hierarchical_splines_sigmas_fixed == global_fit$hierarchical_splines[seq_along(hierarchical_splines_sigmas_fixed)]) ||
    #     !all(hierarchical_splines_terms_fixed == global_fit$hierarchical_splines[seq_along(hierarchical_splines_terms_fixed)])) {
    #   stop("Hierarchical estimates to fix for hierarchical_splines do not match the highest hierarchical_splines levels estimated in the global fit.")
    # }

    # It is not valid to fix terms at a given hierarchy level without also fixing
    # the sigma estimate at that hierarchy level.
    # For example, if x = mu + z * sigma,
    # it does not make sense to fix z without also fixing sigma.
    # on the other hand, we might fix sigma but not z if we want to borrow information
    # about variability from global fit, but the global fit didn't produce an estimate
    # of z for the geo unit we're interested in, or we are ok with re-estimating it?
    if (!all(hierarchical_asymptote_terms_fixed %in% hierarchical_asymptote_sigmas_fixed)) {
      stop("All values of hierarchical_asymptote_terms_fixed must also be contained in hierarchical_asymptote_sigmas_fixed")
    }
    if (!all(hierarchical_level_terms_fixed %in% hierarchical_level_sigmas_fixed)) {
      stop("All values of hierarchical_level_terms_fixed must also be contained in hierarchical_level_sigmas_fixed")
    }
    if (!all(hierarchical_splines_terms_fixed %in% hierarchical_splines_sigmas_fixed)) {
      stop("All values of hierarchical_splines_terms_fixed must also be contained in hierarchical_splines_sigmas_fixed")
    }
  }



  # Create district index for matching district and district index
  hierarchical_column_names <- unique(c(
    hierarchical_asymptote,
    hierarchical_splines,
    hierarchical_level
  )) %>%
    setdiff("intercept")

  # Make sure there are no NAs in any of the columns
  for(column in hierarchical_column_names) {
    if(column == "intercept") next
    #if (column == area) {
    #  check_nas_or_pops(data, column, year, population_data)
    #} else {
    check_nas(data, column)
    #}
  }

  # Initialize start and end year if necessary
  if(is.na(start_year)) start_year <- min(data[[year]])
  if(is.na(end_year)) end_year <- max(data[[year]])

  # to do: just filter, warning if not data!
  # Make sure the observed data are within the estimation period
  if(sum(!(data[[year]] %in% start_year:end_year)) > 0) {
    stop(glue::glue("Observations included in dataset that fall outside the estimation period ({start_year} to {end_year})."))
  }






  ##### Setup data for Stan #####

  # Create year lookup table
  time_index <- tibble::tibble(
    year = seq(start_year, end_year),
    t = 1:length(year)
  )

  data[[area]] <- ifelse(data[[area]] == "National", NA, data[[area]])
  geo_unit_index <- data[!is.na(data[[area]]), ] %>%
    dplyr::distinct(!!! syms(hierarchical_column_names)) %>%
    dplyr::mutate(c = 1:n())

  source_index <- data %>%
    dplyr::distinct("{source}" := .data[[source]]) %>%
    dplyr::mutate(source = 1:n())

  year_by <- c()
  year_by[year] = "year"
  data <- data %>%
    dplyr::left_join(time_index, by = year_by) %>%
    dplyr::left_join(geo_unit_index, by = hierarchical_column_names) %>%
    dplyr::left_join(source_index, by = source)

  # for fp, data needs to be sorted by country
  data <- data %>% arrange(c)
  # now save original data
  original_data <- data %>%
    dplyr::select(all_of(names_original_data))


  ## all data checks and imputation related to NAs
  # Make sure there are no NAs in supplied columns
  check_nas(data, year)
  check_nas(data, source)

  # maybe less relevant for 1 indicator...
  # create obs_isNA
  obs_isNA <- is.na(data[[y]])
  check_nas(data[!obs_isNA, ], se)
  # impute arbitrary numbers to avoid NA issues?
  data[[y]][obs_isNA] <- 100
  data[[se]][obs_isNA] <- 100

  # Make sure SEs are all positive and non-zero
  if(sum(data[[se]] <= 0) > 0) {
    stop(glue::glue("All standard errors must be greater than zero ({sum(data[[se]] <= 0)} observations supplied with zero or negative SEs)."))
  }


  # In "local" fits, ensure that for all hierarchy levels where any quantity is
  # fixed, all geographic units in data used for the local fit also were present
  #  in the global fit
  # LA doesn't think this check is needed? also, gives error when just running with global fit but without fixing any hierarchies
  # if (!is.null(global_fit)) {
  #   fixed_hierarchy_levels <- unique(c(hierarchical_asymptote_sigmas_fixed,
  #                                      hierarchical_asymptote_terms_fixed,
  #                                      hierarchical_splines_sigmas_fixed,
  #                                      hierarchical_splines_terms_fixed,
  #                                      hierarchical_level_sigmas_fixed,
  #                                      hierarchical_level_terms_fixed))
  #   fixed_hierarchy_levels <- fixed_hierarchy_levels[fixed_hierarchy_levels != "intercept"]
  #
  #   fixed_geo_unit_index_local <- geo_unit_index[fixed_hierarchy_levels] |>
  #     dplyr::distinct()
  #   fixed_geo_unit_index_global <- global_fit$geo_unit_index[fixed_hierarchy_levels] |>
  #     dplyr::distinct()
  #   missing_geos <- fixed_geo_unit_index_local |>
  #     dplyr::anti_join(fixed_geo_unit_index_global,
  #                      by = fixed_hierarchy_levels)
  #   if (nrow(missing_geos) > 0) {
  #     stop("All geographic units that appear in the data for the current fit at hierarchical levels for which any parameter is fixed must have also been included in the data used for the `global_fit`.")
  #   }
  # }


  if(!is.null(t_star) && length(t_star) > 1) {
    if(length(t_star) != nrow(geo_unit_index)) stop("t_star must be null, length one, or the same length as the number of countries.")
    t_stars <- t_star
  }  else if(!is.null(t_star)) {
    if(!(t_star %in% time_index$year)) stop(glue::glue("Supplied t_star {t_star} not in range of estimation years ({start_year} to {end_year})."))
    t_stars <- rep(time_index[time_index$year == t_star, ][["t"]], nrow(geo_unit_index));
  }
  else {
    # Otherwise, pick t_star to be the mean of the observation years.
    t_stars <- data %>%
      dplyr::filter(!is.na(c)) %>%
      dplyr::group_by(c) %>%
      dplyr::summarize(t_star = round(mean(t))) %>%
      dplyr::pull(t_star)
  }

  t_last <- max(data$t)

  data <- data %>%
    # add index for DHS
    # minor to do: remove dependencies on data_series_type?
    mutate(isDHS = ifelse(data_series_type == "DHS", 1, 0))
  if (!add_dataoutliers){
    data <- data %>% mutate(nooutlier = 1)
  }

  # add obs period per country
  if (any(is.na(data[[area]]))){
    # if there are national aggregates
    t_min <- rep(min(data$t), nrow(geo_unit_index))
  } else {
    t_min <- data %>%
      group_by(c) %>%
      summarise(t_min = min(t)) %>%
      pull(t_min)
    t_min <- ifelse(t_min > (t_stars-1), t_stars- 1, t_min)
  }
  # t_max <- data %>%
  #   group_by(c) %>%
  #   summarise(t_max = max(t)) %>%
  #   pull(t_max)
  # t_max <- ifelse(t_max < (t_stars+ 1), t_stars+ 1, t_max)
  # update: setting t_max to 2024 to avoid issues when including EMUs
  # this includes same dependence on start and end year as does t star
  t_max <- rep(which(seq(start_year, end_year) == 2024), nrow(geo_unit_index))


  # put together stan data
  stan_data <- list(
    n_geounit = nrow(geo_unit_index),
    T = nrow(time_index),
    N = nrow(data),
    S = nrow(source_index),
    held_out = held_out,
    isDHS = data$isDHS,
    nooutlier = data$nooutlier,
    any_bias = data$any_bias,
    time = array(data$t),
    t_min = array(t_min),
    t_max = array(t_max),
    # for geo_unit:
    # in case of NA values in data$c, replace with the dummy value 0
    # this is only used in multiscale fitting with mixed national and subnational data
    geo_unit = array(ifelse(is.na(data$c), 0L, data$c)),
    source = array(data$source),
    s = array(data[[se]]),
    y = array(data[[y]]),
    obs_isNA = obs_isNA,
    t_star = array(t_stars),
    t_last = t_last,
    add_dataoutliers = add_dataoutliers,
    generate_quantities = generate_quantities,
    validation_run = validation_run,
    verysmallnumber = 0.00001 # lower bound for sds
    # used earlier in generated quanities to get splines (fits)
    #eta_grid = seq(0.01, 0.99, 0.01),
    #N_eta = length(seq(0.01, 0.99, 0.01)),
  )
  #return(stan_data)
  ##### Set up splines and hierarchical structures ######

  stan_spline_data <- get_stan_spline_data(num_knots = num_knots, spline_degree = spline_degree)


  ##### Set up hierarchical structures ######
  stan_data[["Ptilde_isvector"]] <- FALSE
  parname <- "Ptilde"
  stan_data[[paste0(parname, "_scalarprior_sd")]] <- 2
  stan_data[[paste0(parname, "_scalarprior_mean")]] <- 2 # Ptilde transformed is close to 1
  stan_data[[paste0(parname, "_prior_sd_sigma_estimate")]] <- 1

  hier_data <- hier_stan_data  <- list()
  hier_data[["Ptilde_data"]] <- hierarchical_data(geo_unit_index, hierarchical_asymptote)
  hier_stan_data[["Ptilde"]] <- hierarchical_param_stan_data(
    global_fit = global_fit,
    param_name = "Ptilde",
    param_data = hier_data[["Ptilde_data"]],
    hierarchical_terms_fixed = hierarchical_asymptote_terms_fixed,
    hierarchical_sigmas_fixed = hierarchical_asymptote_sigmas_fixed)

  stan_data[["Omega_isvector"]] <- FALSE
  parname <- "Omega"
  stan_data[[paste0(parname, "_scalarprior_sd")]] <- 2
  stan_data[[paste0(parname, "_scalarprior_mean")]] <- 0
  stan_data[[paste0(parname, "_prior_sd_sigma_estimate")]] <- 1


  hier_data[["Omega_data"]] <- hierarchical_data(geo_unit_index, hierarchical_level)
  hier_stan_data[["Omega"]] <- hierarchical_param_stan_data(
    global_fit = global_fit,
    param_name ="Omega",
    param_data = hier_data[["Omega_data"]],
    hierarchical_terms_fixed = hierarchical_level_terms_fixed,
    hierarchical_sigmas_fixed = hierarchical_level_sigmas_fixed)

  stan_data[["Betas_isvector"]] <- TRUE
  parname <- "Betas"
  stan_data[[paste0(parname, "_scalarprior_sd")]] <- 2
  stan_data[[paste0(parname, "_scalarprior_mean")]] <- -1 #  # Beta is negative
  stan_data[[paste0(parname, "_prior_sd_sigma_estimate")]] <- 1

  # k is being calculated somewhere.... replaces this
  stan_data[["Betas_k_terms"]] <- stan_spline_data[["k"]]
  hier_data[["Betas_data"]] <- hierarchical_data(geo_unit_index, hierarchical_splines)

  hier_stan_data[["Betas"]] <- hierarchical_param_stan_data(
    global_fit = global_fit,
    param_name = "Betas",
    param_data = hier_data[["Betas_data"]],
    hierarchical_terms_fixed = hierarchical_splines_terms_fixed,
    hierarchical_sigmas_fixed = hierarchical_splines_sigmas_fixed)

  hier_stan_data[["Betas_lower_bound"]] <- Betas_lower_bound
  hier_stan_data[["Betas_upper_bound"]] <- Betas_upper_bound
  hier_stan_data <- purrr::list_flatten(hier_stan_data, name_spec = "{inner}")

  # if there are maxes for a sigma, update to get max for that sigma
  if (!is.null(global_fit)){
    # to do: decide whether to use
    hier_stan_data_sigmamax <- list(
      Ptilde_sigma_max = min(hier_stan_data$Ptilde_sigma_fixed),
      Omega_sigma_max = min(hier_stan_data$Omega_sigma_fixed),
      Betas_sigma_max_1 = min(hier_stan_data$Betas_sigma_fixed[,1]),
      Betas_sigma_max_2 = min(hier_stan_data$Betas_sigma_fixed[,2]),
      Betas_sigma_max_3 = min(hier_stan_data$Betas_sigma_fixed[,3]),
      Betas_sigma_max_4 = min(hier_stan_data$Betas_sigma_fixed[,4])
    )
  } else {
    hier_stan_data_sigmamax <- list(
      Ptilde_sigma_max = 5,
      Omega_sigma_max = 5,
      Betas_sigma_max_1  = 5,
      Betas_sigma_max_2 = 5,
      Betas_sigma_max_3 = 5,
      Betas_sigma_max_4 = 5
    )
  }

  ##### Set up handling of smoothing #####
  smoothing_data <- list(
    "smoothing" = as.integer(smoothing),
    "correlated_smoothing" = as.integer(correlated_smoothing),
    "fix_smoothing" = as.integer(fix_smoothing),
    "rho_fixed" = numeric(0),
    "tau_fixed" = numeric(0),
    "fix_subnat_corr" = as.integer(fix_subnat_corr),
    "rho_correlationeps_fixed" = numeric(0)
  )
  if (fix_smoothing) {
    if (is.null(global_fit)) {
      stop("fix_smoothing was set to TRUE, but a global_fit was not provided.")
    }
    if (!smoothing) {
      stop("fix_smoothing was set to TRUE, but smoothing is FALSE.")
    }

    # get estimates of the smoothing parameters rho and tau from the global fit
    smoothing_data$rho_fixed <- global_fit$post_summ %>% filter(variable == "rho[1]") %>% pull(postmean)
    smoothing_data$tau_fixed <- global_fit$post_summ %>% filter(variable == "tau[1]") %>% pull(postmean)
  }
  if (fix_subnat_corr){
    if (is.null(global_fit)) {
      stop("fix_subnat_corr was set to TRUE, but a global_fit was not provided.")
    }
    smoothing_data$rho_correlationeps_fixed <- global_fit$post_summ %>% filter(variable == "rho_correlationeps[1]") %>% pull(postmean)
  }

  # compute info for how to do correlated smoothing
  if (correlated_smoothing) {
    if (!(correlated_smoothing_group %in% hierarchical_column_names)) {
      stop("`hierarchical_column_names` must be one of the variables used for hierarchical levels.")
    }

    # TODO: This code assumes that in the geo_unit_index,
    # lowest-level geo areas that are within the same `correlated_smoothing_group`
    # (e.g. within the same country) are assigned consecutive indices c.
    # This depends on the order of data being passed in, so maybe we should
    # also arrange the geo_unit_index by `hierarchical_column_names` at the time
    # it is created, before we use it?
    unique_group_vals <- unique(geo_unit_index[[correlated_smoothing_group]])

    smoothing_data$n_cor_smoothing_blocks <- length(unique_group_vals)
    smoothing_data$cor_smoothing_block_sizes <- array(purrr::map_dbl(
      unique_group_vals,
      function(v) {
        sum(geo_unit_index[[correlated_smoothing_group]] == v)
      }
    ))
    smoothing_data$max_cor_smoothing_block_size <- max(smoothing_data$cor_smoothing_block_sizes)
  } else {
    smoothing_data$n_cor_smoothing_blocks <- 0L
    smoothing_data$cor_smoothing_block_sizes <- integer(0)
    smoothing_data$max_cor_smoothing_block_size <- 0L
  }


  ##### Set up handing of data model hyperparameters ######
  nonse_data <- list(
    "fix_nonse" = as.integer(fix_nonse),
    "nonse_fixed" = numeric(0))
  parnames_outlier_hyper <- c("global_shrinkage_dm", "caux_dm", "sdbias")
  parnames_outlier_hyper_fixed <- paste0(parnames_outlier_hyper, "_fixed")
  # convoluted way to get each par in the list, with numeric(0) assigned
  outlier_fixed <- rep(0, length(parnames_outlier_hyper))
  names(outlier_fixed) <- parnames_outlier_hyper_fixed
  nonse_data <- c(nonse_data, lapply(split(outlier_fixed,names(outlier_fixed)),unname))
  for (parname in parnames_outlier_hyper_fixed){
    nonse_data[[parname]] <- numeric(0)
  }

  if (fix_nonse) {
    if (is.null(global_fit)) {
      stop("fix_nonse was set to TRUE, but a global_fit was not provided.")
    }
    if (!all(source_index$data_series_type %in% global_fit$source_index$data_series_type)) {
      stop("fix_nonse was set to TRUE, but data for local fit includes source types that were not in the data for the global fit.")
    }
    # get the nonse estimates from the global fit, ensuring that we get the
    # right ones, in the right order for sources in local fit
    parnames_outlier_hyper_tofix <-  c(parnames_outlier_hyper)
    parname <- "nonse"
    # to do: check ordering ok here?
    global_nonse_estimates <- global_fit$post_summ %>%
      filter(variable_no_index == parname) %>%
      mutate(data_series_type = global_fit$source_index$data_series_type)
    nonse_data[[paste0(parname, "_fixed")]] <-
      source_index |>
      dplyr::left_join(global_nonse_estimates, by = "data_series_type") |>
      dplyr::pull(postmean)
    # for outliers
    for (parname in parnames_outlier_hyper_tofix){
      nonse_data[[paste0(parname, "_fixed")]] <- global_fit$post_summ %>% filter(variable == paste0(parname, "[1]")) %>% pull(postmean)
    }
  }# end fixing dm pars


  ##### Extra set up for subnational runs ######
  if (!subnational){
    print("NOT using or producing aggregates")
    # extra stan data not used but needs to be provided
    n_agg_units <- 1
    extra_stan_data <- c(extra_stan_data,
                         list(
                           subnational = subnational,
                           is_agg_obs = rep(0, dim(data)[1]),
                           n_agg_units = n_agg_units,
                           agg_unit = rep(0, dim(data)[1]),
                           geo_unit_pop_wt = array(0, c(n_agg_units, nrow(geo_unit_index),
                                                        nrow(time_index)))
                         ))
  } else {
    # get population-based weights for each year
    # currently assumes the fit is to one aggregated (national) unit
    # this should replace the check that's done in check_nas_or_pop
    # geo_unit_subindex <- geo_unit_index[c(area, "c")]
    # required_pop_rows <- tidyr::expand_grid(
    #   year = time_index$year,
    #   area = geo_unit_subindex[[area]]) %>%
    #   dplyr::left_join(time_index, by="year")
    # colnames(required_pop_rows) <- c(year, area, "t")
    # required_pop_rows <- required_pop_rows %>%
    #   dplyr::left_join(geo_unit_subindex, by=area) %>%
    #   dplyr::left_join(population_data, by = c(year, area))
    # missing_pop_rows <- required_pop_rows |>
    #   dplyr::filter(is.na(population))
    # if (nrow(missing_pop_rows) > 0) {
    #   stop(glue::glue("If there are NAs in column {area}, `population_data` must include population data for all areas and years."))
    # }
    # geo_unit_pop_wt <- required_pop_rows[c("t", "c", "population")] %>%
    #   tidyr::pivot_wider(names_from = "t", values_from = "population") %>%
    #   dplyr::select(-c) %>%
    #   as.matrix()
    # geo_unit_pop_wt <- sweep(geo_unit_pop_wt, 2, apply(geo_unit_pop_wt, 2, sum), `/`)


    agg_data <- construct_agg_data(geo_unit_index=geo_unit_index,
                                   time_index=time_index,
                                   aggregation_meta=population_data)
    agg_geo_unit_index <- agg_data$agg_geo_unit_index
    geo_unit_pop_wt <- agg_data$geo_unit_pop_wt


    extra_stan_data <- c(extra_stan_data,


                         list(


                           subnational = subnational,
                           # indicator of whether observations are for aggregates of multiple areas,
                           # and the population weightings for those areas
                           n_agg_units = nrow(agg_geo_unit_index),
                           is_agg_obs = as.integer(data$is_agg_obs),
                           agg_unit = ifelse(
                             data$is_agg_obs,
                             data["agg_unit_name"] |>
                               dplyr::left_join(agg_geo_unit_index, by = "agg_unit_name") |>
                               dplyr::pull(i),
                             0L),
                           geo_unit_pop_wt = geo_unit_pop_wt))
  } # end subnational

  if (add_aggregates){
    # to do: some checks that we have pop for all regions?
    # else a c problem so this would show up anyway?
   # stan_data$prop_tr <- matrix(1/nrow(geo_unit_index), nrow = nrow(time_index), ncol = nrow(geo_unit_index))
    stan_data$prop_tr <- popweights %>%
      filter(iso == iso_select) %>%
      select(-iso) %>%
      left_join(geo_unit_index %>%
                  select(admin1, c)) %>%
      left_join(time_index %>%
                  select(year, t)) %>%
      arrange(c) %>%
      select( -c, -t)  %>%
      pivot_wider(names_from = admin1, values_from = prop) %>%
      select(-year) %>%
      as.matrix()
  }

  #### add routine data
  if (is.null(routine_data)){
    dat_routine <- NULL
    routine_list <- NULL
  } else {
    #hyper_param <- readRDS(here::here("data_raw/internal/", paste0("routine_hyperparameters_", indicator, ".rds")))
    hyper_param <- get(paste0("routine_hyperparameters_", indicator))
    combined_list <- get_standata_routine(
                          service_statistic_df = routine_data,
                          hyper_param = hyper_param,
                          time_index = time_index,
                          geo_unit_index = geo_unit_index %>%
                            dplyr::select(any_of(c("iso", "admin1", "c")))
                    )
      dat_routine <- combined_list$dat_routine # to use for plotting
      routine_list <- combined_list$routine_list # to pass into stan_data
      if (!is.null(mean_log_sigma)){
        routine_list$mean_log_sigma <- mean_log_sigma
      }
  }



  #### reading and loading stan model
  stanmodelname <- case_when(
    is.null(routine_data) & !add_aggregates ~ "fpem",
    !is.null(routine_data) & !add_aggregates ~ "fpem_routine",
    is.null(routine_data) & add_aggregates ~ "fpem_aggregates",
    !is.null(routine_data) & add_aggregates ~ "fpem_routine_aggregates"
  )
  stan_file_path <- system.file(paste0("stan/", stanmodelname, ".stan"),
                                package = "BayesCoverageIndicators")

  if (compile_model){
    stan_model <- compile_model(variational = variational,
                                nthreads_variational = nthreads_variational,
                                force_recompile = force_recompile,
                                stan_file_path = stan_file_path )
  }


  ##### Create list with combined inputs/outputs ####



  # to pass to stan
  stan_data <- c(stan_spline_data,
                 routine_list,
                 extra_stan_data,
                 stan_data,
                 hier_stan_data,
                 hier_stan_data_sigmamax,
                 Ptilde_low = Ptilde_low,
                 smoothing_data,
                 nonse_data)
  # pass back to user
  result <- list(
    runstep = runstep,
    record_id_fixed_used = data$record_id_fixed, # used to define outliers from 1a, to use in 1b
    # relabeled original data to avoid confusion
    original_data = original_data,
    # data now corresponds to input to stan_data
    data = data,
    stan_data = stan_data,
    time_index = time_index,
    geo_unit_index = geo_unit_index,
    source_index = source_index,
    y = y,
    se = se,
    year = year,
    source = source,
    area = area,
    dat_routine = dat_routine,
    national_dat_df = national_dat_df,
    # to do: check for more than 1 process
    # for re-use in (more) local fits
    hierarchical_asymptote = hierarchical_asymptote,
    hierarchical_splines = hierarchical_splines,
    hierarchical_level = hierarchical_level,
    Ptilde_low  = Ptilde_low,
    Betas_upper_bound = Betas_upper_bound,
    Betas_lower_bound = Betas_lower_bound,

    t_star  = t_star,
    num_knots = num_knots,
    spline_degree = spline_degree,
    smoothing = smoothing,
    add_dataoutliers = add_dataoutliers,

    held_out = held_out,
    # tau_prior = tau_prior,
    # rho_prior = rho_prior,
    # fix_subnat_corr = fix_subnat_corr,
    correlated_smoothing = correlated_smoothing
  )
  result <- c(result,
              # hier_data needs to be unlisted in result
              hier_data)


  if (compile_model){
    result$stan_model <- stan_model
  }
  if (subnational){
    result <- c(result,
                list(agg_geo_unit_index = agg_geo_unit_index,
                     geo_unit_pop_wt = geo_unit_pop_wt))
  }

  ##### Create an output directory for the model ####
  if (create_runname_and_outputdir & is.null(runname)){
    run_type <- if(validation_run == TRUE) "val" else "run"
    # set up directory to store the run
    runname <- paste0(indicator, "_", runstep, "_", run_type, "_", runnumber,
                      ifelse(variational, "_variational", ""))
    output_dir <- get_relative_output_dir(runname)
    while(dir.exists(output_dir) & runnumber < 100) {
      print("output directory already exists, increasing runnumber by 1")
      runnumber <- runnumber + 1
      runname <- paste0(indicator, "_", runstep, "_", run_type, "_", runnumber,
                        ifelse(variational, "_variational", ""))
      output_dir <- get_relative_output_dir(runname)
    }
    if (runnumber == 100){
      stop("runnumber is 100, have you really done this run 100 times already?")
    }
  } else {
    if (is.null(rungroup)) {
      output_dir <- get_relative_output_dir(runname)
    } else {
      output_dir <- get_relative_output_dir(rungroup, runname)
    }
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  print(paste("output directory is", output_dir))
  result$output_dir <- output_dir

  ##### Fit model ########
  if (!add_sample){
    return(result)
  }
  if (!variational){
    if (add_inits){
      init_ll <- lapply(1:chains, function(id) init_fun(chain_id = id, stan_data))
    } else {
      init_ll <- NULL
    }
    # can try this too
    #init = function(chain_id) init_fun(chain_id = chain_id, fit$stan_data),
 # return(stan_data)
    fit <- stan_model$sample(
      stan_data,
      save_latent_dynamics = TRUE,
      init = init_ll,
      chains = chains,
      parallel_chains  = chains,
      iter_sampling = iter_sampling,
      iter_warmup = iter_warmup,
      seed = seed,
      refresh = refresh,
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth
    )

    result <- c(result,
                samples = fit)
  } else {
    # variational
    # no longer tested
    fit_pf <- fit$stan_model$pathfinder(data = fit$stan_data, seed = seed,
                                        init = function(chain_id) init_fun(chain_id = chain_id, fit$stan_data),
                                        #init = 0,
                                        # num_psis_draws = num_psis_draws,
                                        num_paths= nthreads_variational,
                                        num_threads = nthreads_variational,
                                        max_lbfgs_iters = max_lbfgs_iters, # default is 1000
                                        single_path_draws=50,
                                        output_dir = output_dir,
                                        history_size = 50
    )
    result <- c(result,
                samples = fit_pf)
  }

  result$data <- add_uncertainty_in_obs(result)
  if (runstep %in% "local_subnational"){
    # fix national obs
    # rename NA as national in obs
    # making sure that data are plotted
    result$data <- result$data %>%
      # data that were used in fitting
      mutate(admin1 = ifelse(is.na(admin1), "National", admin1)) %>%
      # data not used in fitting
      bind_rows(result$national_dat_df %>%
                mutate(admin1 = "National", est_indicator = indicator, held_out = 0))
  }

  if (get_posteriors){
    cat("Extracting posteriors...\n")
    result$posteriors <- process_fit(result, parallel_chains = ifelse(is.null(chains), 1, chains),
                                     save_eps = FALSE,
                                     add_aggregates = add_aggregates,
                                     save_nontemporal  = FALSE)
    # not sure we still want this class
    # attr(result, "class") <- "fpemplus"
    saveRDS(result, file.path(output_dir, paste0(indicator, "_fit_withpost.rds")))
  } else {
    saveRDS(result, file.path(output_dir, paste0(indicator, "_fit_nopost.rds")))
  }


  if (runstep %in% c("step1a", "step1b", "global_subnational") | save_post_summ){
    result$post_summ <- get_posterior_summaries_andfindpar(result)
    # we may not need this extra saving
    saveRDS(result, file.path(output_dir, paste0(indicator, "_fit_wpostsumm.rds")))
    stepname <- dplyr::case_when(
      runstep == "step1a" ~ "1a",
      runstep == "step1b" ~ "1b",
      TRUE ~ runstep
    )
    summary <- result
    summary$samples <- NULL
    saveRDS(summary, file.path(output_dir, paste0(indicator, "_summary", stepname, ".rds")))
  }

  return(result)

}

