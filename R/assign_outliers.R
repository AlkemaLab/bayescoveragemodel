#' Assign which observations are modeled with an outlier error term
#'
#' Determines which observations should be assigned an outlier error in the model fit.
#' Uses either pre-specified outlier indicators from the data, or an algorithm based
#' on observation year, data series type, and country.
#'
#' @param dat A data frame with survey observations. May contain columns:
#'   \describe{
#'     \item{possible_outlier}{Binary indicator if observation is a possible outlier}
#'     \item{possible_outlier_userinput}{User override for possible_outlier}
#'     \item{record_id_fixed}{Unique observation identifier (required if outlier_record_ids provided)}
#'     \item{data_series_type}{Type of data series (e.g., "DHS", "National survey", "Other")}
#'     \item{any_bias}{Binary indicator for known biases}
#'     \item{iso}{Country ISO code}
#'     \item{year}{Observation year}
#'   }
#' @param outlier_record_ids Optional vector of record_id_fixed values to flag as
#'   possible outliers. Typically obtained from step 1a fit using
#'   \code{\link{identify_outliers_in_global_fit}}.
#'
#' @return The input data frame with additional column `nooutlier` (1 = not an outlier,
#'   0 = possible outlier that gets an outlier error term).
#'
#' @details
#' The function uses the following priority for determining outliers:
#' 1. If `possible_outlier_userinput` is provided, use that
#' 2. If `possible_outlier` column exists, use that
#' 3. Otherwise, apply algorithm: DHS surveys before 1990, or observations with
#'    known biases, are flagged as possible outliers
#'
#'
#' @keywords internal
assign_outliers <- function(dat,
                            outlier_record_ids = NULL # needed for 1b, when adding outlier candidates
){

  if (!is.null(outlier_record_ids)){
    print("we use outlier record IDs and outlier_record_ids")
    if (!("record_id_fixed" %in% names(dat))){
      stop("Column record_id_fixed not included in data, we need that!")
    }
    if (any(is.na(dat$record_id_fixed))){
      stop("Missing values in record_id_fixed not allowed!")
    }
    #print(mean(dat$nooutlier))
    dat <- dat %>%
      mutate(outlying_obs = ifelse (record_id_fixed %in% outlier_record_ids, 1, 0))
  } else {
    dat <- dat %>%
      mutate(outlying_obs = 0)
  }

  if ("possible_outlier" %in% names(dat)){
    if ("possible_outlier_userinput" %in% names(dat)){
      if (any(!is.na(dat$possible_outlier_userinput))){
        print("We define outlier error based on columns possible_outlier and possible_outlier_userinput")
        print("and outlying recordID, if provided")
        dat <- dat %>%
          mutate(nooutlier = 1 - ifelse(is.na(possible_outlier_userinput), possible_outlier, possible_outlier_userinput),
                 nooutlier = ifelse(outlying_obs == 1, 0, nooutlier))
        return(dat)
      }
    }
    if (any(is.na(dat$possible_outlier))){
      print("Column possible outliers has NAs, it will be regenerated")
    } else {
      print("We define possible outliers based on column possible_outlier")
      print("and outlying recordID, if provided")
      dat <- dat %>%
        mutate(nooutlier = 1 - possible_outlier,
               nooutlier = ifelse(outlying_obs == 1, 0, nooutlier))
      return(dat)
    }
  } else {
    print("Column possible_outlier not included in data, no pre-defined outliers used")
  }



  # Note: Using year as proxy for start_date; update when actual start_date available
  dat <- dat %>%
    mutate(start_date = year) %>%
    mutate(isafter1990 = ifelse(start_date > 1990, 1, 0),
           yesoutlier = case_when(
             outlying_obs == 1 ~ 1,
            any_bias == 1 ~ 1,
             # dhs may be an outlier if only before 1990
             data_series_type == "DHS" & !isafter1990 ~ 1,
             .default = 0
           )) %>%
    group_by(iso) %>%
    mutate(ndhs_nonoutlying = sum(data_series_type == "DHS" & !yesoutlier),
           # apply recency by counting after 1990
           nnationalsurvey_nonoutlying = sum(data_series_type == "National survey" & !yesoutlier & isafter1990),
           nother_nonoutlying = sum(data_series_type == "Other"  & !yesoutlier & isafter1990)
    ) %>%
    ungroup() %>%
    mutate(
      nooutlier = case_when(
        yesoutlier == 1  ~ 0,
        data_series_type == "DHS"  ~ 1,
        # do assign as reference also prior to 1990
        ndhs_nonoutlying == 0 & nnationalsurvey_nonoutlying >= nother_nonoutlying &
          data_series_type == "National survey" ~ 1,
        ndhs_nonoutlying == 0 & nnationalsurvey_nonoutlying < nother_nonoutlying &
          data_series_type == "Other" ~ 1,
        .default = 0
      )) %>%
    group_by(iso) %>%
    #    group_by(name_country) %>%
    # to come back to:
    # if nothing else, use the most recent non-outlying point in a country
    # but this will not result in anything new...
    mutate(noreferenceyet = ifelse(sum(nooutlier) == 0 , 1, 0),
           most_recent_notoutlying = max(c(0, # just a small number added to avoid -Inf warnings
                                           start_date[yesoutlier == 0]))) %>%
    ungroup() %>%
    mutate(nooutlier = ifelse(noreferenceyet & start_date == most_recent_notoutlying, 1, nooutlier),
    possible_outlier = 1- nooutlier, possible_outlier_userinput = NA)
  return(dat)
}

# add_bias_info <- function(data, married){
#   data %>%
#     # added for 2024 data set
#     mutate(modern_method_bias = ifelse(is.na(modern_method_bias), "None", modern_method_bias)) %>%
#     mutate(any_bias = ifelse(modern_method_bias == "None" # modern applies to all
#                              & has_geographical_region_bias == "N"
#                              & has_non_pregnant_and_other_positive_biases == "N"
#                              & age_group_bias == "None"
#                              #(TRUE|FALSE)&TRUE
#                              & ( (married == TRUE & group_type_relative_to_baseline == "MW") |
#                                    (married  == FALSE & group_type_relative_to_baseline == "UW") ),
#                              0, 1),
#            trad_bias = ifelse(has_traditional_method_bias == "N"
#                               & has_absence_of_probing_questions_bias == "N", 0, 1),
#            tradorany_bias = ifelse(any_bias ==1 | trad_bias ==1, 1, 0))
# }



#' identify_outliers_in_global_fit
#'
#' @param fit Global fit object with posterior etas and data set that has record_id_fixed
#'
#' @return vector with record_id_fixed of outliers
#' @keywords internal
#'
identify_outliers_in_global_fit <- function(fit){
  # needs posteriors
  if (is.null( fit$posteriors$temporal))
    stop("needs posteriors")
  #   fit$posteriors <- process_fit(fit, parallel_chains = 8,
  #                                 save_eps = FALSE,use_d_param  = TRUE,
  #                                 save_nontemporal  = FALSE,
  #                                 just_one_indicator = FALSE,
  #                                 # for step 2
  #                                 add_trad = FALSE)
  # }
  # to do: replace with eta_i stuff later
  res <- fit$posteriors$temporal %>%
    dplyr::select(variable, c, t, '50%', iso, year) %>%
    tidyr::pivot_wider(names_from = variable, values_from = '50%')

  res2 <- tibble(c = fit$stan_data$geo_unit, t = fit$stan_data$time,
                 nooutlier = fit$stan_data$nooutlier,
                 record_id_fixed = fit$record_id_fixed_used,
                 y = fit$stan_data$y) %>%
    left_join(res) %>%
    mutate(error_y = y - inv_probit(eta))

  cutoff_y <- quantile(abs(res2$error_y), 0.9)
  res2 %>%
    filter(abs(error_y) > cutoff_y  | nooutlier == 0) %>%
    pull(record_id_fixed)
}


# # code used to add record_id_fixed
# # how to merge...
# # does data have ID?
# # yes but NA for recent obs...
# names(fit$data)
# fit$data %>%
#   filter(is.na(record_id))
# # just get the numbers used, prob an easier way :)
# numbers_used <- as.numeric(unlist(lapply(strsplit(fit$data$record_id, "_"), function(x) x[2])))
# max_number <- max(numbers_used, na.rm = TRUE)
# max_number
#
# dat <- readr::read_csv(here::here("data-raw/Track20Database031023.csv"), show_col_types = FALSE)
# dat <- dat %>%
#   mutate(record_id_fixed = ifelse(
#     !is.na(record_id), record_id,
#     paste0(is_in_union, "_", seq(3001, 3001 + n() -1))))
# dat$record_id_fixed
# write.csv(dat, here::here("data-raw/Track20Database031023_wrecordid.csv"))
# just introduced extra column at start
# to remove again
