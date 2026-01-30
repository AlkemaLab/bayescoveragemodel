
#' Title
#'
#' @param dat data frame with iso, year, indic, r (indicator), se, source, final_year columns
#' @param regions_dat data frame with iso and cluster columns
#' @param indicator character, default "ideliv"
#'
#' @returns processed data frame
#' @export
#'
#' @examples
process_data <- function(dat,
                         regions_dat,
                         indicator = "ideliv"

  ){
  # data processing to prepare data for model fitting
  # select indicators
  # calculate obs and se on logit scale
  # check for missing SEs and impute
  # add information on bias: zero currently for CD indicators
  # add information on possible outliers:
  # either read from data or add columns based on algorithm




  # filter based on indicator
  if(indicator == "anc4"){
    print("For ANC4, we combined anc42 and anc43 and call it anc4.")
    dat <- dat %>%
      filter(indic %in% c("anc42", "anc43", "anc4")) %>%
      mutate(indic = "anc4")
  }
  else{
    if (!(indicator == "all")){
      dat <- dat %>%
        filter(indic == indicator)
    } else {
      dat <- dat %>%
        mutate(indic = ifelse(is.element(indic, c("anc42", "anc43")), "anc4", indic))
    }
  }

  # add region info
  dat <- dat %>%
    left_join(regions_dat, by = c("iso"))
  if (any(is.na(dat$cluster))){
    print("Some clusters are NA, these observations are excluded for now.")
    print("Observations with missing clusters")
    print(dat %>% filter(is.na(cluster)) %>% dplyr::select(iso, year, cluster))
    dat <- dat %>% filter(!is.na(cluster))
  }

  print("Minor TO DO: Figure out exact reference years, ie different reference period for anc4.")
  print("Right now, we delete column year and rename final_year as year, to take forward")
  dat <- dat %>%
    dplyr::select(-year) %>%
    rename(year = final_year)

  # check SEs, any missing?
  if (any(is.na(dat$se))){
    print("Some SEs are NA, these observations are excluded for now.")
    print("TO DO: add imputation for missing SEs.")
    dat <- dat %>% filter(!is.na(se))
  }

  # print("Minimum sampling error set to 1%")
  dat2_1 <- dat %>%
    rename(indicator = r, data_series_type = source) %>%
    # mutate(se = ifelse(se < 0.01, 0.01, se)) %>%
    # get things on logit scale
    mutate(logit_indicator = logit(indicator),
           se_logit_indicator = get_se_logitprop(prop = indicator, se_prop = se),
           invprobit_indicator = inv_probit(indicator),
           se_invprobit_indicator = get_se_invprobitprop(prop = indicator, se_prop = se))

  # add record_id_fixed if not yet in data set
  if (!("record_id_fixed" %in% names(dat2_1))){
    print("We add record_id_fixed to the data.")
    dat2_1 <- dat2_1 %>%
      mutate(record_id_fixed = seq(1, n()))
  }

  # add information on bias, currently all zero (could also be inside run_step)
  print("Currently no biases assigned.")
  dat2 <- dat2_1 %>%
    mutate(any_bias = 0)
  #   dat <- add_bias_info(dat)

  # # add possible outlier columns
  # dat2 <- add_outlier_related_columns( dat2 %>%
  #                                  mutate(start_date = year))


  # for running the model, we use this function to assign dummies
  # (can be moved inside run_step)
  dat2 <- assign_outliers(dat2)
  #  dat2 %>% dplyr::select(iso, year, data_series_type, possible_outlier, possible_outlier_userinput, nooutlier, any_bias, record_id_fixed)

  print("Remove data that's not DHS or MICS, number of obs removed:")
  dat3 <- dat2 %>%
    filter(data_series_type %in% c("DHS", "MICS"))
  print(nrow(dat2) - nrow(dat3))
  return(dat3)
}

