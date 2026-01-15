#' Add uncertainty to observations
#'
#' This function adds uncertainty to observations using the model fit object.
#'
#' @param fit model_fit object.
#' @param perc_low lower percentile for uncertainty.
#' @param perc_up upper percentile for uncertainty.
#'
#' @importFrom tidybayes spread_draws
#' @importFrom dplyr group_by summarise pull
#' @export
add_uncertainty_in_obs <- function(fit, perc_low = 0.025, perc_up = 0.975){

  yhat_i <- fit$samples$draws("eta_i") %>%
    tidybayes::spread_draws(eta_i[i]) %>%
    group_by(i) %>%
    summarise(mean_yhat = mean(eta_i[i])) %>%
    pull(mean_yhat)

  scale_i <- fit$samples$draws("scale") %>%
    tidybayes::spread_draws(scale[i]) %>%
    group_by(i) %>%
    summarise(mean_scale = mean(scale[i])) %>%
    pull(mean_scale)

  model_data <- fit$data %>%
    as_tibble() %>%
    mutate(yhat = yhat_i, scale = scale_i)

  n_rows <- nrow(model_data)
  n_simulations <- 2000
  simulated_samples <- matrix(NA, nrow = n_rows, ncol = n_simulations)
  set.seed(12345)
  for (i in 1:n_rows) {
    # to do: define transform for obs function!
    #simulated_samples[i, ] <- inv_logit(rnorm(
    simulated_samples[i, ] <- probit(rnorm(
        n = n_simulations,
      #mean = logit(model_data$indicator[i]),
      mean = inv_probit(model_data$indicator[i]),
      sd = model_data$scale[i]
    ))
  }

  if (any(is.na(simulated_samples))) {
    print("Removing NAs from simulated samples to get uncertainty in obs")
  }
  model_data <- model_data %>% mutate(low_indicator = apply(simulated_samples, 1, quantile, probs = perc_low, na.rm = TRUE),
                                      up_indicator = apply(simulated_samples, 1, quantile, probs = perc_up, na.rm = TRUE),
                                      est_indicator = indicator)

  return(model_data)

}
