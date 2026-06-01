# Test script for cmdstanr vs rstan backend compatibility
# This script verifies that both backends produce consistent results

#library(dplyr)
#library(tibble)

#library(haven)
#library(readr)


# Check if packages are available
has_cmdstanr <- requireNamespace("cmdstanr", quietly = TRUE)
has_rstan <- requireNamespace("rstan", quietly = TRUE)
has_posterior <- requireNamespace("posterior", quietly = TRUE)

if (has_rstan){
  library(rstan)
  rstan_options(auto_write = TRUE)
  library(posterior)

}
if (has_cmdstanr){
  library(cmdstanr)
}


cat("Package availability:\n")
cat("  cmdstanr: ", has_cmdstanr, "\n")
cat("  rstan:    ", has_rstan, "\n")
cat("  posterior:", has_posterior, "\n\n")

if (!has_cmdstanr && !has_rstan) {
  stop("Neither cmdstanr nor rstan is installed. Install at least one to test.")
}

# Load the package (assumes devtools::load_all() or installed package)
if (!require(bayescoveragemodel, quietly = TRUE)) {
  stop("bayescoveragemodel package not loaded. Run devtools::load_all() first.")
}

#------------------------------------------------------------------------------
# Helper function to create minimal test data
#------------------------------------------------------------------------------
create_test_data <- function(seed = 123) {
  set.seed(seed)

  # # Create minimal dataset for testing
  # # This is a simplified version - adjust based on your actual data requirements
  # n_obs <- 30
  # n_countries <- 3
  #
  # data <- tibble::tibble(
  #   iso = rep(c("USA", "CAN", "MEX"), each = n_obs / n_countries),
  #   year = rep(2010:2019, length.out = n_obs),
  #   cluster = rep(c("Americas", "Americas", "Americas"), each = n_obs / n_countries),
  #   subcluster = rep(c("North America", "North America", "North America"),
  #                    each = n_obs / n_countries),
  #   indic = "ANC4",
  #   # Simulated coverage indicators on inv-probit scale
  #   invprobit_indicator = rnorm(n_obs, mean = 0.5, sd = 0.3),
  #   se_invprobit_indicator = runif(n_obs, 0.05, 0.15),
  #   data_series_type = "DHS",
  #   nooutlier = 1,
  #   any_bias = 0,
  #   record_id_fixed = 1:n_obs
  # )

  # Read national survey data and region metadata
  data_folder <- "data_raw"
  dat0 <- haven::read_dta(here::here(data_folder, "ICEH_national.dta"))
  regions_dat <- readr::read_csv(
    here::here(data_folder, "regions_updated.csv"))

  # Choose an indicator: "anc4", "ideliv", or "vdpt"
  indicator_select <- "anc4"

  # Process data
  data <- process_data(
    dat = dat0,
    regions_dat = regions_dat,
    indicator = indicator_select
  )

  return(data)
}



test_fitting <- function() {
  cat("Creating test data...\n")
  test_data <- create_test_data()

  results <- list()
  fits <- list()

  # Minimal settings for fast testing
  fit_args <- list(
    survey_df = test_data,
    runstep = "local_national",#"step1a",#
    chains = 1,
    iter_sampling = 5,
    iter_warmup = 5,
    refresh = 0,
    seed = 123,
    # code for summary object does NOT work for rstan
    save_post_summ = FALSE
    #,
  # get_posteriors = FALSE,  # Skip posterior processing for speed
   # create_runname_and_outputdir = FALSE
  )

  if (has_cmdstanr) {
    cat("\nFitting with cmdstanr backend...\n")
    tryCatch({
      fit_cmdstan <- do.call(fit_model, c(fit_args, list(backend = "cmdstanr")))
      cat("✓ cmdstanr fit completed\n")
      results$cmdstanr <- "success"
      fits$cmdstanr <- fit_cmdstan
    }, error = function(e) {
      cat("✗ cmdstanr fit FAILED:", conditionMessage(e), "\n")
      results$cmdstanr <- paste("failed:", conditionMessage(e))
    })
  }

  if (has_rstan) {
    cat("\nFitting with rstan backend...\n")
    tryCatch({
      fit_rstan <- do.call(fit_model, c(fit_args, list(backend = "rstan")))
      cat("✓ rstan fit completed\n")
      results$rstan <- "success"
      fits$rstan <- fit_rstan
    }, error = function(e) {
      cat("✗ rstan fit FAILED:", conditionMessage(e), "\n")
      results$rstan <- paste("failed:", conditionMessage(e))
    })
  }

  cat("\n")
  return(list(results = results, fits = fits))
}

#library(localhierarchy)
#devtools::load_all(here::here())
library(bayescoveragemodel)
rstan_options(auto_write = TRUE)
fitting_results <- test_fitting()

# wroks for step1a, just note that we cant save posterior summaries with rstan
#

#------------------------------------------------------------------------------
# Test 3: Wrapper Functions (extract_draws and extract_summary)
#------------------------------------------------------------------------------
cat(strrep("=", 70), "\n")
cat("TEST 3: Wrapper Functions\n")
cat(strrep("=", 70), "\n\n")

test_wrappers <- function(fits) {
  results <- list()

  for (backend_name in names(fits)) {
    cat(sprintf("\nTesting wrappers with %s fit...\n", backend_name))
    fit <- fits[[backend_name]]

    # Test extract_draws
    cat("  - extract_draws()... ")
    tryCatch({
      draws <- extract_draws(fit, "Omega")
      cat("✓ (", nrow(as.matrix(draws)), " draws)\n", sep = "")
      results[[backend_name]]$extract_draws <- "success"
    }, error = function(e) {
      cat("✗ FAILED:", conditionMessage(e), "\n")
      results[[backend_name]]$extract_draws <- paste("failed:", conditionMessage(e))
    })

    # Test extract_summary
    cat("  - extract_summary()... ")
    tryCatch({
      summ <- extract_summary(fit, "Omega")
      cat("✓ (", nrow(summ), " parameters)\n", sep = "")
      results[[backend_name]]$extract_summary <- "success"
    }, error = function(e) {
      cat("✗ FAILED:", conditionMessage(e), "\n")
      results[[backend_name]]$extract_summary <- paste("failed:", conditionMessage(e))
    })
  }

  cat("\n")
  return(results)
}

if (length(fitting_results$fits) > 0) {
  wrapper_results <- test_wrappers(fitting_results$fits)
} else {
  cat("Skipping wrapper tests (no successful fits)\n\n")
  wrapper_results <- NULL
}

#------------------------------------------------------------------------------
# Test 4: Downstream Functions
#------------------------------------------------------------------------------
cat(strrep("=", 70), "\n")
cat("TEST 4: Downstream Functions\n")
cat(strrep("=", 70), "\n\n")

test_downstream <- function(fits) {
  results <- list()

  for (backend_name in names(fits)) {
    cat(sprintf("\nTesting downstream functions with %s fit...\n", backend_name))
    fit <- fits[[backend_name]]

    # Test get_eta_samples
    cat("  - get_eta_samples()... ")
    tryCatch({
      eta_samples <- get_eta_samples(fit, year_select = 2015)
      cat("✓ (", nrow(eta_samples), " rows)\n", sep = "")
      results[[backend_name]]$get_eta_samples <- "success"
    }, error = function(e) {
      cat("✗ FAILED:", conditionMessage(e), "\n")
      results[[backend_name]]$get_eta_samples <- paste("failed:", conditionMessage(e))
    })

    # Test add_uncertainty_in_obs
    cat("  - add_uncertainty_in_obs()... ")
    tryCatch({
      data_with_uncertainty <- add_uncertainty_in_obs(fit)
      cat("✓ (", nrow(data_with_uncertainty), " rows)\n", sep = "")
      results[[backend_name]]$add_uncertainty_in_obs <- "success"
    }, error = function(e) {
      cat("✗ FAILED:", conditionMessage(e), "\n")
      results[[backend_name]]$add_uncertainty_in_obs <- paste("failed:", conditionMessage(e))
    })
  }

  cat("\n")
  return(results)
}

if (length(fitting_results$fits) > 0) {
  downstream_results <- test_downstream(fitting_results$fits)
} else {
  cat("Skipping downstream tests (no successful fits)\n\n")
  downstream_results <- NULL
}

#------------------------------------------------------------------------------
# Test 5: Compare Results Between Backends
#------------------------------------------------------------------------------
cat(strrep("=", 70), "\n")
cat("TEST 5: Cross-Backend Comparison\n")
cat(strrep("=", 70), "\n\n")

compare_backends <- function(fits) {
  if (length(fits) < 2) {
    cat("Need at least 2 successful fits to compare. Skipping.\n\n")
    return(NULL)
  }

  backends <- names(fits)
  cat(sprintf("Comparing %s vs %s...\n\n", backends[1], backends[2]))

  # Extract a common parameter from both
  param <- "Omega_estimate"

  cat("Extracting parameter summaries...\n")
  summ1 <- extract_summary(fits[[1]], param)
  summ2 <- extract_summary(fits[[2]], param)

  # Compare means
  if ("mean" %in% names(summ1) && "mean" %in% names(summ2)) {
    mean1 <- summ1$mean
    mean2 <- summ2$mean

    if (length(mean1) == length(mean2)) {
      diff <- mean(abs(mean1 - mean2))
      cat(sprintf("  Mean absolute difference in posterior means: %.4f\n", diff))

      if (diff < 0.1) {
        cat("  ✓ Results are similar (difference < 0.1)\n")
      } else if (diff < 0.5) {
        cat("  ⚠ Results show moderate differences (0.1 < difference < 0.5)\n")
        cat("    This may be due to MCMC sampling variation with few iterations\n")
      } else {
        cat("  ✗ Results show large differences (difference > 0.5)\n")
        cat("    WARNING: Backends may not be equivalent!\n")
      }
    }
  }

  cat("\n")
}

if (length(fitting_results$fits) >= 2) {
  compare_backends(fitting_results$fits)
}

#------------------------------------------------------------------------------
# Final Summary
#------------------------------------------------------------------------------
cat(strrep("=", 70), "\n")
cat("TEST SUMMARY\n")
cat(strrep("=", 70), "\n\n")

print_results <- function(test_name, results) {
  cat(sprintf("%s:\n", test_name))
  if (is.null(results) || length(results) == 0) {
    cat("  (skipped)\n")
  } else {
    for (backend in names(results)) {
      if (is.list(results[[backend]])) {
        cat(sprintf("  %s:\n", backend))
        for (test in names(results[[backend]])) {
          status <- results[[backend]][[test]]
          symbol <- ifelse(status == "success", "✓", "✗")
          cat(sprintf("    %s %s: %s\n", symbol, test, status))
        }
      } else {
        status <- results[[backend]]
        symbol <- ifelse(status == "success", "✓", "✗")
        cat(sprintf("  %s %s: %s\n", symbol, backend, status))
      }
    }
  }
  cat("\n")
}

print_results("1. Compilation", compilation_results)
print_results("2. Model Fitting", fitting_results$results)
print_results("3. Wrapper Functions", wrapper_results)
print_results("4. Downstream Functions", downstream_results)

cat(strrep("=", 70), "\n")
cat("Testing complete!\n")
cat(strrep("=", 70), "\n")

# Note about test data
cat("\nNOTE: This test uses minimal synthetic data and few iterations for speed.\n")
cat("For production use, always use real data with adequate iterations.\n")
