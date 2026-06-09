#' Backend wrapper functions for cmdstanr and rstan compatibility
#'
#' These internal functions provide a unified interface for extracting
#' draws and summaries from both cmdstanr and rstan fit objects.
#'
#' The main compatibility issues addressed:
#' - **Output extraction**: Different APIs for extracting draws and summaries
#' - **Formula support**: Uses posterior::summarise_draws for consistent formula handling
#' - **Initialization**: rstan requires explicit array dimensions (see fix_init_dims_for_rstan in inits.R)
#' - **Data preparation**: rstan requires explicit matrix/vector structures (see fit_model.R lines 913-995)
#'
#' @name backend_wrappers
#' @keywords internal
NULL

#' Extract draws from either cmdstanr or rstan fit object
#'
#' Converts both backends to a unified draws_array format for compatibility
#' with tidybayes, bayesplot, and other posterior analysis packages.
#'
#' @param fit A fitted model object containing a `samples` component and `backend` indicator
#' @param variables Character vector of variable names to extract (ignored, all params extracted)
#'
#' @return A draws_array object from the posterior package
#' @keywords internal
extract_draws <- function(fit, variables = NULL) {
  # Convert both backends to unified draws_array format
  # This ensures consistent behavior with tidybayes::spread_draws() and bayesplot

  if (inherits(fit$samples, "stanfit")) {
    # For rstan: extract all parameters as array
    draws <- rstan::extract(fit$samples, permuted = FALSE, inc_warmup = FALSE)
    return(posterior::as_draws_array(draws))

  } else if (inherits(fit$samples, "CmdStanMCMC")) {
    # For cmdstanr: use built-in draws() method, returns draws_array
    return(fit$samples$draws())

  } else {
    stop("Unknown samples object class: ", paste(class(fit$samples), collapse = ", "))
  }
}

#' Get summary statistics from either cmdstanr or rstan fit object
#'
#' @param fit A fitted model object containing a `samples` component and `backend` indicator
#' @param variables Character vector of variable names to summarize
#' @param funs Optional function, formula, or list of functions/formulas to apply for summary statistics.
#'   Examples:
#'   - Function: `mean`, `sd`
#'   - Formula: `~ quantile(.x, probs = c(0.025, 0.5, 0.975))`
#'   - List: `list(mean = mean, sd = sd)`
#' @param .cores Number of cores to use for parallel processing (cmdstanr only)
#'
#' @return A tibble with summary statistics. Column names depend on the functions used.
#' @keywords internal
#' @importFrom tibble as_tibble
extract_summary <- function(fit, variables, funs = NULL, .cores = 1) {
  # Robust backend detection - check actual object class first
  # This prevents mismatches where fit$backend might be set incorrectly
  if (inherits(fit$samples, "stanfit")) {
    backend <- "rstan"
  } else if (inherits(fit$samples, "CmdStanMCMC")) {
    backend <- "cmdstanr"
  } else if (!is.null(fit$backend)) {
    # Fall back to stored backend if class detection fails
    backend <- fit$backend
    warning(sprintf(
      "Could not auto-detect backend from samples class (%s). Using stored backend: %s",
      paste(class(fit$samples), collapse = ", "), backend))
  } else {
    # Last resort: default to cmdstanr for backward compatibility
    backend <- "cmdstanr"
    warning(sprintf(
      "Could not detect backend. Samples class: %s. Defaulting to cmdstanr",
      paste(class(fit$samples), collapse = ", ")))
  }

  if (backend == "cmdstanr") {
    # Verify this is actually a CmdStanMCMC object before calling $summary
    if (!inherits(fit$samples, "CmdStanMCMC")) {
      stop(sprintf(
        "Backend is 'cmdstanr' but samples object is class '%s', not 'CmdStanMCMC'. ",
        paste(class(fit$samples), collapse = ", "),
        "This indicates a backend mismatch. Check that fit$backend matches the actual samples object."
      ))
    }
    # cmdstanr has built-in summary method
    if (is.null(funs)) {
      return(fit$samples$summary(variables))
    } else {
      return(fit$samples$summary(variables, funs, .cores = .cores))
    }

  } else if (backend == "rstan") {
    # rstan: use summary() or compute from draws
    if (is.null(funs)) {
      # Use rstan's built-in summary
      summ <- rstan::summary(fit$samples, pars = variables)$summary
      return(tibble::as_tibble(summ, rownames = "variable"))

    } else {
      # For custom functions, use posterior::summarise_draws which handles formulas and functions
      # This is what cmdstanr uses internally, so it ensures consistency

      # Extract draws and convert to posterior format
      draws_array <- rstan::extract(fit$samples, pars = variables,
                                     permuted = FALSE, inc_warmup = FALSE)

      # Use posterior::summarise_draws which handles formulas, functions, and lists
      # This automatically converts formulas to functions and handles multi-value returns
      result <- posterior::summarise_draws(draws_array, funs)

      return(result)
    }

  } else {
    stop("Unknown backend: ", backend, ". Must be 'cmdstanr' or 'rstan'.")
  }
}

#' Check if rstan is available
#'
#' @return Logical indicating if rstan package is installed
#' @keywords internal
has_rstan <- function() {
  requireNamespace("rstan", quietly = TRUE)
}

#' Check if cmdstanr is available
#'
#' @return Logical indicating if cmdstanr package is installed
#' @keywords internal
has_cmdstanr <- function() {
  requireNamespace("cmdstanr", quietly = TRUE)
}
