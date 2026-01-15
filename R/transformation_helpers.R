#' Get Standard Error on Logit Scale
#'
#' Transforms standard error from proportion scale to logit scale using the delta method.
#'
#' @param prop Proportion value (between 0 and 1)
#' @param se_prop Standard error on proportion scale
#'
#' @return Standard error on logit scale
#' @export
get_se_logitprop <- function(prop, se_prop){
  se_prop/(prop - prop^2)
}

#' Probit Transform (Probit to Proportion)
#'
#' Transforms values from probit scale to proportion scale using the standard normal CDF.
#'
#' @param x Value on probit scale
#'
#' @return Value on proportion scale (between 0 and 1)
#' @export
probit <- function(x) pnorm(x)

#' Inverse Probit Transform (Proportion to InvProbit)
#'
#' Transforms values from proportion scale to inv-probit scale using the inverse standard normal CDF.
#'
#' @param x Value on proportion scale (between 0 and 1)
#'
#' @return Value on inv-probit scale
#' @export
inv_probit <- function(x) qnorm(x)

#' Get Standard Error on invProbit Scale from Proportion
#'
#' Transforms standard error from proportion scale to invprobit scale using the delta method.
#' Uses the derivative of the inverse probit function: SE_probit = SE_prop / dnorm(qnorm(prop))
#'
#' @param prop Proportion value (between 0 and 1)
#' @param se_prop Standard error on proportion scale
#'
#' @return Standard error on probit scale
#' @export
get_se_invprobitprop <- function(prop, se_prop){
  se_prop*1/dnorm(inv_probit(prop))
}

#' Get Standard Error on Proportion Scale from invProbit proportion
#'
#' Transforms standard error from invprobit scale back to proportion scale using the delta method.
#' Uses the derivative of the probit function: SE_prop = SE_invprobit * dnorm(invprobit_value)
#'
#' @param invprobitprop Value on invprobit scale (i.e., qnorm of proportion)
#' @param se_invprobitprop Standard error on invprobit scale
#'
#' @return Standard error on proportion scale
#' @export
get_se_probitofinvprobitprop <- function(invprobitprop, se_invprobitprop){
  se_invprobitprop*dnorm(invprobitprop)
}

#' Back-transform Residuals to Proportion Scale
#'
#' Transforms residuals from probit scale back to the original proportion scale.
#' Computes the response on probit scale, then calculates residuals and standard
#' errors on the proportion scale.
#'
#' @param df Data frame containing columns:
#'   \itemize{
#'     \item \code{level}: Predicted value on probit scale
#'     \item \code{residual}: Residual on probit scale
#'     \item \code{scale}: Standard error on probit scale
#'   }
#'
#' @return Data frame with added columns:
#'   \itemize{
#'     \item \code{response}: Observed value on probit scale (level + residual)
#'     \item \code{residual_ori}: Residual on proportion scale
#'     \item \code{scale_ori}: Standard error on proportion scale
#'   }
#'
#' @importFrom dplyr mutate
#' @export
backtransform_residuals <- function(df){
  df %>%
    mutate(response = level + residual,
       residual_ori = probit(response) - probit(level),
       scale_ori =
         get_se_probitofinvprobitprop(
           invprobitprop = level,
           se_invprobitprop = scale))
}

#' Add Probit Scale Transformation Columns
#'
#' Adds standardized columns for probit/proportion scale transformations.
#' Used by get_residuals_samples and get_inno_samples to ensure consistent
#' column naming and transformations.
#'
#' @param df Data frame to transform
#' @param eta_col Name of column containing eta values on probit scale
#' @param sd_col Name of column containing standard deviation on probit scale,
#'   or NULL to use sd_value
#' @param sd_value Numeric value for sd_y if sd_col is NULL (default: 1)
#' @param y_col Name of column containing y values on proportion scale,
#'   or NULL if y should be computed from residual + yhat
#'
#' @return Data frame with added columns:
#'   \itemize{
#'     \item \code{level_prop}: Eta on probit scale
#'     \item \code{level}: Coverage on proportion scale
#'     \item \code{yhat}: Fitted value on proportion scale (same as level)
#'     \item \code{sd_y}: Standard deviation on probit scale
#'     \item \code{sd_y_prop}: Standard deviation on proportion scale
#'     \item \code{y_prop}: y on probit scale
#'   }
#'   If y_col is NULL and residual column exists, also adds:
#'   \itemize{
#'     \item \code{y}: Reconstructed observation (residual + yhat)
#'   }
#'
#' @importFrom dplyr mutate
#' @keywords internal
add_probit_scale_columns <- function(df, eta_col, sd_col = NULL, sd_value = 1,
                                     y_col = NULL) {
  df <- df %>%
    dplyr::mutate(
      level_prop = .data[[eta_col]],
      level = inv_probit(level_prop),
      yhat = level,
      sd_y = if (!is.null(sd_col)) .data[[sd_col]] else sd_value
    )

  # Add y column if not provided and residual exists

  if (is.null(y_col) && "residual" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(y = residual + yhat)
    y_col <- "y"
  }

  # Add proportion-scale transformations
  df <- df %>%
    dplyr::mutate(
      sd_y_prop = get_se_probitofinvprobitprop(level, sd_y),
      y_prop = if (!is.null(y_col)) probit(.data[[y_col]]) else NA_real_
    )

  df
}
