#' Plot model fits, with options to add routine data or additional fits for comparison.
#'
#' @param results Model fit.
#' @param dat_routine Optional routine data to add to model estimates for comparison.
#' @param indicator_name Custom title for the y label of the plot. If `NULL`, name will be pulled from model fit.
#' @param results2 Optional second model fit.
#' @param results3 Optional third model fit.
#' @param results4 Optional fourth model fit.
#' @param modelnames Optional names of the models to display in the plot legend.
#' @param iso_codes Optional character vector of ISO codes to plot. If NULL, plots all countries.
#' @param save_plots Boolean indicator, if set to TRUE plots will be saved in output directory of results fit.
#' @param output_folder Folder to save plots in, if save_plots is TRUE, to overwrite where plots are saved.
#' @param plot_name Plot name if saved as pdf. Defaults to "fit".
#'
#' @export
plot_estimates_local_all <- function(results,
                                     dat_routine = NULL,
                                     indicator_name = NULL,
                                     results2 = NULL,
                                     results3 = NULL,
                                     results4 = NULL,
                                     modelnames =  c("model1", "model2", "model3", "model4"),
                                     iso_codes = NULL,
                                     save_plots = FALSE,
                                     output_folder = NULL,
                                     add_caption = FALSE,
                                     add_estimates = TRUE,
                                     use_for_facetting = FALSE,
                                     plot_name = "fit"){


  plot_caption <- "Survey data from DHS and MICS are shown in red and blue, with vertical bars indicating how uncertain each survey point is.\nIf available, routine data from CAM2024 are shown in black. The lines represent the model's point estimates with shaded areas\nhighlighting uncertainty (in red if routine data were included, black/green otherwise)."

  subnational <- ifelse("admin1" %in% names(results$posteriors$temporal), TRUE, FALSE)

  geo_col <- ifelse(subnational, "admin1", "iso")

  # Get unique country or subnational codes
  model_country_codes <- results$posteriors$temporal[[geo_col]] |> unique()

  # subnational future additions
  if(!is.null(dat_routine)){
    routine_country_codes <- dat_routine |> dplyr::pull(!!rlang::sym(geo_col)) |> unique()
    plot_name <- paste0(plot_name, "_wroutinedata")

    country_codes <- model_country_codes[model_country_codes %in% routine_country_codes]
  }
  else {
    country_codes <- model_country_codes
  }

 # Filter to requested ISO codes if provided
  if (!is.null(iso_codes)) {
    missing_codes <- iso_codes[!iso_codes %in% country_codes]
    if (length(missing_codes) > 0) {
      warning("Some iso_codes not found in fit: ", paste(missing_codes, collapse = ", "))
    }
    country_codes <- country_codes[country_codes %in% iso_codes]
    if (length(country_codes) == 0) {
      stop("None of the requested iso_codes found in the fit.")
    }
  }

  fit_data <- results$data

  # Collect all survey types across all countries to be plotted for consistent legends
  all_survey_types <- fit_data |>
    dplyr::filter(.data[[geo_col]] %in% country_codes, !is.na(est_indicator)) |>
    dplyr::pull(data_series_type) |>
    unique()

  if(is.null(indicator_name)){
    indicator_name <- results$data$indic |> unique()
    if(length(indicator_name) > 1){
      indicator_name <- "ANC4"
    }
  }

  plot_list <- list()

  for (i in 1:length(country_codes)) {

    filtered_data <- fit_data |> dplyr::filter(.data[[geo_col]] == country_codes[i])

    if (all(is.na(filtered_data$est_indicator))) {
      filtered_data <- NULL
    } else {
      filtered_data <- filtered_data |> dplyr::filter(!is.na(est_indicator))
    }

    estimates <- results$posteriors$temporal |> dplyr::filter(.data[[geo_col]] == country_codes[i])

    estimates2 <- if (!is.null(results2)) results2$posteriors$temporal |> dplyr::filter(.data[[geo_col]] == country_codes[i]) else NULL
    estimates3 <- if (!is.null(results3)) results3$posteriors$temporal |> dplyr::filter(.data[[geo_col]] == country_codes[i]) else NULL
    estimates4 <- if (!is.null(results4)) results4$posteriors$temporal |> dplyr::filter(.data[[geo_col]] == country_codes[i]) else NULL

    p <- plot_estimates_local(estimates,
                              filtered_data,
                              estimates2 = estimates2,
                              estimates3 = estimates3,
                              estimates4 = estimates4,
                              modelnames = modelnames,
                              indicator_name = indicator_name,
                              add_estimates = add_estimates,
                              all_survey_types = all_survey_types)


    if (!is.null(results$dat_routine)){
      dat_routine_select <-
        results$dat_routine |>
        dplyr::filter(.data[[geo_col]] == country_codes[i]) |>
        dplyr::mutate(included = "Yes", data_series_type = "Routine data")
      if (dim(dat_routine_select)[1] > 0){
        p <- p +
          #ggplot2::geom_errorbar(data = dat_routine_select,
          #              ggplot2::aes(x = year, y = routine_value, ymin = routine_lower, ymax = routine_upper,
          #                  colour = data_series_type), alpha = 0.3) +
          ggplot2::geom_point(data = dat_routine_select,
                     ggplot2::aes(x = year, y = routine_value, # routine_value in original version
                         colour = data_series_type))
      }
    }

    if(!is.null(dat_routine)){
      dat_routine_select <- dat_routine |>
        dplyr::filter(.data[[geo_col]] == country_codes[i]) |>
        dplyr::mutate(included = "No", data_series_type = "Routine data")
      if (dim(dat_routine_select)[1] > 0){
        p <- p +
          #ggplot2::geom_errorbar(data = dat_routine_select,
          #              ggplot2::aes(x = year, y = routine_value, ymin = routine_lower, ymax = routine_upper,
          #                  colour = data_series_type), alpha = 0.3) +
          ggplot2::geom_point(data = dat_routine_select,
                     ggplot2::aes(x = year, y = routine_value, # routine_value in original version
                         colour = data_series_type))
      }
    }

    if (!is.null(filtered_data)){
      plot_title <- ifelse(subnational, filtered_data |> dplyr::pull(level) |> unique(), filtered_data |> dplyr::pull(country) |> unique())
      iso_select <- filtered_data$iso[1]
    } else {
      plot_title <- country_codes[i]
      iso_select <- NULL
    }

    # add caption
    if (add_caption)
      p <- p + ggplot2::labs(caption = plot_caption)
    # note for toy plots
    # when using annotate, we can no longer combine plots later on (and share legends across plots)
    if (use_for_facetting){
      plot_list[[plot_title]] <- p + ggplot2::ggtitle(paste0(plot_title, " (",iso_select, ")")) #+ ggplot2::theme(legend.position = "none")
    } else {
      plot_list[[i]] <- ggpubr::annotate_figure(p, top = ggpubr::text_grob(paste0(plot_title, " (",country_codes[i], ")"),
                                             face = "bold", size = 14))
      # temp: only show plot title region (iso)
      # and show National if missing
      plot_title <- ifelse(is.na(plot_title), "National aggregate", plot_title)
      plot_list[[i]] <- ggpubr::annotate_figure(p, top = ggpubr::text_grob(paste0(plot_title, " (",iso_select, ")"),
                                                           face = "bold", size = 14))
    }
  }

  if (save_plots) {
    if (!is.null(output_folder)) {
      output_dir <- output_folder
    } else if (dir.exists(results$output_dir)) {
      output_dir <- results$output_dir
    } else {
      stop("Please provide a valid output_folder to save the plot in.")
    }
    pdf(file.path(output_dir, paste0(plot_name,".pdf")), width = 6, height = 6)
    for (plot in plot_list) {
      print(plot)
    }
    dev.off()
  }

  return(plot_list)
}

#' Plot local model fits.
#'
#'
#' @param estimates A data frame estimates from the model fit.
#' @param filtered_data A data frame containing data from the model fit.
#' @param estimates2 Optional estimates from a second model.
#' @param estimates3 Optional estimates from a third model.
#' @param estimates4 Optional estimates from a fourth model.
#' @param modelnames A vector of model names used for plotting. Default is `c("model1", "model2", "model3", "model4")`.
#' @param indicator_name Name of indicator used for plotting.
#' @param add_estimates Boolean to show model estimates/ribbons.
#' @param all_survey_types Optional character vector of all survey types to include in legend (for consistent legends across multiple plots).
#' @param cols_sourcetypes A named vector of colors for the different data series types (e.g., DHS, MICS, etc.).
#'
#' @keywords internal

plot_estimates_local <- function(estimates,
                                 filtered_data,
                                 estimates2 = NULL,
                                 estimates3 = NULL,
                                 estimates4 = NULL,
                                 modelnames,
                                 indicator_name,
                                 add_estimates = TRUE,
                                 all_survey_types = NULL,
                                 cols_sourcetypes = c("DHS" = "red",
                                                      "DHS0" = "red",
                                                      "MICS" = "blue",
                                                      "PMA" = "darkgreen",
                                                      "Other" = "orange",
                                                      "National survey" = "purple",
                                                      "NSS" = "deepskyblue2",
                                                      "Routine data" = "black")
                                 ) {

  #model <- NULL
  # temp fix for model names when just plotting one model
  estimates <-
    estimates |>
    dplyr::mutate(model = modelnames[1])

  if (!is.null(estimates2)){
    estimates <- dplyr::bind_rows(
      estimates, # |> dplyr::mutate(model = modelnames[1]),
      estimates2 |> dplyr::mutate(model = modelnames[2])
    )
  }
  if (!is.null(estimates3)){
    estimates <- dplyr::bind_rows(
      estimates,
      estimates3 |> dplyr::mutate(model = modelnames[3])
    )
  }
  if (!is.null(estimates4)){
    estimates <- dplyr::bind_rows(
      estimates,
      estimates4 |> dplyr::mutate(model = modelnames[4])
    )
  }

  # Filter out empty model names (padding from compare_fits)
  estimates <- estimates |> dplyr::filter(model != "")

  # Get unique model names for consistent ordering
  model_levels <- unique(estimates$model)

  p <- estimates |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = `50%`)) +
    ggplot2::labs(x = "Year", y = indicator_name) +
    ggplot2::theme_bw() +
    ggplot2::expand_limits(y = 0)
  if (add_estimates){
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = `2.5%`, ymax = `97.5%`, fill = model),
                  colour = NA, alpha = 0.3, show.legend = FALSE) +
      ggplot2::geom_line(ggplot2::aes(color = model, lty = model), linewidth = 1.2)
    #+
    #  ggplot2::scale_color_discrete(name = "Model", limits = model_levels) +
    #  ggplot2::scale_linetype_discrete(name = "Model", limits = model_levels)
  }

  if (!is.null(filtered_data)) {
    filtered_data <- filtered_data |>
      dplyr::mutate(included = ifelse(held_out == 1, "No", "Yes"))

    # Use all_survey_types if provided for consistent legend across plots
    survey_limits <- if (!is.null(all_survey_types)) all_survey_types else NULL

    p <- p +
      ggnewscale::new_scale_color() +
      ggplot2::geom_errorbar(data = filtered_data,
                    ggplot2::aes(y = est_indicator, ymin = low_indicator,
                        ymax = up_indicator, color = data_series_type), alpha = 0.3, width = 0.1) +
      ggplot2::geom_point(data = filtered_data,
                 ggplot2::aes(y = est_indicator, x = year, color = data_series_type)) +
      ggplot2::scale_colour_manual(values = cols_sourcetypes, name = "Survey type",
                          limits = survey_limits, drop = FALSE)
  }

  return(p)
}

