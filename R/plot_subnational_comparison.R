
#' Plot subnational comparison
#'
#' @param results Model fit.
#' @param results2 Option model fit 2. If NULL, only one model will be plotted.
#' @param model_names Optional names for the models to be plotted.
#' Should be of length 2 if results2 is not NULL, otherwise length 1.
#' @param year_select Year to plot. Should be a single value.
#' @param ymin_select Minimum y-axis value. Default is 0.
#' @param ymax_select Maximum y-axis value. Default is NA (no limit).
#' @param arrange_point Boolean to arrange points by median value. Default is TRUE.
#'
#' @returns ggplot object with comparison plot
#' @export
plot_subnational_comparison <- function(results,
                                        results2 = NULL,
                                        model_names = c("Bayesian Model 1",
                                                        "Bayesian Model 2"),
                                        year_select,
                                        ymin_select = 0, ymax_select = NA,
                                        arrange_point = TRUE) {

  if (is.null(results2)){
    results_all <- results$posteriors$temporal|>
      dplyr::mutate(model = model_names[1])
  } else {
    results_all <- results$posteriors$temporal |>
          dplyr::mutate(model = model_names[1]) |>
      dplyr::bind_rows(
        results2$posteriors$temporal |>
          dplyr::mutate(model = model_names[2]))
  }
  res <- results_all |>
    dplyr::filter(year == year_select) |>
    # dplyr::mutate(
    #   region_code = factor(admin1) |>
    #     forcats::fct_relevel("National") |>
    #     forcats::fct_rev()) |>
    dplyr::rename(median = '50%')
  # remove national
  res <- res |>
    dplyr::filter(admin1 != "National")
  # clean names
  # if !  _ in admin1, then region_code is admin1
  if (!all(stringr::str_detect(res$admin1, "_"))){
    res$region_code <- res$admin1
  } else {
    # otherwise split by _ and take second part
    res$region_code <- purrr::map_chr(stringr::str_split(res$admin1, "_"), function(l) l[2])
  }

  if (arrange_point){
    res <-  res |>
      dplyr::mutate(region_code = forcats::fct_reorder(region_code, median))
  }
  res |>
    ggplot2::ggplot(ggplot2::aes(x = region_code, y = median,
                                 # these colors don't work as red/black but differences show up :)
           #                         color = ifelse(region_code == "National", "red", "black")
                                    color = model
           #,
    )) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = `2.5%`, ymax = `97.5%`),
                             position = ggplot2::position_dodge(width = 0.5) #"dodge2"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Region", y = "Proportion",
                  title = paste0(res$iso[1], " - ", gsub("_", " ", indicator_select), ", ", year_select)) +
    ggplot2::ylim(ymin_select, ymax_select) +
    #ggplot2::theme(legend.position = "none") +
    #guides(color = guide_legend(title="New Legend Title"))
    ggplot2::coord_flip()
}

plot_subnational_comparison_acrossyears <- function(results,
                                        year_select,
                                        ymin_select = 0, ymax_select = NA,
                                        arrange_point = TRUE) {

  results_all <- results$posteriors$temporal
  res <- results_all |>
    dplyr::filter(year %in% year_select) |>
    dplyr::rename(median = '50%')
  # remove national
  res <- res |>
    dplyr::filter(admin1 != "National")
  # clean names
  # if !  _ in admin1, then region_code is admin1
  if (!all(stringr::str_detect(res$admin1, "_"))){
    res$region_code <- res$admin1
  } else {
    # otherwise split by _ and take second part
    res$region_code <- purrr::map_chr(stringr::str_split(res$admin1, "_"), function(l) l[2])
  }

  if (arrange_point){
    res <-  res |>
      dplyr::mutate(region_code = forcats::fct_reorder(region_code, median))
  }
  res |>
    ggplot2::ggplot(ggplot2::aes(x = region_code, y = median,
                                 # these colors don't work as red/black but differences show up :)
                                 #                         color = ifelse(region_code == "National", "red", "black")
                                 color = as.factor(year)
                                 #,
    )) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = `2.5%`, ymax = `97.5%`),
                             position = ggplot2::position_dodge(width = 0.5) #"dodge2"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Region", y = "Proportion",
                  title = paste0(res$iso[1], " - ", gsub("_", " ", indicator_select), ", ", year_select)) +
    ggplot2::ylim(ymin_select, ymax_select) +
    #ggplot2::theme(legend.position = "none") +
    #guides(color = guide_legend(title="New Legend Title"))
    ggplot2::coord_flip()
}
