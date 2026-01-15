
plot_subnational_comparison <- function(results,
                                        results_woroutine = NULL,
                                        model_names = c("Model w routine data",
                                                        "Model w/o routine data"),
                                        year_select,
                                        ymin_select = 0, ymax_select = NA,
                                        arrange_point = TRUE) {

  if (is.null(results_woroutine)){
    results_all <- results$posteriors$temporal|>
      mutate(model = model_names[1])
  } else {
    results_all <- results$posteriors$temporal |>
          mutate(model = model_names[1]) |>
      dplyr::bind_rows(
        results_woroutine$posteriors$temporal |>
          mutate(model = model_names[2]))
  }
  res <- results_all |>
    dplyr::filter(year == year_select) |>
    # dplyr::mutate(
    #   region_code = factor(admin1) |>
    #     forcats::fct_relevel("National") |>
    #     forcats::fct_rev()) %>%
    rename(median = '50%')
  # remove national
  res <- res |>
    dplyr::filter(admin1 != "National")
  # clean names
  # if !  _ in admin1, then region_code is admin1
  if (!all(str_detect(res$admin1, "_"))){
    res$region_code <- res$admin1
  } else {
    # otherwise split by _ and take second part
    res$region_code <- map_chr(str_split(res$admin1, "_"), function(l) l[2])
  }

  if (arrange_point){
    res <-  res |>
      mutate(region_code = fct_reorder(region_code, median))
  }
  res |>
    ggplot2::ggplot(ggplot2::aes(x = region_code, y = median,
                                 # these colors don't work as red/black but differences show up :)
           #                         color = ifelse(region_code == "National", "red", "black")
                                    color = model
           #,
    )) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = `2.5%`, ymax = `97.5%`),
                             position=position_dodge(width = 0.5) #"dodge2"
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
    rename(median = '50%')
  # remove national
  res <- res |>
    dplyr::filter(admin1 != "National")
  # clean names
  # if !  _ in admin1, then region_code is admin1
  if (!all(str_detect(res$admin1, "_"))){
    res$region_code <- res$admin1
  } else {
    # otherwise split by _ and take second part
    res$region_code <- map_chr(str_split(res$admin1, "_"), function(l) l[2])
  }

  if (arrange_point){
    res <-  res |>
      mutate(region_code = fct_reorder(region_code, median))
  }
  res |>
    ggplot2::ggplot(ggplot2::aes(x = region_code, y = median,
                                 # these colors don't work as red/black but differences show up :)
                                 #                         color = ifelse(region_code == "National", "red", "black")
                                 color = as.factor(year)
                                 #,
    )) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = `2.5%`, ymax = `97.5%`),
                             position=position_dodge(width = 0.5) #"dodge2"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Region", y = "Proportion",
                  title = paste0(res$iso[1], " - ", gsub("_", " ", indicator_select), ", ", year_select)) +
    ggplot2::ylim(ymin_select, ymax_select) +
    #ggplot2::theme(legend.position = "none") +
    #guides(color = guide_legend(title="New Legend Title"))
    ggplot2::coord_flip()
}
