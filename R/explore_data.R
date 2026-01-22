#' Explore Coverage Indicators Over Time
#'
#' This function performs exploratory data analysis on a dataset containing coverage indicators over time.
#'
#' @param data A data frame containing the coverage indicators and year variable.
#' @param indicator_col A string specifying the column name for the coverage indicator.
#' @param indicator_se_col An optional string specifying the column name for the standard error of the indicator.
#' @param data_types_col An optional string specifying the column name for data types in the data frame.
#' @param group_col A string specifying the column name for first-level grouping (e.g., country).
#' @param region_col An optional string specifying the column name for second-level grouping (e.g., region).
#' @param indicator_name An optional string to use as the title for plots instead of the default "Indicator".
#' @param routine_data An optional data frame containing routine data.
#'
#' @return A series of plots for exploratory data analysis.
#' @import ggplot2
#' @import dplyr
#' @export

explore_data <- function(data, indicator_col, indicator_se_col = NULL, data_types_col = NULL, group_col, region_col = NULL, indicator_name = NULL, routine_data = NULL) {

  # Ensure that the required columns exist in the data
  required_cols <- c(indicator_col, group_col)
  if (!is.null(indicator_se_col)) {
    required_cols <- c(required_cols, indicator_se_col)
  }
  if (!is.null(data_types_col)) {
    required_cols <- c(required_cols, data_types_col)
  }
  if (!is.null(region_col)) {
    required_cols <- c(required_cols, region_col)
  }

  if (!all(required_cols %in% colnames(data))) {
    stop("One or more specified columns are not found in the dataset.")
  }

  if(!"year" %in% colnames(data)){
    stop("Year column not found in the dataset (needs to be named year).")
  }

  # Use 'Indicator' if indicator_name is not provided
  title_name <- if (!is.null(indicator_name)) indicator_name else "Indicator"

  # Calculate the rate of change for the main data
  data <- data %>%
    arrange(year) %>%
    group_by(across(all_of(group_col))) %>%
    mutate(
      year_diff = year - lag(year),
      rate_of_change = get(indicator_col) - lag(get(indicator_col)),
      indicator_level = get(indicator_col),
      annualised_rate_of_change = ifelse(year_diff == 0,
                                         rate_of_change,
                                         rate_of_change / year_diff)
    ) %>%
    ungroup()


  if (!is.null(routine_data)) {
    country_codes <- data %>% pull(CODE_ISO_NUMERIC) %>% unique()

    country_names <- data %>%
      dplyr::select(country, CODE_ISO_NUMERIC) %>%
      unique()

    routine_data <- routine_data %>%
      filter(CODE_ISO_NUMERIC %in% country_codes) %>%
      left_join(country_names, by = "CODE_ISO_NUMERIC") %>% # Join to add the country column from country_names
      dplyr::select(country = country.y, everything(), -country.x) %>%
      arrange(year) %>%
      group_by(across(all_of(group_col))) %>%
      mutate(
        year_diff = year - lag(year),
        routine_rate_of_change = routine_value - lag(routine_value)
      ) %>%
      ungroup()

    routine_country_codes <- routine_data %>% pull(CODE_ISO_NUMERIC) %>% unique()

    data <- data %>%
      filter(CODE_ISO_NUMERIC %in% routine_country_codes)
  }

  # Determine the facet formula
  facet_formula <- if (!is.null(region_col)) {
    as.formula(paste0("~ ", region_col))
  } else {
    as.formula(paste0("~ ", group_col))
  }

  # Dynamically create aes mapping
  aes_mapping <- if (!is.null(data_types_col)) {
    aes(
      x = year,
      y = indicator_level,
      group = get(group_col),
      colour = get(data_types_col)
    )
  } else {
    aes(
      x = year,
      y = indicator_level,
      group = get(group_col)
    )
  }

  # Create a list to store the plots
  plots <- list()

  # Plot time series of the indicator_col with conditional faceting
  p1 <- ggplot(data, aes_mapping) +
    geom_point()

  # Add error bars if indicator_se_col is provided
  if (!is.null(indicator_se_col)) {
    p1 <- p1 +
      geom_errorbar(aes(ymin = indicator_level - get(indicator_se_col), ymax = indicator_level + get(indicator_se_col)),
                    alpha = 0.6, width = 0.3)
  }

  # If routine data exists, add it to the plot
  if (!is.null(routine_data)) {
    p1 <- p1 +
      geom_point(data = routine_data, aes(x = year, y = routine_value, colour = "Routine data"))
  }

  p1 <- p1 +
    labs(
      title = paste0(title_name, " over time"),
      x = "Year",
      y = title_name,
      colour = if (!is.null(data_types_col)) "Data type" else NULL
    ) +
    theme_bw() +
    facet_wrap(facet_formula) +
    geom_smooth(aes(group = 1))

  plots[[1]] <- p1

  # Update aes mapping for rate of change vs level
  aes_mapping <- if (!is.null(data_types_col)) {
    aes(
      x = indicator_level,
      y = annualised_rate_of_change,
      group = get(group_col),
      colour = get(data_types_col)
    )
  } else {
    aes(
      x = indicator_level,
      y = annualised_rate_of_change,
      group = get(group_col)
    )
  }


  # Plot rate of change over time with conditional faceting
  p2 <- ggplot(data, aes_mapping) +
    geom_point() +
    labs(
      title = paste0("Annualised rate of change of ", title_name, " over time"),
      x = "Year",
      y = "Rate of change",
      colour = if (!is.null(data_types_col)) "Data type" else NULL
    ) +
    theme_bw() +
    facet_wrap(facet_formula)+
    geom_smooth(aes(group = 1))

  # If routine data exists, add it to the rate of change plot
  if (!is.null(routine_data)) {
    p2 <- p2 +
      geom_point(data = routine_data, aes(x = year, y = routine_rate_of_change, colour = "Routine data"))
  }

  plots[[2]] <- p2


  # Plot rate of change vs level with conditional faceting
  p3 <- ggplot(data %>% filter(!is.na(annualised_rate_of_change)), aes_mapping) +
    geom_point() +
    labs(
      title = paste0("Annualised rate of change vs ", title_name, " Level"),
      x = "Level",
      y = "Rate of change",
      colour = if (!is.null(data_types_col)) "Data type" else NULL
    ) +
    theme_bw() +
    facet_wrap(facet_formula)+
    geom_smooth(aes(group = 1)) #+
    #geom_smooth(method.args = list(family = "symmetric", span = 2), se = FALSE)

  # If routine data exists, add it to the rate of change vs level plot
  if (!is.null(routine_data)) {
    p3 <- p3 +
      geom_point(data = routine_data, aes(x = routine_value, y = routine_rate_of_change, colour = "Routine data"))  # Customize as needed
  }

  plots[[3]] <- p3

  return(plots)
}
