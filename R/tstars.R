#' Look up t_stars to use in local fit based on the values used in a global fit
#'
#' @param local_geo_unit_index Local geo unit index
#' @param global_geo_unit_index Global geo unit index
#' @param global_t_stars Global t_stars values
#'
#' @return Vector of t_stars for local fit
#'
#' @keywords internal
lookup_t_stars_from_global_fit <- function(local_geo_unit_index,
                                           global_geo_unit_index,
                                           global_t_stars) {
  join_colnames <- intersect(colnames(local_geo_unit_index),
                             colnames(global_geo_unit_index))
  join_colnames <- join_colnames[join_colnames != "c"]
  local_t_stars <- local_geo_unit_index |>
    dplyr::left_join(
      global_geo_unit_index |> dplyr::mutate(t_star = global_t_stars),
      by = join_colnames) |>
    dplyr::pull(t_star)

  return(local_t_stars)
}
