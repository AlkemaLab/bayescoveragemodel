



#' write model
#'
#' @param add_aggregates Add national aggregates? makes sense only for subnational all-region run for 1 country
#' @param  add_routine Add routine data?
#'
#' @returns writes a stan file to the stan directory, naming ...
#' @export
#'

# testing: for no aggr, add_trad
write_model <- function(add_aggregates = FALSE,
                        add_routine = FALSE){

  # general
  # make sure that names are not used in function calls etc!
  # and part of other things... eg eta = bad!! ie in beta
  # we use capitalization to avoid issues

  ### build base model
  stan_code <- readr::read_file(here::here("inst/stan", "fpem_buildingblocks.stan"))


  # ### consider aggregates
  # don't add if not subnational
  aggregates_data_code <- readr::read_file(here::here("inst/stan", "aggregates_data.stan"))
  # choose one for this {{datamodel_MODEL}}
  datamodel_waggregates_code <- readr::read_file(here::here("inst/stan", "datamodel_waggregates.stan"))
  datamodel_code <- readr::read_file(here::here("inst/stan", "datamodel.stan"))
  # choose one for this {{CHOOSE_AGGREGATESYESNO_GENQUANTITIES}}
  genquantities_waggregates_code <- readr::read_file(here::here("inst/stan", "aggregates_genquantities.stan"))
  genquantities_code <- readr::read_file(here::here("inst/stan", "nonaggregates_genquantities.stan"))

  stan_code <- stan_code %>%
    stringr::str_replace_all("\\{\\{AGGREGATES_DATA\\}\\}",
                             ifelse(add_aggregates, aggregates_data_code, " ")) %>%
    stringr::str_replace_all("\\{\\{datamodel_MODEL\\}\\}",
                             ifelse(add_aggregates, datamodel_waggregates_code, datamodel_code))%>%
    stringr::str_replace_all("\\{\\{CHOOSE_AGGREGATESYESNO_GENQUANTITIES\\}\\}",
                             ifelse(add_aggregates,
                                    genquantities_waggregates_code,
                                    genquantities_code))


  # routine data included?
  routine_data_code <- readr::read_file(here::here("inst/stan", "routine_data.stan"))
  routine_parameters_code <- readr::read_file(here::here("inst/stan", "routine_parameters.stan"))
  routine_model_code <- readr::read_file(here::here("inst/stan", "routine_model.stan"))
  stan_code <- stan_code %>%
    stringr::str_replace_all("\\{\\{routine_DATA\\}\\}",
                             ifelse(add_routine, routine_data_code, " ")) %>%
    stringr::str_replace_all("\\{\\{routine_PARAMETERS\\}\\}",
                             ifelse(add_routine, routine_parameters_code, " ")) %>%
    stringr::str_replace_all("\\{\\{routine_MODEL\\}\\}",
                             ifelse(add_routine, routine_model_code, " "))

  # write to file
  stanmodelname <- case_when(
    ! add_routine & !add_aggregates ~ "fpem",
    add_routine & !add_aggregates ~ "fpem_routine",
    ! add_routine & add_aggregates ~ "fpem_aggregates",
    add_routine & add_aggregates ~ "fpem_routine_aggregates"
  )
  complete_model <- cmdstanr::write_stan_file(stan_code, dir = here::here("inst/stan"),
                                                force_overwrite = TRUE,
                                                basename = stanmodelname)
  return(invisible(NULL))
}



