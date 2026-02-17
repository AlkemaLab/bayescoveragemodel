
# write_models
# we only need to do this once!

# libraries
library(tidyverse)
library(ggplot2)
library(stringr)
devtools::load_all(here::here())

write_model(add_aggregates = FALSE,
            add_routine = FALSE)

write_model(add_aggregates = FALSE,
            add_routine = TRUE)


write_model(add_aggregates = TRUE,
            add_routine = FALSE)

write_model(add_aggregates = TRUE,
            add_routine = TRUE)

