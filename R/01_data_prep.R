library(dplyr)
library(spData)

load_worldbank <- function() {
  data("worldbank_df", package = "spData")
  worldbank_df
}

prepare_worldbank <- function(df) {
  df |>
    mutate(
      urban_pop_sq = urban_pop^2,
      pop_growth_sq = pop_growth^2,
      interaction   = urban_pop * pop_growth,
      log_X1        = log(urban_pop + 1e-6),
      log_X2        = log(pop_growth + 1e-6),
      y             = unemployment
    ) |>
    tidyr::drop_na(unemployment, urban_pop, pop_growth)
}
