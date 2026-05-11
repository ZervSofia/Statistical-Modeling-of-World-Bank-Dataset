library(quantreg)

fit_M1 <- function(df) {
  list(
    ls  = lm(unemployment ~ 1, data = df),
    lad = rq(unemployment ~ 1, tau = 0.5, data = df)
  )
}

fit_M2 <- function(df) {
  list(
    ls  = lm(unemployment ~ urban_pop + pop_growth, data = df),
    lad = rq(unemployment ~ urban_pop + pop_growth, tau = 0.5, data = df)
  )
}

fit_M3 <- function(df) {
  list(
    ls  = lm(unemployment ~ urban_pop + pop_growth +
               urban_pop_sq + pop_growth_sq + interaction, data = df),
    lad = rq(unemployment ~ urban_pop + pop_growth +
               urban_pop_sq + pop_growth_sq + interaction,
             tau = 0.5, data = df)
  )
}

fit_M4_transformed <- function(df, lambda) {
  df <- df |>
    mutate(
      y_transformed = if (abs(lambda) > 1e-5) (unemployment^lambda - 1) / lambda else log(unemployment)
    )

  list(
    data = df,
    model = lm(y_transformed ~ log_X1 + log_X2, data = df)
  )
}

generate_Z1_Z2 <- function(df, rho = 0.8, seed = 123) {
  set.seed(seed)
  n <- nrow(df)

  log_X1_std <- scale(df$log_X1)
  log_X2_std <- scale(df$log_X2)

  Z1 <- rho * log_X1_std + sqrt(1 - rho^2) * scale(rnorm(n))
  Z2 <- rho * log_X2_std + sqrt(1 - rho^2) * scale(rnorm(n))

  df$Z1 <- as.numeric(Z1)
  df$Z2 <- as.numeric(Z2)
  df
}

fit_M5 <- function(df) {
  lm(y_transformed ~ log_X1 + log_X2 + Z1 + Z2, data = df)
}
