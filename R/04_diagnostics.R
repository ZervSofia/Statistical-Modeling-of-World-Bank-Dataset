library(lmtest)
library(car)

diagnostics_gauss_markov <- function(model, df, name = "model") {
  res <- residuals(model)
  fitted <- fitted(model)

  list(
    bp   = bptest(model),
    dw   = dwtest(model),
    shapiro = shapiro.test(res),
    ks      = ks.test(
      (res - mean(res)) / sd(res),
      "pnorm", mean = 0, sd = 1
    ),
    vif  = tryCatch(vif(model), error = function(e) NA),
    residuals = res,
    fitted    = fitted
  )
}
