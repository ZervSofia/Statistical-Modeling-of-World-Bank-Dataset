library(caret)
library(dplyr)
library(quantreg)


cv_lm <- function(formula, data, k = 10, seed = 123) {
  set.seed(seed)
  train(
    form = formula,
    data = data,
    method = "lm",
    trControl = trainControl(method = "cv", number = k),
    metric = "RMSE"
  )$resample
}

cv_lad_manual <- function(formula, data, k = 10, seed = 123) {
  set.seed(seed)
  folds <- createFolds(data[[all.vars(formula)[1]]], k = k, list = TRUE)

  metrics <- lapply(seq_along(folds), function(i) {
    test_idx  <- folds[[i]]
    train_dat <- data[-test_idx, ]
    test_dat  <- data[test_idx, ]

    fit  <- rq(formula, tau = 0.5, data = train_dat)
    pred <- predict(fit, newdata = test_dat)
    y    <- test_dat[[all.vars(formula)[1]]]

    data.frame(
      Fold = i,
      RMSE = sqrt(mean((y - pred)^2)),
      MAE  = mean(abs(y - pred))
    )
  })

  bind_rows(metrics)
}
