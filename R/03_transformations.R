library(MASS)

boxcox_lambda <- function(model, lambda_seq = seq(-2, 2, 0.1)) {
  bc <- boxcox(model, lambda = lambda_seq)
  bc$x[which.max(bc$y)]
}
