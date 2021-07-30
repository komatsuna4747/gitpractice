compute_logit_lnlikelihood <- function (param_constant, param_covariate, y, X) {
  # Compute exp(xb)
  exp_xb <- exp(param_constant + X %*% param_covariate)

  # Compute logit log-likelihood.
  # log(L(y, X; \theta)) = \sum y * log(exp(xb) / (1 + exp(xb))) + (1-y) * log(1 / (1 + exp(xb)))
  output <- sum(y * log(exp_xb/ (1 + exp_xb)) + (1 - y) * log(1 / (1 + exp_xb)))
  return(output)
}
