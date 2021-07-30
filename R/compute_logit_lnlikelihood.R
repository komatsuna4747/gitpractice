#' Function that compute the logit log-likelihood function
#'
#' @param param_constant a parameter for the constant term
#' @param param_covariate parameters for covariates
#' @param y a vector of binary outcomes
#' @param X a covariate matrix
#'
#' @return logit log-likelihood function
#'
#' @export
compute_logit_lnlikelihood <- function (param_constant, param_covariate, y, X) {
  # If length(y) != nrow(X), stop the process.
  if (length(y) != nrow(X)) {
    stop("\nThe length of the outcome binary vector and the number of rows of the covariate matrix don't match.")
  }

  # Compute exp(xb)
  exp_xb <- exp(param_constant + X %*% param_covariate)

  # Compute logit log-likelihood.
  # log(L(y, X; \theta)) = \sum y * log(exp(xb) / (1 + exp(xb))) + (1-y) * log(1 / (1 + exp(xb)))
  output <- sum(y * log(exp_xb/ (1 + exp_xb)) + (1 - y) * log(1 / (1 + exp_xb)))
  return(output)
}
