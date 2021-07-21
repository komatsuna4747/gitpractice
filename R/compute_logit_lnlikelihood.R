compute_logit_lnlikelihood <- function (param, y, X) {
  output <- 0
  for (i in 1:length(y)) {

    exp_xb <- 0
    for (k in 1:ncol(X)) {
      exp_xb <- exp_xb + X[i, k] * param[[k]]
    }
    output <- output + y[[i]] * log(exp_xb/(1 + exp_xb)) + (1 - y[[i]]) * log(1/(1 + exp_xb))
  }
  return(output)
}
