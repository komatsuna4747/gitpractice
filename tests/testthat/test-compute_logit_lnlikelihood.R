###### Data preparation ######
# Number of observations
N <- 1000

# Outcome binary variable
y <- sample(x = c(0, 1), size = N, replace = TRUE)

# Covariates
X <- matrix(rnorm(2 * N), ncol = 2)

# Parameters
alpha <- -1
beta <- c(1, -1)

test_that("computing logit log-likelihood works", {
  # Compute log-likelihood using `compute_logit_lnlikelihood`
  lnlikelihood <- compute_logit_lnlikelihood(alpha, beta, y, X)

  # Compute log-likelihood using in a naive (and correct) way
  lnlikelihood_true <- 0
  for (i in 1:length(y)) {
    xb <- alpha
    for (k in 1:ncol(X)) {
      xb <- xb + X[i, k] * beta[[k]]
    }
    exp_xb <- exp(xb)
    lnlikelihood_true <- lnlikelihood_true + y[[i]] * log(exp_xb/(1 + exp_xb)) + (1 - y[[i]]) * log(1/(1 + exp_xb))
  }

  # Check if they match
  expect_equal(lnlikelihood, lnlikelihood_true)
})

test_that("returing an error when inappropriate inputs are given works", {
  # When the dimension of `param_covariate` and ncol(X) don't match, return an error.
  expect_error(compute_logit_lnlikelihood(alpha, c(beta, 1), y, X))

  # When nrow(y) and nrow(X) don't match, return an error.
  expect_error(compute_logit_lnlikelihood(alpha, beta, c(y, 1), X))
})
