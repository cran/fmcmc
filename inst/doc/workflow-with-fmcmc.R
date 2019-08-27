## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.dim=c(9, 9), out.width=600, out.height=600)

## ----loading-data, include = TRUE----------------------------------------
library(fmcmc)
data(logit, package = "mcmc")
out <- glm(y ~ x1 + x2 + x3 + x4, data = logit, family = binomial, x = TRUE)
beta.init <- as.numeric(coefficients(out))

## ----log-unnorm-posterior------------------------------------------------
lupost_factory <- function(x, y) function(beta) {
    eta <- as.numeric(x %*% beta)
    logp <- ifelse(eta < 0, eta - log1p(exp(eta)), - log1p(exp(- eta)))
    logq <- ifelse(eta < 0, - log1p(exp(eta)), - eta - log1p(exp(- eta)))
    logl <- sum(logp[y == 1]) + sum(logq[y == 0])
    return(logl - sum(beta^2) / 8)
}

lupost <- lupost_factory(out$x, out$y)

## ----estimation1.0-------------------------------------------------------
# to get reproducible results
set.seed(42)
out <- MCMC(
  initial = beta.init,
  fun     = lupost,
  nsteps  = 1e3
)

## ----post-estimation-----------------------------------------------------
library(coda)
plot(out[,1:3])

## ----estimation1.1-------------------------------------------------------
# to get reproducible results
set.seed(42)
out <- MCMC(
  initial = beta.init,
  fun     = lupost,
  nsteps  = 1e3,
  kernel  = kernel_normal(scale = .2) 
)

## ------------------------------------------------------------------------
plot(out[,1:3])

## ----estimation1.2-------------------------------------------------------
# to get reproducible results
set.seed(42)
out <- MCMC(
  initial = beta.init,
  fun     = lupost,
  nsteps  = 1e4,
  kernel  = kernel_normal(scale = .2),
  conv_checker = convergence_geweke(200)
)

## ----estimation2.0-------------------------------------------------------
# Now we change the seed so we get a different stream of
# pseudo random numbers
set.seed(112) 

out_final <- MCMC(
  initial   = out,                       # Automagically takes the last 2 points
  fun       = lupost, 
  nsteps    = 5e4,                       # Increasing the sample size
  kernel    = kernel_normal(scale = .2),
  thin      = 10,
  nchains   = 2L,                        # Running parallel chains
  multicore = TRUE                       # in parallel.
  )

## ----final-results-------------------------------------------------------
plot(out_final[, 1:3])
summary(out_final[, 1:3])

