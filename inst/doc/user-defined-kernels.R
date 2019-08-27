## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  out.width = "80%",
  warning = FALSE,
  fig.width = 7, fig.height = 5
)

## ----dgp-----------------------------------------------------------------
library(fmcmc)
set.seed(1) # Always set the seed!!!!

# Population parameters
p <- .2
N <- 500

y <- rbinom(300, size = N, prob = p)

## ----ll------------------------------------------------------------------
ll <- function(n., p.) {
  sum(dbinom(y, n., prob = p., log = TRUE))
}

## ----creating-kernel-----------------------------------------------------
kernel_unif_int <- kernel_new(
    proposal = function(env) env$theta0 + sample(-3:3, 1),
    logratio = function(env) env$f1 - env$f0 # We could have skipped this
    )

## ----print-kernel--------------------------------------------------------
kernel_unif_int

## ----first-run-----------------------------------------------------------
ans <- MCMC(
  ll,                        # The log-likleihood function
  initial = max(y),          # A fair initial guess
  kernel  = kernel_unif_int, # Our new kernel function
  nsteps  = 1000,            # 1,000 MCMC draws
  thin    = 10,              # We will sample every 10
  p.      = p                # Passing extra parameters to be used by `ll`.
  )

## ----plot1---------------------------------------------------------------
plot(ans)

## ----second-run----------------------------------------------------------
ans <- MCMC(
  ll,
  initial = ans,             # MCMC will use tail(ans, 0) automatically
  kernel  = kernel_unif_int, # same as before
  nsteps  = 10000,           # More steps this time
  thin    = 10,              # same as before
  p.      = p                # same as before
)

## ----plot2---------------------------------------------------------------
plot(ans)
summary(ans)
table(ans)

