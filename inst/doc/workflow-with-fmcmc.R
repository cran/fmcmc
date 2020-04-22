## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.dim=c(9, 9), out.width=600, out.height=600)

## ----loading-data, include = TRUE---------------------------------------------
library(fmcmc)
data(logit, package = "mcmc")
out <- glm(y ~ x1 + x2 + x3 + x4, data = logit, family = binomial, x = TRUE)
beta.init <- as.numeric(coefficients(out))

## ----log-unnorm-posterior-----------------------------------------------------
lupost_factory <- function(x, y) function(beta) {
    eta <- as.numeric(x %*% beta)
    logp <- ifelse(eta < 0, eta - log1p(exp(eta)), - log1p(exp(- eta)))
    logq <- ifelse(eta < 0, - log1p(exp(eta)), - eta - log1p(exp(- eta)))
    logl <- sum(logp[y == 1]) + sum(logq[y == 0])
    return(logl - sum(beta^2) / 8)
}

lupost <- lupost_factory(out$x, out$y)

## ----estimation1.0------------------------------------------------------------
# to get reproducible results
set.seed(42)
out <- MCMC(
  initial = beta.init,
  fun     = lupost,
  nsteps  = 1e3
)

## ----post-estimation----------------------------------------------------------
library(coda)
plot(out[,1:3])

## ----estimation1.1------------------------------------------------------------
# to get reproducible results
set.seed(42)
out <- MCMC(
  initial = beta.init,
  fun     = lupost,
  nsteps  = 1e3,
  kernel  = kernel_normal(scale = .2) 
)

## -----------------------------------------------------------------------------
plot(out[,1:3])

## ----estimation1.2------------------------------------------------------------
# to get reproducible results
set.seed(42)
out <- MCMC(
  initial = beta.init,
  fun     = lupost,
  nsteps  = 1e4,
  kernel  = kernel_normal(scale = .2),
  conv_checker = convergence_geweke(200)
)

## ----estimation2.0------------------------------------------------------------
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

## ----final-results------------------------------------------------------------
plot(out_final[, 1:3])
summary(out_final[, 1:3])

## ----kernel-object-adapt------------------------------------------------------
khaario <- kernel_adapt(freq = 1, warmup = 500)

## ----haario-first-inspect-----------------------------------------------------
# Number of iterations (absolute count, starts in 0)
khaario$abs_iter

# Variance covariance matrix (is empty... for now)
khaario$Sigma

## ----haario-first-run---------------------------------------------------------
set.seed(12) 

out_harrio_1 <- MCMC(
  initial   = out,                       
  fun       = lupost, 
  nsteps    = 1000,    # We will only run the chain for 100 steps                    
  kernel    = khaario, # We passed the predefined kernel
  thin      = 1,       # No thining here
  nchains   = 1L,      # A single chain
  multicore = FALSE    # Running in serial
  )

## ----haario-first-run-plots---------------------------------------------------
traceplot(out_harrio_1[,1], main = "Traceplot of the first parameter")
abline(v = 500, col = "red", lwd = 2, lty=2)

## ----haario-second-inspect----------------------------------------------------
# Number of iterations (absolute count, the counts equal the number of steps)
khaario$abs_iter

# Variance covariance matrix (now is not empty)
(Sigma1 <- khaario$Sigma)

## ----haario-second-run--------------------------------------------------------
out_harrio_2 <- MCMC(
  initial   = out_harrio_1,
  fun       = lupost, 
  nsteps    = 2000,    # We will only run the chain for 2000 steps now
  kernel    = khaario, # Same as before, same kernel.
  thin      = 1,       
  nchains   = 1L,      
  multicore = FALSE    
  )

## ----haario-second-run-plots--------------------------------------------------
traceplot(out_harrio_2[,1], main = "Traceplot of the first parameter")
abline(v = 500, col = "red", lwd = 2, lty=2)

## ----haario-third-inspect-----------------------------------------------------
# Number of iterations (absolute count, the counts equal the number of steps)
# This will have 1000 (first run) + 2000 (second run) steps
khaario$abs_iter

# Variance covariance matrix (now is not empty)
(Sigma2 <- khaario$Sigma)

# How different these are?
Sigma1 - Sigma2

