## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.dim=c(9, 9), out.width=600, out.height=600)

## ----loading------------------------------------------------------------------
library(fmcmc)
data(lifeexpect)
head(lifeexpect) # Taking a look at the data

## ----logpost------------------------------------------------------------------
logpost <- function(p, D) {
  sum(dnorm(
    D$age - (p[1] + p[2] * D$smoke + p[3] * D$female),
    sd = p[4],
    log = TRUE
  ))
}

## ----print-help---------------------------------------------------------------
# This will show the available objects
ith_step

## ----logpost2-----------------------------------------------------------------
logpost2 <- function(p, D) {

  # Getting the number of step
  i <- ith_step("i")

  # After the first iteration, every 1000 steps:
  if (i > 1L && !(i %% 1000)) {

    # The last accepted state. Accepted states are
    # stored in -ans-.
    s <- ith_step()$ans[i - 1L,]

    cat("Step: ", i, " state: c(\n  ", paste(s, collapse = ", "), "\n)\n", sep = "")

  }

  # Just returning the previous value
  logpost(p, D)

}

## ----haario-with-ith-step, echo = TRUE----------------------------------------
# Generating kernel
kern <- kernel_ram(warmup = 1000, lb = c(-100,-100,-100,.001))

# Running MCMC
ans0 <- MCMC(
  initial  = c(70, 0, 0, sd(lifeexpect$age)),
  fun      = logpost2, 
  nsteps   = 10000,    
  kernel   = kern, 
  seed     = 555,
  D        = lifeexpect,
  progress = FALSE
  )


## ----lupost3, eval = TRUE-----------------------------------------------------
logpost3 <- function(p, D) {

  # Getting the number of step
  i <- ith_step("i")

  # After the first iteration, every 1000 steps:
  if (i > 1L && !(i %% 1000)) {

    # The last accepted state. Accepted states are
    # stored in -ans-.
    s <- ith_step()$ans[i - 1L,]
    
    chain <- ith_step("chain_id")

    cat("Step: ", i, " chain: ", chain, " state: c(\n  ",
        paste(s, collapse = ",\n  "), "\n)\n", sep = ""
        )

  }
  
  # Just returning the previous value
  logpost(p, D)

}

# Rerunning using parallel chains
ans1 <- MCMC(
  initial   = ans0,
  fun       = logpost3, # The new version of logpost includes chain
  nsteps    = 1000,    
  kernel    = kern,     # Reusing the kernel
  thin      = 1,       
  nchains   = 2L,       # Two chains, two different prints     
  multicore = FALSE,
  seed      = 555,
  progress  = FALSE,
  D         = lifeexpect
  )

## ----lupost4------------------------------------------------------------------
logpost4 <- function(p, D) {

  # Timestamp
  set_userdata(
    p1 = p[1],
    p2 = p[2],
    p3 = p[3]
  )

  # Just returning the previous value
  logpost(p, D)

}

# Rerunning using parallel chains
ans1 <- MCMC(
  initial   = ans0,
  fun       = logpost4, # The new version of logpost includes chain
  nsteps    = 1000,    
  kernel    = kern,     # Reusing the kernel
  thin      = 10,       # We are adding thinning
  nchains   = 2L,       # Two chains, two different prints     
  multicore = FALSE,
  seed      = 555,
  progress  = FALSE,
  D         = lifeexpect
  )

## ----inspect-and-plot---------------------------------------------------------
print(MCMC_OUTPUT)
str(get_userdata())

