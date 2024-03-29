% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcmc_info.R
\name{mcmc-output}
\alias{mcmc-output}
\alias{get_}
\alias{get_logpost}
\alias{get_draws}
\alias{get_elapsed}
\alias{get_initial}
\alias{get_fun}
\alias{get_nsteps}
\alias{get_seed}
\alias{get_nchains}
\alias{get_burnin}
\alias{get_thin}
\alias{get_kernel}
\alias{get_multicore}
\alias{get_conv_checker}
\alias{get_cl}
\alias{get_progress}
\alias{get_chain_id}
\title{Information about the last \code{MCMC} call}
\usage{
get_(x)

get_logpost()

get_draws()

get_elapsed()

get_initial()

get_fun()

get_nsteps()

get_seed()

get_nchains()

get_burnin()

get_thin()

get_kernel()

get_multicore()

get_conv_checker()

get_cl()

get_progress()

get_chain_id()
}
\arguments{
\item{x}{Character scalar. Name of an argument to retrieve. If \code{x} was not
passed to the last call, the function returns with an error.}
}
\description{
This environment holds a copy of the last call to \link{MCMC}, including the start
and end time (to compute total elapsed time) of the call. Since the resulting
object of \code{MCMC} is an object of class \link[coda:mcmc]{coda::mcmc}, this is a way to capture
more information in case the user needs it.
}
\details{
The function \code{get_logpost} returns the \code{logposterior} value at each
iteration. The values correspond to a named numeric vector. If \code{nchains > 1}
then it will return a list of length \code{nchains} with the corresponding logpost
values for each chain.

The function \code{get_draws()} retrieves the proposed states from the
kernel function.
}
\examples{
# Getting the logpost -------------------------------------------------------
set.seed(23133)
x <- rnorm(200)
y <- -4 + x*2 + rnorm(200)
f <- function(p) {
  sum(dnorm(y - p[1] - x*p[2], log = TRUE))
}

# Setting a RAM kernel
kern <- kernel_am(eps = 1e-2)

ans <- MCMC(fun = f, initial = c(0, 1), nsteps = 2000, kernel = kern)
plot(
  # Plotting the logpost from the last run
  -get_logpost(), 
  # Getting the number of chains
  main = paste0("nchains: ", get_nchains()),
  
  # And the elapsed time
  sub  = sprintf("Run time: \%.4f(s)", get_elapsed()[3]),
  type = "l",
  log = "y"
) 

# This also works using multiple chains
ans <- MCMC(fun = f, initial = c(0, 0), nsteps=2000, nchains = 2, kernel = kern)

# In this case, just like -ans-, 
draws <- get_draws()

# Plotting proposed points vs accepted
plot(
  draws[[1]], pch = 20,
  col = adjustcolor("gray", alpha = .5),
  main = "Accepted vs proposed states\n(chain 1)"
  )
lines(ans[[1]], pch = 20, col = "tomato", lwd = 2)
legend(
  "topleft", legend = c("Accepted", "Proposed"), pch = c(NA, 20),
  col = c("tomato", "black"), lty = c(1, NA), lwd = c(2, NA)
)

}
