% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convergence.R
\docType{data}
\name{convergence-checker}
\alias{convergence-checker}
\alias{automatic-stop}
\alias{LAST_CONV_CHECK}
\alias{convergence_data_set}
\alias{convergence_data_get}
\alias{convergence_msg_set}
\alias{convergence_msg_get}
\alias{convergence_gelman}
\alias{convergence_geweke}
\alias{convergence_heildel}
\alias{convergence_auto}
\title{Convergence Monitoring}
\format{
An object of class \code{fmcmc_output_conv_check} (inherits from \code{environment}) of length 1.
}
\usage{
LAST_CONV_CHECK

convergence_data_set(x)

convergence_data_get(x)

convergence_msg_set(msg = NA_character_)

convergence_msg_get()

convergence_gelman(freq = 1000L, threshold = 1.1, check_invariant = TRUE, ...)

convergence_geweke(
  freq = 1000L,
  threshold = 0.025,
  check_invariant = TRUE,
  ...
)

convergence_heildel(freq = 1000L, ..., check_invariant = TRUE)

convergence_auto(freq = 1000L)
}
\arguments{
\item{x}{In the case of \code{convergence_data_set}, a named list. For
\code{convergence_data_get}, a character vector.}

\item{msg}{Character scalar. Personalized message to print.}

\item{freq}{Integer scalar. Frequency of checking.}

\item{threshold}{Numeric value. A Gelman statistic below the threshold
will return \code{TRUE}.}

\item{check_invariant}{Logical. When \code{TRUE} the function only computes
the Gelman diagnostic using variables with greater than \code{1e-10} variance.}

\item{...}{Further arguments passed to the method.}
}
\value{
A function passed to \link{MCMC} to check automatic convergence.
}
\description{
Built-in set of functions to be used in companion with the argument
\code{conv_checker} in \link{MCMC}. These functions are not intended to be used
in a context other than the \code{MCMC} function.

The object \code{LAST_CONV_CHECK} is an environment that holds
information regarding the convergence checker used. This information can be
updated every time that the \code{conv_checker} function is called by \code{MCMC} using
the functions \code{convergence_data_set} and \code{convergence_msg_set}. The function
\code{convergence_data_get} is just a wrapper of \code{\link[=get]{get()}}.

The \code{msg} member of \code{LAST_CONV_CHECK} is resetted before \code{conv_checker} is
called.
}
\details{
\code{convergence_gelman} is a wrapper of \code{\link[coda:gelman.diag]{coda::gelman.diag()}}.

In the case of \code{convergence_geweke}, \code{threshold} sets the p-value
for the null \eqn{H_0: Z = 0}, i.e. equal means between the first and last
chunks of the chain. See \link[coda:geweke.diag]{coda::geweke.diag}. This implies that the higher
the threshold, the lower the probability of stopping the chain.

In the case that the chain has more than one parameter, the algorithm will
return true if and only if the test fails to reject the null for all the
parameters.

For the \code{convergence_heildel}, see \link[coda:heidel.diag]{coda::heidel.diag} for details.

The \code{convergence_auto} function is the default and is just a wrapper
for \code{convergence_gelman} and \code{convergence_geweke}. This function returns a
convergence checker that will be either of the other two depending on whether
\code{nchains} in \code{MCMC} is greater than one--in which case it will use the Gelman
test--or not--in which case it will use the Geweke test.
}
\section{Building a convergence checker}{

Convergence checkers are simply a function that receives as argument a matrix
(or list of them) with sampled values, and returns a logical scalar with the
value \code{TRUE} if the chain converged. An example of a personalized convergence
checker is provided below. The frequency with which the check is performed is
retrieved from the attribute \code{"freq"} from the convergence checker function,
i.e., \code{attr(..., "freq")}. If missing, convergence will be checked halfway
the number of steps in the chain, i.e., \code{floor(nsteps/2)}.
}

\examples{
# Example 1: Presonalized conv checker --------------------------------------
# Dummy rule, if acceptance rate is near between .2 and .3.
convergence_example <- function(x) {
  arate <- 1 - coda::rejectionRate(x)
  all(
    abs(arate - .25) < .05
  )
}

# Tell fmcmc::MCMC what is the frequency
attr(convergence_example, "freq") <- 2e3

set.seed(223)
x <- rnorm(1000)
y <- x * 2 + rnorm(1000)
logpost <- function(p) {
  sum(dnorm(y, mean = x * p, log = TRUE))
}

ans <- MCMC(
  initial = 0, fun = logpost, nsteps = 5e4,
  kernel= kernel_ram(),
  conv_checker = convergence_example
)

# Example 2: Adding information ---------------------------------------------
# Here we do two things: Save a value and set a message for the user
convergence_example_with_info <- structure(function(x) {
  arate <- 1 - coda::rejectionRate(x)
  
  # Saving a value
  if (!exists("arates", envir = LAST_CONV_CHECK, inherits = FALSE)) {
    convergence_data_set(list(arates = arate))
  } else {
    convergence_data_set(list(
      arates = rbind(convergence_data_get("arates"), arate)
    ))
  }
  
  # Setting up the message
  convergence_msg_set(
    sprintf("Current Avg. Accept. Rate: \%.2f", mean(arate))
  )
  
  all(
    abs(arate - .25) < .05
  )
}, freq = 2000)


ans <- MCMC(
  initial = 0, fun = logpost, nsteps = 5e4,
  kernel= kernel_ram(),
  conv_checker = convergence_example_with_info,
  seed = 112,
  progress = FALSE
)
}
\keyword{datasets}
