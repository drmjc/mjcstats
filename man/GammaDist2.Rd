\name{rgamma2}
\alias{dgamma2}
\alias{pgamma2}
\alias{qgamma2}
\alias{rgamma2}
\title{The Gamma Distribution}
\usage{
  rgamma2(n, mean, var)

  dgamma2(x, mean, var, log = FALSE)

  pgamma2(q, mean, var, lower.tail = TRUE, log.p = TRUE)

  qgamma2(p, mean, var, lower.tail = TRUE, log.p = FALSE)
}
\arguments{
  \item{x}{vector of quantiles}

  \item{q}{vector of quantiles}

  \item{p}{vector of probabilities}

  \item{n}{number of observations. If \code{length(n) > 1},
  the length is taken to be the number required.}

  \item{log}{logical: if \code{TRUE},
  probabilities/densities p are returned as \code{log(p)}.}

  \item{log.p}{as for \code{log}}

  \item{lower.tail}{logical: if \code{TRUE} (default),
  probabilities are \code{P[X <= x]}, otherwise, \code{P[X
  > x]}.}

  \item{mean}{the mean of the distribution}

  \item{var}{the variance of the distribution}
}
\value{
  \code{dgamma2} gives the density, \code{pgamma2} gives
  the distribution function, \code{qgamma2} gives the
  quantile function, and \code{rgamma2} generates random
  deviates.

  Invalid arguments will result in return value \code{NaN},
  with a warning.
}
\description{
  Density, distribution function, quantile function and
  random generation for the Gamma distribution with
  parameters \code{mean} and \code{variance}, as opposed to
  the standard \code{shape} ans \code{scale}.
}
\details{
  The Gamma distribution with parameters \code{shape = a}
  and \code{scale = s} have a mean and variance of
  \code{E(X) = a*s} and \code{Var(X) = a*s^2.} Since
  \code{Var(X) = E(X) * scale}, thus \code{scale = Var(X) /
  E(X)} and \code{shape = E(X) / scale}.
}
\author{
  Mark Cowley, 19 July 2005
}
\seealso{
  \code{\link{rgamma}}
}

