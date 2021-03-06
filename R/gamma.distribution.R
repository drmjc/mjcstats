#' The Gamma Distribution
#' 
#' @description Density, distribution function, quantile function and random
#' generation for the Gamma distribution with parameters \code{mean} and \code{variance},
#' as opposed to the standard \code{shape} ans \code{scale}.
#' 
#' @details  The Gamma distribution with parameters \code{shape = a} and \code{scale = s}
#' have a mean and variance of \code{E(X) = a*s} and \code{Var(X) = a*s^2.}
#' Since \code{Var(X) = E(X) * scale}, thus \code{scale = Var(X) / E(X)}
#' and \code{shape = E(X) / scale}.
#'
#' @param x vector of quantiles
#' @param q vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations. If \code{length(n) > 1}, the length is
#'  taken to be the number required.
#' @param log logical: if \code{TRUE}, probabilities/densities p are returned
#'   as \code{log(p)}.
#' @param log.p as for \code{log}
#' @param lower.tail logical: if \code{TRUE} (default), probabilities are \code{P[X <= x]},
#'   otherwise, \code{P[X > x]}.
#' 
#' @param mean the mean of the distribution
#' @param var the variance of the distribution
#' 
#' @return \code{dgamma2} gives the density, \code{pgamma2} gives the distribution
#'  function, \code{qgamma2} gives the quantile function, and \code{rgamma2}
#'  generates random deviates.
#'  
#'  Invalid arguments will result in return value \code{NaN}, with a
#'  warning.
#' 
#' @export
#' @importFrom stats rgamma pgamma dgamma qgamma
#' @author Mark Cowley, 19 July 2005
#' @rdname GammaDist2
#' @seealso \code{\link{rgamma}}
rgamma2 <- function(n, mean, var) {
	scale <- var/mean
	shape <- mean/scale

	rgamma(n, shape=shape, scale=scale)
}

#' @export
#' @rdname GammaDist2
dgamma2 <- function(x, mean, var, log=FALSE) {
	scale <- var/mean
	shape <- mean/scale

	dgamma(x=x, shape=shape, scale=scale, log=log)
}

#' @export
#' @rdname GammaDist2
pgamma2 <- function(q, mean, var, lower.tail=TRUE, log.p=TRUE) {
	scale <- var/mean
	shape <- mean/scale

	pgamma(q=q, shape=shape, scale=scale, lower.tail=lower.tail, log.p=log.p)
}

#' @export
#' @rdname GammaDist2
qgamma2 <- function(p, mean, var, lower.tail = TRUE, log.p = FALSE) {
	scale <- var/mean
	shape <- mean/scale

	qgamma(p=p, shape=shape, scale=scale, lower.tail=lower.tail, log.p=log.p)
}
