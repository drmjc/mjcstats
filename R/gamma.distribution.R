#################################################################
## The Gamma Distribution    .                                 ##
##                                                             ##
## Helper functions parameterised by mean and variance rather  ##
## than shape and scale                                        ##
##                                                             ##
#################################################################

## Helper functions for generating gamma functions parameterised by mean and variance.
## The Gamma distribution with parameters 'shape' = a and 'scale' = s
## The mean and variance are E(X) = a*s and Var(X) = a*s^2.
##
## So Var(X) = E(X) * scale; scale = Var(X) / E(X)
## then shape = E(X) / scale
##
## Mark Cowley, 19 July 2005
##
rgamma2 <- function(n, mean, var) {
	scale <- var/mean
	shape <- mean/scale

	rgamma(n, shape=shape, scale=scale)
}


## Helper function for dgamma parameterised by mean and var instead of shape and scale.
##
## dgamma(x, shape, rate = 1, scale = 1/rate, log = FALSE)
##
## Mark Cowley, 19 July 2005
##
dgamma2 <- function(n, mean, var, log=FALSE) {
	scale <- var/mean
	shape <- mean/scale

	dgamma(n=n, shape=shape, scale=scale, log=log)
}

## Helper function for pgamma parameterised by mean and var instead of shape and scale.
##
##  pgamma(q, shape, rate = 1, scale = 1/rate, lower.tail = TRUE,
##             log.p = FALSE)
##
## Mark Cowley, 19 July 2005
##
pgamma2 <- function(q, mean, var, lower.tail=TRUE, log.p=TRUE) {
	scale <- var/mean
	shape <- mean/scale

	pgamma(q=q, shape=shape, scale=scale, lower.tail=lower.tail, log.p=log.p)
}


## Helper function for pgamma parameterised by mean and var instead of shape and scale.
##
##  qgamma(p, shape, rate = 1, scale = 1/rate, lower.tail = TRUE,
##             log.p = FALSE)
##
## Mark Cowley, 19 July 2005
##
qgamma2 <- function(p, mean, var, lower.tail = TRUE, log.p = FALSE) {
	scale <- var/mean
	shape <- mean/scale

	qgamma(p=p, shape=shape, scale=scale, lower.tail=lower.tail, log.p=log.p)
}

