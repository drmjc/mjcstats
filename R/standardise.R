#' Standardise
#' 
#' Standardise a vector of numbers, by subtracting the mean, and dividing by
#' the stdev
#' 
#' @param x a numeric vector
#' @return a numeric vector
#' @author Mark Cowley, 2008-06-11
#' @export
standardise <- function(x) {
	(x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
}
