#' inverse Kosambi
#' 
#' Calculate the mab distance using inverse Kosambi function.
#' NB, x must be in range [0, 0.5]
#'
#' @param x a numeric vector of distance values, all values in range [0, 0.5]
#' 
#' @return a numeric vector of inverse Kosambi distances
#' 
#' @references \url{http://www.mapmanager.org/MMM/MMMAlgorithms.html#996839}
#'
#' @author Mark Cowley, 30 June 2005
#'
inverse.kosambi <- function(x) {
	return( 0.25*(log(1+2*x)) - log(1-2*x) )
}
