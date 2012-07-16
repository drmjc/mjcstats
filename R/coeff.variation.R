#' coefficient of variance
#'
#' The coefficient of variation (CV) is defined as the ratio of the standard deviation to the mean.
#' As such, it is a measure of dispersion of a probability distribution. It can only
#' be computed on for data on a ratio scale (ie all positive).
#' 
#' @param x a numeric vector of positive values
#' @param as.percent logical: return result as a percentage?
#' @param na.rm logical: remove \code{NA}'s ?
#' @return a numeric(1)
#' 
#' @references \url{http://en.wikipedia.org/wiki/Coefficient_of_variation}
#' @author Mark Cowley, 2012-07-09
#' @export
#' @importFrom stats sd
#' 
#' @examples
#' val <- runif(25)
#' cv(val)
#' cv(val, as.percent=TRUE)
#' cv(c(val, NA), na.rm=FALSE)
cv <- function(x, as.percent=FALSE, na.rm=TRUE) {
	if( na.rm ) 
		x <- x[!is.na(x)]
	else if (any(is.na(x))) 
		return(NA)
	
	all(x>0) || stop("cv can only be calculated on ratio-scale data (ie no negatives)")
	res <- sd(x)/mean(x)
	if( as.percent ) res <- res * 100
	
	res
}
