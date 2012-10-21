#' check if entries in a \code{data.frame} are infinite
#'
#' @param x a \code{data.frame}
#' @return a \code{data.frame} of logicals.
#' @author Mark Cowley, 2012-10-19
#' @export
#' @method is.infinite data.frame
#' @S3method is.infinite data.frame
#' @examples
#' df <- data.frame(names=1:5, a=1:5, b=Inf)
#' is.infinite(df)
is.infinite.data.frame <- function(x) {
	as.data.frame(x == Inf)
}
