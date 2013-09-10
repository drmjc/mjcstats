#' permute
#' 
#' Permute the elements in x. Ie shuffle the order, with no replacement.
#'
#' @param x a numeric or character vector
#' @return a numeric or characer vector, same length as 'x', with the order rearranged
#' @author Mark Cowley, 2013-08-28
#' @export
#' @seealso \code{\link{sample}}
#' @examples
#' permute(1:10)
#' permute(letters)
permute <- function(x) {
	sample(x,length(x), replace=FALSE)
}

