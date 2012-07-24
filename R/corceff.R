#' corceff
#' 
#' corceff is the matlab function name for \code{\link[stats]{cor}}
#'
#' @inheritParams stats::cor
#' @return see \code{\link[stats]{cor}}
#' @author Mark Cowley
#' @export
#' @importFrom stats cor
corceff <- function(x, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman")) {
	cor(x=x, y=y, use=use, method=method)
}
# note, you can't send corceff <- cor, as cor contains .Internal calls.
