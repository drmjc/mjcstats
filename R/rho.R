#' Spearman's correlation (rho)
#'
#' Spearman's rho statistic is used to estimate a rank-based
#'   measure of association.  It is more robust than Pearson's correlation, and has been
#'   recommended if the data do not necessarily come from a bivariate
#'   normal distribution.
#' 
#' @details This is a wrapper around \code{cor(method="spearman")}
#' 
#' @inheritParams stats::cor
#' 
#' @return the Spearman's correlation
#' 
#' @author Mark Cowley, 9 Nov 2005
#' @export
#' @seealso \code{\link[stats]{cor}}
rho <- function(x, y=NULL, use="all.obs") {
    cor(x=x, y=y, use=use, method="spearman")
}
