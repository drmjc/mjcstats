#' p.value
#' 
#' Extract the p.value from a model fit.
#' 
#' @param x an object from coxph, surdiff, any result of class htest, which you get from most of the 
#'  built in \sQuote{*.test} functions. see examples.
#' @return a numeric(1) containing the p-value. If \code{x} is a \code{survdiff} object, then the result is the 
#'  chi-squared p.value (see survival:::summary.survfit). If \code{x} is a \code{code} object, then the result is the 
#'  likelihood ratio p.value (see survival:::summary.coxph).
#' @param \dots currently unused.
#' 
#' @author Mark Cowley
#' @rdname p.value-methods
#' @docType methods
#' 
#' @export
#' @S3method p.value default
#' @S3method p.value htest
#' @S3method p.value survdiff
#' @S3method p.value coxph
#' @rdname p.value-methods
#' 
#' @examples
#' # example from t.test
#' a <- t.test(1:10,y=c(7:20))
#' p.value(a)
#' # [1] 1.855282e-05
#'
#' # example from prop.test
#' heads <- rbinom(1, size=100, prob = .5)
#' a <- prop.test(heads, 100)
#' p.value(a)
#' # [1] 0.9203443
#' 
#' # example from survdiff
#' if( require(survival) ) {
#'    a <- survdiff(Surv(futime, fustat) ~ rx,data=ovarian)
#'    p.value(a)
#' # [1] 0.302591
#' }
#' 
#' # example from coxph
#' if( require(survival) ) {
#'   test1 <- list(time=c(4,3,1,1,2,2,3), 
#'                 status=c(1,1,1,0,1,1,0), 
#'                 x=c(0,2,1,1,1,0,0), 
#'                 sex=c(0,0,0,0,1,1,1)) 
#'   a <- coxph(Surv(time, status) ~ x + strata(sex), test1) 
#'   p.value(a)
#' }
p.value <- function(x, ...) UseMethod("p.value")

p.value.default <- function(x) {
	"p.value" %in% names(x) || stop("Can't extract p.value from object of type: ", class(x))
	x$p.value
}

p.value.htest <- function(x) {
	x$p.value
}

p.value.survdiff <- function(x) {
    if (length(x$n) == 1) {
		stop("unsupported condition")
    }
    else {
        if (is.matrix(x$obs)) {
            otmp <- apply(x$obs, 1, sum)
            etmp <- apply(x$exp, 1, sum)
        }
        else {
            otmp <- x$obs
            etmp <- x$exp
        }
        df <- (sum(1 * (etmp > 0))) - 1
        res <- signif(1 - pchisq(x$chisq, df))
    }

    return(res)
}

p.value.coxph <- function(x) {
	digits <- getOption("digits", 4)
    logtest <- -2 * (x$loglik[1] - x$loglik[2])
    if (is.null(x$df)) 
        df <- sum(!is.na(x$coefficients))
    else df <- round(sum(x$df), 2)
    lrt <- signif(logtest, digits)
    p <- signif(1 - pchisq(logtest, df), digits)
    p
}
