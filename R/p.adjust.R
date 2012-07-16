#' Improved p.adjust
#' 
#' A new version of \code{\link[stats]{p.adjust}} which allows for \code{qvalue} estimation.
#' 
#' qvalue is known to print ERROR's if the input p-values are not suitably 
#' distributed.
#' This function uses \code{\link{qvalue2}}, which tries to run \code{qvalue} with 2 different
#' settings, else it falls back to using \dQuote{BH} to estimate FDR.
#' 
#' @param p a vector of p-values
#' @param method see \code{\link{p.adjust.methods}}
#' @param n optional number of observations
#' @author Mark Cowley 2008-05-05
#' @seealso \code{\link[stats]{p.adjust}} \code{\link[stats]{p.adjust.methods}} \code{\link{qvalue2}}
#' @export
#' @examples
#' pvals <- c(runif(100,0,0.05),runif(1e03,0,1))
#' bh    <- p.adjust(pvals, "BH")
#' bonf  <- p.adjust(pvals, "bonferroni")
#' qvals <- p.adjust(pvals, "qvalue")
#' \dontrun{
#'   par(mfrow=c(2,2))
#'   hist(pvals)
#'   hist(bh)
#'   hist(bonf)
#'   hist(qvals)
#' }
p.adjust <- function (p, method = p.adjust.methods, n = length(p)) {
    method <- match.arg(method)
    if (method == "fdr") 
        method <- "BH"
    nm <- names(p)
    p <- as.numeric(p)
    names(p) <- nm
    p0 <- p
    if (all(nna <- !is.na(p))) 
        nna <- TRUE
    p <- p[nna]
    lp <- length(p)
    stopifnot(n >= lp)
    if (n <= 1) 
        return(p0)
    if (n == 2 && method == "hommel") 
        method <- "hochberg"
    p0[nna] <- switch(method, bonferroni = pmin(1, n * p), holm = {
        i <- seq_len(lp)
        o <- order(p)
        ro <- order(o)
        pmin(1, cummax((n - i + 1L) * p[o]))[ro]
    }, hommel = {
        if (n > lp) p <- c(p, rep.int(1, n - lp))
        i <- seq_len(n)
        o <- order(p)
        p <- p[o]
        ro <- order(o)
        q <- pa <- rep.int(min(n * p/i), n)
        for (j in (n - 1):2) {
            ij <- seq_len(n - j + 1)
            i2 <- (n - j + 2):n
            q1 <- min(j * p[i2]/(2:j))
            q[ij] <- pmin(j * p[ij], q1)
            q[i2] <- q[n - j + 1]
            pa <- pmax(pa, q)
        }
        pmax(pa, p)[if (lp < n) ro[1:lp] else ro]
    }, hochberg = {
        i <- lp:1L
        o <- order(p, decreasing = TRUE)
        ro <- order(o)
        pmin(1, cummin((n - i + 1L) * p[o]))[ro]
    }, BH = {
        i <- lp:1L
        o <- order(p, decreasing = TRUE)
        ro <- order(o)
        pmin(1, cummin(n/i * p[o]))[ro]
    }, BY = {
        i <- lp:1L
        o <- order(p, decreasing = TRUE)
        ro <- order(o)
        q <- sum(1L/(1L:n))
        pmin(1, cummin(q * n/i * p[o]))[ro]
    }, qvalue = {
		q <- qvalue2( as.numeric(p), fallback="BH" )$qvalues
		
		if( !is.null(names(p)) ) 
			names(q) <- names(p)
		q
    }, none = p)
    p0
}

#' An updated p.adjust.methods vector
#' 
#' A replacement to the standard \code{\link[stats]{p.adjust.methods}}, including the
#'  Storey-Tibshirani positive FDR
#' @author Mark Cowley
#' @seealso \code{\link[=p.adjust]{p.adjust.methods}} \code{\link{p.adjust}}
#' @export
#' @rdname p.adjust
p.adjust.methods <- c(stats::p.adjust.methods, "qvalue")
