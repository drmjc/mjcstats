#' Extract LRS or p-values from coxph object
#' 
#' @details \code{lrt.coxph}: Extract the log ratio test statistic 
#' from the overall Cox Proportional-hazards model fit.
#' 
#' @note Code derived from the \code{survival:::print.coxph} method
#' 
#' @param x a coxph object
#' @param digits the number of digits to print
#' @param \dots currently unused
#' 
#' @return \code{lrt.coxph}: numeric(1), the Likelihood ratio test statistic
#' 
#' @author Mark Cowley, 2011-09-02
#' @export
#' @rdname coxph-utils
#' @examples
#' # lrt.coxph
#' \dontrun{
#'  cph.res <- coxph(Surv(...) ~ a)
#' 	lrt.coxph(cph.res)
#' 	lrt_pvalue.coxph(cph.res)
#'  # [1] 0.0197
#' 	coef_pvalue.coxph(cph.res)
#' }
lrt.coxph <- function(x, digits = options()$digits, ...)  {
	require(survival)
	is(x, "coxph") || stop("x != coxph")
	
	logtest <- -2 * (x$loglik[1] - x$loglik[2])
	if (is.null(x$df)) 
		df <- sum(!is.na(x$coefficients))
	else df <- round(sum(x$df), 2)
	lrt <- signif(logtest, digits)
	p <- 1 - pchisq(logtest, df)
	
	lrt
}


#' @details \code{lrt_pvalue.coxph}: Extract the log ratio test Pvalue 
#' from the overall Cox Proportional-hazards model fit.
#' 
#' @return \code{lrt_pvalue.coxph}: numeric(1), the p-value from the 
#' Likelihood ratio test statistic
#' 
#' @author Mark Cowley, 2011-09-02
#' @export
#' @rdname coxph-utils
#' @aliases lrt_pvalue.coxph lrt.pvalue.coxph
lrt_pvalue.coxph <- function(x, digits = options()$digits, ...)  {
	require(survival)
	is(x, "coxph") || stop("x != coxph")
	
	logtest <- -2 * (x$loglik[1] - x$loglik[2])
	if (is.null(x$df)) 
		df <- sum(!is.na(x$coefficients))
	else df <- round(sum(x$df), 2)
	lrt <- signif(logtest, digits)
	p <- signif(1 - pchisq(logtest, df), digits)
	
	p
}


#' @details \code{coef_pvalue.coxph}: Extract the pvalue from 
#' each coefficient in \code{x}
#' 
#' @return \code{coef_pvalue.coxph}: numeric: the p-value from 
#' each coefficient in the coxph object
#' 
#' @export
#' @rdname coxph-utils
#' @aliases coef_pvalue.coxph coef.pvalue.coxph
coef_pvalue.coxph <- function(x, digits = options()$digits, ...)  {
	require(survival)
	is(x, "coxph") || stop("x != coxph")
	
	if (!is.null(x$fail)) {
		cat(" Coxph failed.", x$fail, "\n")
		return(NA)
	}
	savedig <- options(digits = digits)
	on.exit(options(savedig))
	coef <- x$coefficients
	se <- sqrt(diag(x$var))
	if (is.null(coef) | is.null(se)) 
		stop("Input is not valid")
	p <- 1 - pchisq((coef/se)^2, 1)
	p <- signif(p, digits)
	
	p
}




# # don't source me - i'm included in the survival package
# survival:::print.coxph <- function (x, digits = max(options()$digits - 4, 3), ...)	{
# 	if (!is.null(cl <- x$call)) {
# 		cat("Call:\n")
# 		dput(cl)
# 		cat("\n")
# 	}
# 	if (!is.null(x$fail)) {
# 		cat(" Coxph failed.", x$fail, "\n")
# 		return()
# 	}
# 	savedig <- options(digits = digits)
# 	on.exit(options(savedig))
# 	coef <- x$coefficients
# 	se <- sqrt(diag(x$var))
# 	if (is.null(coef) | is.null(se)) 
# 		stop("Input is not valid")
# 
# 	if (is.null(x$naive.var)) {
# 		tmp <- cbind(
# 			coef, 
# 			exp(coef), 
# 			se, 
# 			coef/se, 
# 			signif(1 - pchisq((coef/se)^2, 1), digits - 1)
# 		)
# 		dimnames(tmp) <- list(
# 			names(coef), 
# 			c("coef", "exp(coef)", "se(coef)", "z", "p")
# 		)
# 	}
# 	else {
# 		nse <- sqrt(diag(x$naive.var))
# 		tmp <- cbind(
# 			coef, 
# 			exp(coef), 
# 			nse, 
# 			se, 
# 			coef/se, 
# 			signif(1 - pchisq((coef/se)^2, 1), digits - 1)
# 		)
# 		dimnames(tmp) <- list(
# 			names(coef), 
# 			c("coef", "exp(coef)", "se(coef)", "robust se", "z", "p")
# 		)
# 	}
# 	cat("\n")
# 	prmatrix(tmp)
# 	logtest <- -2 * (x$loglik[1] - x$loglik[2])
# 	if (is.null(x$df)) 
# 		df <- sum(!is.na(coef))
# 	else df <- round(sum(x$df), 2)
# 	cat("\n")
# 	cat("Likelihood ratio test=", format(round(logtest, 2)), 
# 		"  on ", df, " df,", " p=", format(1 - pchisq(logtest, 
# 			df)), sep = "")
# 	omit <- x$na.action
# 	cat("  n=", x$n)
# 	if (!is.null(x$nevent)) 
# 		cat(", number of events=", x$nevent, "\n")
# 	else cat("\n")
# 	if (length(omit)) 
# 		cat("	(", naprint(omit), ")\n", sep = "")
# 	invisible(x)
# }
# 
# ###############################################################################
# 
# 
# 
