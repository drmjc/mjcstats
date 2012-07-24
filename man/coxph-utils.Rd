\name{lrt.coxph}
\alias{coef_pvalue.coxph}
\alias{coef.pvalue.coxph}
\alias{lrt_pvalue.coxph}
\alias{lrt.coxph}
\alias{lrt.pvalue.coxph}
\title{Extract LRS or p-values from coxph object}
\usage{
  lrt.coxph(x, digits = options()$digits, ...)

  lrt_pvalue.coxph(x, digits = options()$digits, ...)

  coef_pvalue.coxph(x, digits = options()$digits, ...)
}
\arguments{
  \item{x}{a coxph object}

  \item{digits}{the number of digits to print}

  \item{\dots}{currently unused}
}
\value{
  \code{lrt.coxph}: numeric(1), the Likelihood ratio test
  statistic

  \code{lrt_pvalue.coxph}: numeric(1), the p-value from the
  Likelihood ratio test statistic

  \code{coef_pvalue.coxph}: numeric: the p-value from each
  coefficient in the coxph object
}
\description{
  Extract LRS or p-values from coxph object
}
\details{
  \code{lrt.coxph}: Extract the log ratio test statistic
  from the overall Cox Proportional-hazards model fit.

  \code{lrt_pvalue.coxph}: Extract the log ratio test
  Pvalue from the overall Cox Proportional-hazards model
  fit.

  \code{coef_pvalue.coxph}: Extract the pvalue from each
  coefficient in \code{x}
}
\note{
  Code derived from the \code{survival:::print.coxph}
  method
}
\examples{
# lrt.coxph
\dontrun{
 cph.res <- coxph(Surv(...) ~ a)
	lrt.coxph(cph.res)
	lrt_pvalue.coxph(cph.res)
 # [1] 0.0197
	coef_pvalue.coxph(cph.res)
}
}
\author{
  Mark Cowley, 2011-09-02

  Mark Cowley, 2011-09-02
}

