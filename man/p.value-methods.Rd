\docType{methods}
\name{p.value}
\alias{p.value}
\title{p.value}
\usage{
  p.value(x, ...)
}
\arguments{
  \item{x}{an object from coxph, surdiff, any result of
  class htest, which you get from most of the built in
  \sQuote{*.test} functions. see examples.}

  \item{\dots}{currently unused.}
}
\value{
  a numeric(1) containing the p-value. If \code{x} is a
  \code{survdiff} object, then the result is the
  chi-squared p.value (see survival:::summary.survfit). If
  \code{x} is a \code{code} object, then the result is the
  likelihood ratio p.value (see survival:::summary.coxph).
}
\description{
  Extract the p.value from a model fit.
}
\examples{
# example from t.test
a <- t.test(1:10,y=c(7:20))
p.value(a)
# [1] 1.855282e-05

# example from prop.test
heads <- rbinom(1, size=100, prob = .5)
a <- prop.test(heads, 100)
p.value(a)
# [1] 0.9203443

# example from survdiff
if( require(survival) ) {
   a <- survdiff(Surv(futime, fustat) ~ rx,data=ovarian)
   p.value(a)
# [1] 0.302591
}

# example from coxph
if( require(survival) ) {
  test1 <- list(time=c(4,3,1,1,2,2,3),
                status=c(1,1,1,0,1,1,0),
                x=c(0,2,1,1,1,0,0),
                sex=c(0,0,0,0,1,1,1))
  a <- coxph(Surv(time, status) ~ x + strata(sex), test1)
  p.value(a)
}
}
\author{
  Mark Cowley
}

