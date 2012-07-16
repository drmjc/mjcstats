\docType{data}
\name{p.adjust}
\alias{p.adjust}
\alias{p.adjust.methods}
\title{Improved p.adjust}
\format{chr [1:9] "holm" "hochberg" "hommel" "bonferroni" "BH" "BY" ...}
\usage{
  p.adjust(p, method = p.adjust.methods, n = length(p))

  p.adjust.methods
}
\arguments{
  \item{p}{a vector of p-values}

  \item{method}{see \code{\link{p.adjust.methods}}}

  \item{n}{optional number of observations}
}
\description{
  A new version of \code{\link[stats]{p.adjust}} which
  allows for \code{qvalue} estimation.

  A replacement to the standard
  \code{\link[stats]{p.adjust.methods}}, including the
  Storey-Tibshirani positive FDR
}
\details{
  qvalue is known to print ERROR's if the input p-values
  are not suitably distributed. This function uses
  \code{\link{qvalue2}}, which tries to run \code{qvalue}
  with 2 different settings, else it falls back to using
  \dQuote{BH} to estimate FDR.
}
\examples{
pvals <- c(runif(100,0,0.05),runif(1e03,0,1))
bh    <- p.adjust(pvals, "BH")
bonf  <- p.adjust(pvals, "bonferroni")
qvals <- p.adjust(pvals, "qvalue")
\dontrun{
  par(mfrow=c(2,2))
  hist(pvals)
  hist(bh)
  hist(bonf)
  hist(qvals)
}
}
\author{
  Mark Cowley 2008-05-05

  Mark Cowley
}
\seealso{
  \code{\link[stats]{p.adjust}}
  \code{\link[stats]{p.adjust.methods}}
  \code{\link{qvalue2}}

  \code{\link[=p.adjust]{p.adjust.methods}}
  \code{\link{p.adjust}}
}
\keyword{datasets}

