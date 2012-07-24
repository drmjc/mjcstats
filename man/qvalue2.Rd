\name{qvalue2}
\alias{qvalue2}
\title{qvalue2}
\usage{
  qvalue2(p, lambda = seq(0, 0.9, 0.05),
    fallback = c("BH", "fdr", "BY")[1])
}
\arguments{
  \item{p}{a vector of p-values}

  \item{lambda}{The value of the tuning parameter to
  estimate pi_0. Must be in [0,1). Optional, see Storey
  (2002).}

  \item{fallback}{The fallback p.adjust method. Default =
  \dQuote{BH}, but can be one of the other FDR methods from
  \code{\link{p.adjust.methods}}}
}
\value{
  An object of class \code{qvalue}, even if the the FDR was
  estimated using the \code{fallback} option, where the
  latter has \code{pi0=1.0}.
}
\description{
  Estimate the q-values for a given set of p-values, in a
  safe manner. The most frequent error that I have seen in
  using \code{qvalue}, is that pertaining to the
  \dQuote{pi_0 estimate is less than zero} which is thrown
  as a result of not being able to estimate the lambda
  parameter. This function first attempts to generate
  qvalues using the default \dQuote{smoother} method. If
  that fails due to pi_0 estimate, then this is repeated
  using the \dQuote{bootstrap} method. If the same error is
  still thrown, then a fallback FDR estimation method is
  used, which defaults to \dQuote{BH}, the
  Benjamini-Hochberg FDR
}
\details{
  In addition, when running \code{library(qvalue)} in a
  headless server, you often get a warning: \code{In
  fun(libname, pkgname) : no DISPLAY variable so Tk is not
  available} which is actually thrown by tcltk. qvalue2
  suppresses that message. See references.
}
\examples{
p <- runif(100,0,1)
qvalue2(p)
# qv
pbad <- c( 2e-04, 3e-04, 5e-04, 6e-04, 8e-04, 8e-04, 9e-04, 9e-04, 0.001,
           0.001, 0.0013, 0.0013, 0.0013, 0.0014, 0.0016, 0.0017, 0.0019,
           0.0019, 0.0023, 0.0023, 0.0023, 0.0024, 0.0028, 0.0029, 0.0031,
           0.0032, 0.0032, 0.0032, 0.0034, 0.0034, 0.0035, 0.0037, 0.0038,
           0.0038, 0.0042, 0.0043, 0.0044, 0.0044, 0.0044, 0.0045 )
qvalue2(pbad)
# These will now throw errors:
\dontrun{
  qvalue2(p=c(-0.1,0,0.5,0.9,1.0))
  qvalue2(p, lambda=seq(-0.1,1,0.05))
  qvalue2(p, lambda=c(0,0.2,0.5))
}
}
\author{
  Mark Cowley, 2011-10-25
}
\references{
  \url{http://stackoverflow.com/a/11554019/178297}
}

