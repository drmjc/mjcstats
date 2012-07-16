\name{lrs2p}
\alias{lrs2p}
\title{convert lrs to p-values}
\usage{
  lrs2p(x, df = 1)
}
\arguments{
  \item{x}{a numeric vector, \code{matrix} or
  \code{data.frame} of LRS statistics.  even a table of
  data with an \dQuote{lrs} column is supported.}

  \item{df}{degrees of freedom}
}
\value{
  p-values of the same dimension as \code{x}
}
\description{
  Convert likelihood ratio statistics (LRS) to p-values
  (P), with an appropriate df (degrees of freedom).
}
\note{
  This was developed in the context of mapping eQTL data in
  BXD recombinant mice
}
\examples{
lrs2p( 5.28*2*log(10), 2)
# [1] 5.25e-06
lod2p(lrs2lod(24.3))
# [1] 5.29e-06
}
\author{
  Mark Cowley, 30 June 2005
}
\references{
  \url{http://dx.doi.org/10.1007/s00335-005-0187-8}
}

