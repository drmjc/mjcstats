\name{lod2p}
\alias{lod2p}
\title{Convert a LOD score to a P-value.}
\usage{
  lod2p(lod, df = 1)
}
\arguments{
  \item{lod}{a numeric vector of LOD scores}

  \item{df}{degrees of freedom}
}
\value{
  a numeric vector of p-values
}
\description{
  The simple interpretation of a LOD score is the log of
  the odds, ie 3 means 1:1000 odds. Simple conversion to P
  is just 10^-lod ie lod 3 becomes 0.001

  Accoring to Peirce2006 p646: \cr convert lrs2p via p =
  10^-(LRS/4.6); where 4.6 = 2 ln(10) \cr and lod = LRS/4.6
  \cr thus p = 10^-lod \cr
}
\details{
  The more accurate method of converting LRS scores to
  Pvalues is to transform them to chisquared variables, you
  need to know the degrees of freedom (number of different
  genotypes - 1; ie df=1 for inbred mice, df=2 for outbred
  SNPs, df= >2 for microsat alleles). Remember, chi sq
  distributions are defined only by the degrees of freedom
  (http://en.wikipedia.org/wiki/Chi-square_distribution).
}
\examples{
lod2p(lrs2lod(24.3))
# [1] 5.29e-06
lrs2p( 5.28*2*log(10), 2)
# [1] 5.25e-06
}
\author{
  Mark Cowley, 20/9/07
}
\references{
  \url{http://dx.doi.org/10.1007/s00335-005-0187-8}
}

