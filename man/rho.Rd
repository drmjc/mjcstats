\name{rho}
\alias{rho}
\title{Spearman's correlation (rho)}
\usage{
  rho(x, y = NULL, use = "all.obs")
}
\arguments{
  \item{x}{a numeric vector, matrix or data frame.}

  \item{y}{\code{NULL} (default) or a vector, matrix or
  data frame with compatible dimensions to \code{x}.  The
  default is equivalent to \code{y = x} (but more
  efficient).}

  \item{use}{an optional character string giving a method
  for computing covariances in the presence of missing
  values.  This must be (an abbreviation of) one of the
  strings \code{"everything"}, \code{"all.obs"},
  \code{"complete.obs"}, \code{"na.or.complete"}, or
  \code{"pairwise.complete.obs"}.}
}
\value{
  the Spearman's correlation
}
\description{
  Spearman's rho statistic is used to estimate a rank-based
  measure of association.  It is more robust than Pearson's
  correlation, and has been recommended if the data do not
  necessarily come from a bivariate normal distribution.
}
\details{
  This is a wrapper around \code{cor(method="spearman")}
}
\author{
  Mark Cowley, 9 Nov 2005
}
\seealso{
  \code{\link[stats]{cor}}
}

