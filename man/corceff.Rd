\name{corceff}
\alias{corceff}
\title{corceff}
\usage{
  corceff(x, y = NULL, use = "everything",
    method = c("pearson", "kendall", "spearman"))
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

  \item{method}{a character string indicating which
  correlation coefficient (or covariance) is to be
  computed.  One of \code{"pearson"} (default),
  \code{"kendall"}, or \code{"spearman"}, can be
  abbreviated.}
}
\value{
  see \code{\link[stats]{cor}}
}
\description{
  corceff is the matlab function name for
  \code{\link[stats]{cor}}
}
\author{
  Mark Cowley
}

