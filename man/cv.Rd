\name{cv}
\alias{cv}
\title{coefficient of variance}
\usage{
  cv(x, as.percent = FALSE, na.rm = TRUE)
}
\arguments{
  \item{x}{a numeric vector of positive values}

  \item{as.percent}{logical: return result as a
  percentage?}

  \item{na.rm}{logical: remove \code{NA}'s ?}
}
\value{
  a numeric(1)
}
\description{
  The coefficient of variation (CV) is defined as the ratio
  of the standard deviation to the mean. As such, it is a
  measure of dispersion of a probability distribution. It
  can only be computed on for data on a ratio scale (ie all
  positive).
}
\examples{
val <- runif(25)
cv(val)
cv(val, as.percent=TRUE)
cv(c(val, NA), na.rm=FALSE)
}
\author{
  Mark Cowley, 2012-07-09
}
\references{
  \url{http://en.wikipedia.org/wiki/Coefficient_of_variation}
}

