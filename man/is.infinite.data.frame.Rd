\name{is.infinite.data.frame}
\alias{is.infinite.data.frame}
\title{check if entries in a \code{data.frame} are infinite}
\usage{
  \method{is.infinite}{data.frame} (x)
}
\arguments{
  \item{x}{a \code{data.frame}}
}
\value{
  a \code{data.frame} of logicals.
}
\description{
  check if entries in a \code{data.frame} are infinite
}
\examples{
df <- data.frame(names=1:5, a=1:5, b=Inf)
is.infinite(df)
}
\author{
  Mark Cowley, 2012-10-19
}

