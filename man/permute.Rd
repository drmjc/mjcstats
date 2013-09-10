\name{permute}
\alias{permute}
\title{permute}
\usage{
  permute(x)
}
\arguments{
  \item{x}{a numeric or character vector}
}
\value{
  a numeric or characer vector, same length as 'x', with
  the order rearranged
}
\description{
  Permute the elements in x. Ie shuffle the order, with no
  replacement.
}
\examples{
permute(1:10)
permute(letters)
}
\author{
  Mark Cowley, 2013-08-28
}
\seealso{
  \code{\link{sample}}
}

