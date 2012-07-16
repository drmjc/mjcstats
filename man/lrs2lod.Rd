\name{lrs2lod}
\alias{lrs2lod}
\title{Convert LRS to LOD}
\usage{
  lrs2lod(lrs)
}
\arguments{
  \item{lrs}{a numeric vector of LRS scores}
}
\value{
  a numeric vector of LOD scores
}
\description{
  LOD = LRS / 2 ln(10)\cr LRS is chi-sq distributed\cr
}
\examples{
lrs2lod(29.80)
# 6.47
lrs2lod(30.99)
# 6.73
lrs2lod(31.91)
# 6.93
}
\author{
  Mark Cowley, 30 June 2005
}
\references{
  \url{http://statmaster.sdu.dk/courses/st115/module08/index.html}
}

