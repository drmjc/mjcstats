\name{lod2lrs}
\alias{lod2lrs}
\title{convert LOD to LRS scores}
\usage{
  lod2lrs(lod)
}
\arguments{
  \item{lod}{a numeric vector of LOD scores}
}
\value{
  a numeric vector of LRS scores
}
\description{
  Transform LOD scores to become chi-squared by multiplying
  be 2 ln(10)
}
\author{
  Mark Cowley, 20/9/07
}
\references{
  \url{http://www.sph.umich.edu/csg/abecasis/LAMP/tour/association.html}
  \url{http://statmaster.sdu.dk/courses/st115/module08/index.html}
}

