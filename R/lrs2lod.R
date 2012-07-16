#' Convert LRS to LOD
#' 
#' LOD = LRS / 2 ln(10)\cr
#' LRS is chi-sq distributed\cr
#' 
#' @param lrs a numeric vector of LRS scores
#' @return a numeric vector of LOD scores
#' 
#' @author Mark Cowley, 30 June 2005
#'
#' @references \url{http://statmaster.sdu.dk/courses/st115/module08/index.html}
#' @export
#' @examples
#' lrs2lod(29.80)
#' # 6.47
#' lrs2lod(30.99)
#' # 6.73
#' lrs2lod(31.91)
#' # 6.93
#' 
lrs2lod <- function(lrs) {
	return(lrs/(2*log(10)))
}
## confirmation of results via Abecasis' MERLIN output:
##  http://statmaster.sdu.dk/courses/st115/module08/index.html
#             Position      H2    ChiSq     LOD  pvalue
#               90.000   54.47%   29.80    6.47 0.00000
#               91.000   57.15%   30.99    6.73 0.00000
#               92.000   58.89%   31.91    6.93 0.00000


#' Convert a LOD score to a P-value.
#'
#' @description The simple interpretation of a LOD score is the log
#' of the odds, ie 3 means 1:1000 odds.
#' Simple conversion to P is just 10^-lod
#' ie lod 3 becomes 0.001
#'
#'     Accoring to Peirce2006 p646:                              \cr
#'     convert lrs2p via p = 10^-(LRS/4.6); where 4.6 = 2 ln(10) \cr
#'     and lod = LRS/4.6                                         \cr
#'     thus p = 10^-lod                                          \cr
#'
#' @details The more accurate method of converting LRS scores to Pvalues
#' is to transform them to chisquared variables, you need to know
#' the degrees of freedom (number of different genotypes - 1; ie
#' df=1 for inbred mice, df=2 for outbred SNPs, df= >2 for microsat
#' alleles). Remember, chi sq distributions are defined only by the
#' degrees of freedom (http://en.wikipedia.org/wiki/Chi-square_distribution).
#'
#' @param lod a numeric vector of LOD scores
#' @param df degrees of freedom
#' @return a numeric vector of p-values
#' 
#' @export
#' 
#' @author Mark Cowley, 20/9/07
#' @references \url{http://dx.doi.org/10.1007/s00335-005-0187-8}
#'
#' @examples
#' lod2p(lrs2lod(24.3))     
#' # [1] 5.29e-06           
#' lrs2p( 5.28*2*log(10), 2)
#' # [1] 5.25e-06         
#' 
lod2p <- function(lod, df=1) {
    return( lrs2p(lod2lrs(lod), df=df) )
}


#' convert LOD to LRS scores
#' 
#' Transform LOD scores to become chi-squared by multiplying be
#' 2 ln(10)
#' 
#' @param lod a numeric vector of LOD scores
#' @return  a numeric vector of LRS scores
#' 
#' @export
#' 
#' @references \url{http://www.sph.umich.edu/csg/abecasis/LAMP/tour/association.html} 
#'  \url{http://statmaster.sdu.dk/courses/st115/module08/index.html}
#'
#' @author Mark Cowley, 20/9/07
lod2lrs <- function(lod) {
    return( lod * (2*log(10)) )
}
