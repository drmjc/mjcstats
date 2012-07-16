#' convert lrs to p-values
#' 
#' Convert likelihood ratio statistics (LRS) to p-values (P), with an
#' appropriate df (degrees of freedom). 
#' 
#' @note This was developed in the context of
#' mapping eQTL data in BXD recombinant mice
#' 
#' @references \url{http://dx.doi.org/10.1007/s00335-005-0187-8}
#'
#' @param x a numeric vector, \code{matrix} or \code{data.frame} of LRS statistics.
#'  even a table of data with an \dQuote{lrs} column is supported.
#' 
#' @param df degrees of freedom
#' 
#' @return p-values of the same dimension as \code{x}
#' 
#' @author Mark Cowley, 30 June 2005
#' @export
#' 
#' @examples
#' lrs2p( 5.28*2*log(10), 2)
#' # [1] 5.25e-06         
#' lod2p(lrs2lod(24.3))     
#' # [1] 5.29e-06           
lrs2p <- function(x, df=1) {
    if( "lrs" %in% names(x) ) {
        ## Then x is the list containing LRS and p matrices...
        ## Overwrite the p matrix
        x$p <- lrs2p(x$lrs, df=df)
        x$nperm <- 0
        return( x )
    }
    else if(is.data.frame(x) | is.matrix(x)) {
        x <- apply(x, 2, lrs2p)
        return(x)
    }
    else {
        if( all(is.na(x)) )
            return(x)
        else
            return( pchisq(x, df, lower.tail=F) )
    }
}

#
# What is the justification for this???
# Peirce et al, explicitly use qtlreaper, and say:
#   p = 10^-(LRS/4.6) where 4.61 = 2 ln(10)
# However the pvalue histogram is REALLY skewed towards p = 1
#     I DONT agree that you can convert LOD scores to P-values
#     just using the 10^-lod transformation...
#     LOD -> LRS -> P is the correct way, backed up by 2 stat. genet.
#     methods,
#   http://www.sph.umich.edu/csg/abecasis/LAMP/tour/association.html
#   http://statmaster.sdu.dk/courses/st115/module08/index.html
#
# The above function make P-values that are somewhat flat, but
# there are tails at P close to 0 (as you'd hope for the true
# alternative hypotheses) but there is also strange behaviour
# near the p=1 tail.
#
# Abecasis says:
# http://www.sph.umich.edu/csg/abecasis/LAMP/tour/association.html
#     LAMP summarizes evidence for association using a LOD score. You
#     can easily convert a LOD score into a chi-squared statistic by
#     multiplying it by 2ln(10) (about 4.61, according to google).
#     In genetics, LOD scores are commonly used in settings where many
#     statistical tests are performed -- such as most SNP association studies.
# further:
#     a LOD score of 5.28 w/ 2df gets P of 5x10^-6
# Using my code: lod2p(5.28)
#     [1] 5.25e-06
# which is the same as:
#   1) convert LOD to "a chi sq statistic"
#       5.28*2*log(10) = 5.28*4.61 = 24.3
#   2) convert the "chi sq statistic" to a P-value using df=2
#       lrs2p( 24.3 ) = 5.25e-06
#  == my implementation of lrs2p
#  which means:
#   the "chi squared statistic" is an LRS score
#   we can convert from lrs to lod to p == lrs2p
# lrs2p( 5.28*2*log(10), 2)
#     [1] 5.25e-06
# lod2p(lrs2lod(24.3))
#   [1] 5.29e-06
#
# CONCLUSION:
#   to get P values from LOD scores you need to convert LOD -> LRS (mult by 2ln(10) )
#   then LRS to P using chi sq and appropriate degrees of freedom.
#
#