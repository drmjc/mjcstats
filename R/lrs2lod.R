## Function to convert LRS scores to LOD scores.
## LOD = LRS / 2 ln(10)
## LRS is chi-sq distributed
## Mark Cowley, 30 June 2005
##
## confirmation of results via Abecasis' MERLIN output:
##  http://statmaster.sdu.dk/courses/st115/module08/index.html
#             Position      H2    ChiSq     LOD  pvalue
#               90.000   54.47%   29.80    6.47 0.00000
#               91.000   57.15%   30.99    6.73 0.00000
#               92.000   58.89%   31.91    6.93 0.00000
## lrs2lod(29.80) -> 6.47
##
lrs2lod <- function(lrs) {
	return(lrs/(2*log(10)))
}

## Calculate the mab distance using inverse Kosambi function.
## NB, x must be in range [0, 0.5]
##
## http://www.mapmanager.org/MMM/MMMAlgorithms.html#996839
##
## Mark Cowley, 30 June 2005
##
inverse.kosambi <- function(x) {
	return( 0.25*(log(1+2*x)) - log(1-2*x) )
}

#
# Convert a LOD score to a P-value.
#
# The simple interpretation of a LOD score is the log
# of the odds, ie 3 means 1:1000 odds.
# Simple conversion to P is just 10^-lod
# ie lod 3 becomes 0.001
#
#     Accoring to Peirce2006 p646:
#     convert lrs2p via p = 10^-(LRS/4.6); where 4.6 = 2 ln(10)
#     and lod = LRS/4.6
#     thus p = 10^-lod
#
# The more accurate method of converting LRS scores to Pvalues
# is to transform them to chisquared variables, you need to know
# the degrees of freedom (number of different genotypes - 1; ie
# df=1 for inbred mice, df=2 for outbred SNPs, df= >2 for microsat
# alleles). Remember, chi sq distributions are defined only by the
# degrees of freedom (http://en.wikipedia.org/wiki/Chi-square_distribution).
#
# Mark Cowley, 20/9/07
#
lod2p <- function(lod, df=1) {
    return( lrs2p(lod2lrs(lod), df=df) )
#     return( 10^ -lod )
}


#
# Transform LOD scores to become chi-squared by multiplying be
# 2 ln(10)
#   http://www.sph.umich.edu/csg/abecasis/LAMP/tour/association.html
#   http://statmaster.sdu.dk/courses/st115/module08/index.html
#
# Mark Cowley, 20/9/07
lod2lrs <- function(lod) {
    return( lod * (2*log(10)) )
}
