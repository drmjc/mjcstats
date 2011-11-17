## Function to calculate the coefficient of variation.
##
## The coefficient of variation (CV) is a measure of dispersion of a probability distribution
##     (http://en.wikipedia.org/wiki/Coefficient_of_variation)
##
## CV is standard deviation / absolute mean of a sample * 100
##
## Mark Cowley, 22 June 2005
##
calc.cv <- function(x, as.percent=T) {
    multipier <- 1
    if( as.percent )
        multiplier <- 100

    return( multiplier * sd(x) / abs(mean(x)) )
}

Cv <- function(x, as.percent=T) {
    return( calc.cv(x, as.percent) )
}
