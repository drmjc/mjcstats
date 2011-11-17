## Hypergeometric Distribution Probability
##
## probability of exactly M successes of K elements drawn in a pool of
##  S favorable elements from a total of N elements
##
## eg: What is the probability of getting exactly 5 winners out of 6 in my
##     pool of 12 numbers from a total of 49 lotto numbers?
##    M = 5, K = 6, S = 12, N = 49
##
## http://www.saliu.com/theory-of-probability.html
HDF <- function(M, K, S, N) {
    1 / ( choose(N, K) / ( choose(S, M) * choose(N-S, K-M) ) )
}

## Binomial Distribution Probability
##     inseparable events, multiple trials
##
## probability of exactly M successes from N trials where
##  probability of success in each event is p
##
## eg: probability of tossing exactly 5 heads from 10 tosses
##    M=5  N=10  p=1/2
##
## http://www.saliu.com/theory-of-probability.html
BDF <- function(p, M, N) {
    choose(N, M) * p^M * (1 - p)^(N - M)
}


