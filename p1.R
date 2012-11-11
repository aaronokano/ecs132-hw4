ssq <- function(n) {
    # normalized population
    X <- rnorm(n, mean=0, sd=2)
    # return variance
    (1/n) * sum( (X - mean( X) )^2)
}

P1 <- function(nreps) {
    # run for length repetitions
    tmp <- vector(length=100)
    # s^2/sigma^2 is chi-squared w/ 10 df
    print(pchisq(24.8/2^2,df=10))
    # see proportion of s^2 < 24.8
    print(mean(sapply(tmp,function(x) {ssq(1000)}) < 24.8))
}

P1(1000)