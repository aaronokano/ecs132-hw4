ssq <- function(n) {
    # normalized population
    X <- rnorm(n, mean=0, sd=2)
    # return variance
    (1/n) * sum( (X - mean( X) )^2)
}

P1 <- function(nreps) {
    # run for nreps repetitions
    tmp <- vector(length=nreps)
    # (n - 1)s^2/sigma^2 is chi-squared w/ 9 df
    print(pchisq(9*4.8/4,df=9))
    # see proportion of s^2 < 4.8
    print(mean(sapply(tmp,function(x) {ssq(10)}) < 4.8))
}

P1(1000)
