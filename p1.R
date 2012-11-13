ssq1 <- function(n) {
    # normalized population
    X <- rnorm(n, mean=0, sd=2)
    # return variance
    (1/n) * sum( (X - mean( X) )^2)
}

ssq2 <- function(n) {
    # exponential population
    X <- rexp(n)
    # return variance
    (1/n) * sum( (X - mean( X) )^2)
}

P1 <- function(nreps) {
    # run for nreps repetitions
    tmp <- vector(length=nreps)
    # (n - 1)s^2/sigma^2 is chi-squared w/ 9 df
    chi <- pchisq(9*4.8/4,df=9)
    # proportion of s^2 < 4.8
    res1 <- mean(sapply(tmp,function(x) {ssq1(10)}) < 4.8)
    res2 <- mean(sapply(tmp,function(x) {ssq2(10)}) < 4.8)
    cat('Part A:\t',chi,'\t',res1,'\n')
    cat('Part B:\t',res2,'\n')
}

P1(1000)
