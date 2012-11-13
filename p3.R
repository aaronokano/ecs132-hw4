P3samples <- function(nsamples,p1,p2,p3) {
    X <- sum(sample(0:1,nsamples,replace=TRUE,prob=c(1-p1,p1)))
    Y <- sum(sample(0:1,nsamples,replace=TRUE,prob=c(1-p2,p2)))
    Z <- sum(sample(0:1,nsamples,replace=TRUE,prob=c(1-p3,p3)))
    return( (Z/nsamples) > 2*(X/nsamples)*(Y/nsamples) )
}

P3 <- function(nreps) {
  f = vector( length=nreps )
  P <- sapply( f, function(x) P3samples(15,0.11,0.16,0.05) )
  cat('Mean is ',mean(P), '\n')
}

P3( 10000 )
