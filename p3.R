P3samples <- function(nsamples,p1,p2,p3) {
    X <- sum(sample(0:1,nsamples,replace=TRUE,prob=c(1-p1,p1)))
    Y <- sum(sample(0:1,nsamples,replace=TRUE,prob=c(1-p2,p2)))
    Z <- sum(sample(0:1,nsamples,replace=TRUE,prob=c(1-p3,p3)))
    return( Z > 2*X*Y )
}

P3 <- function(nreps) {
  f = vector( length=nreps )
  P <- sapply( f, function(x) P3samples(15,0.11,0.16,0.05) )
  mean(P)
}

P3( 10000 )
