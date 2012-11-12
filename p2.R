X <- function(n,r1,r2) {
    p12 <- sample(c(0,1,0,1),n,replace=TRUE,prob=c(r1,1-r1,r2,1-r2))
    ep12 <- mean(p12)
    ep12
}

Y <- function(n,q,r1,r2) {
    p1 <- sample(0:1,q*n,replace=TRUE, prob=c(r1,1-r1))
    p2 <- sample(0:1,(1-q)*n,replace=TRUE, prob=c(r2,1-r2))
    ep1 <- mean(p1)
    ep2 <- mean(p2)
    (ep1+ep2)/2
}

P2 <- function(nreps) {
    X <- vector(length=nreps)
    Y <- vector(length=nreps)
    X <- sapply(X, function(x) {X(1000,0.25,0.75)})
    Y <- sapply(Y, function(y) {Y(1000,.5,0.25,0.75)})
    print(var(X))
    print(var(Y))
    print(var(Y)/var(X))
}

P2(1000)
