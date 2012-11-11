P3 <- function(nreps,p1,p2,p3) {
    X <- sample(0:1,nreps,replace=TRUE,prob=c(p1,1-p1))
    Y <- sample(0:1,nreps,replace=TRUE,prob=c(p2,1-p2))
    Z <- sample(0:1,nreps,replace=TRUE,prob=c(p3,1-p3))
    print(mean(Z > 2*X*Y))
}

P3(1000,0.11,0.16,0.05)