postscript("dotplotscoping.ps")
library(lattice0)

fubar <- function() {
    k <- 2
    kkk <- 1:10
    names(kkk) <- 1:10
    data = list(x=kkk)
    dotplot(~x^k + rnorm(10), data)
}

fubar()
dev.off()
