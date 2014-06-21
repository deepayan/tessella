
## testing

library(tessella)

N <- 1000

mydata <-
    data.env(data.frame(x = 1:N, y = rnorm(N, mean = (5/N) * 1:N), z = runif(N),
                        g = gl(3, 1, N, labels = month.name[1:3]),
                        a = gl(5, 1, N)))


m <- tessella.xyplot(x = x, y = y, groups = g,
                      data = mydata, margin = ~a,
                      type = "b", layout = c(2, 3),
                      legend.args = list(type = "l", title = "Groups"),
                      main = "An example")


## m <- tessella.xyplot(x = Sepal.Length, y = Petal.Length, groups = Species, data = iris, 
##                      legend.args = list(type = "p", title = "Groups"),
##                      main = "Fisher iris data")


library(qtbase)
source("../R/primitives.R")

view <- Qt$QGraphicsView()
view$setRenderHint(Qt$QPainter$Antialiasing, TRUE)
view$size <- qsize(600, 600)
view

quilt(m, view = view)

