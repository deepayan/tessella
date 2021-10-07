
## an alternative rendering function using the tessella API

## library(lattice0)

library(tessella)
source.pkg("lattice0")
## .onLoad()


xyplot(Sepal.Length ~ Sepal.Width | Species, data = iris,
       main = "main label", layout = c(2, 2),
       ## scales = list(relation = "free", rot = 0),
       strip = strip.custom(strip.names = F))


xyplot(Sepal.Length ~ Sepal.Width | Species, data = iris,
       main = "main label", layout = c(2, 2),
       scales = list(relation = "free", rot = 0),
       strip.left = TRUE, strip = FALSE)

stripplot(Species ~ Sepal.Length, data = iris, jitter = TRUE,
          scales = list(y = list(relation = "free", rot = 45)),
          ylab = "Species")



Depth <- equal.count(quakes$depth, number=8, overlap=.1)
xyplot(lat ~ long | Depth, data = quakes)

xyplot(Sepal.Length ~ Sepal.Width, data = iris, groups = Species,
       jitter.x = TRUE, jitter.y = TRUE, grid = TRUE, 
       main = "main label")

