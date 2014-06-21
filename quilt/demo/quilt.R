
## QGraphicsView backend

library("flattice")
library("quilt")

lattice.options(backend = quilt_primitives())

data(Chem97, package = "mlmRev")

histogram(~ gcsescore | factor(score), data = Chem97)

densityplot(~ gcsescore | factor(score), data = Chem97, 
            plot.points = FALSE, ref = TRUE)


