
## qtpaint backend

library("flattice")

library(qtbase)
library(qtpaint)

library("quilt")
## source.pkg("quilt")

scene <- qscene() # or Qt$QGraphicsScene()

dview <- Qt$QGraphicsView()
dview$setRenderHint(Qt$QPainter$Antialiasing, TRUE)
dview$size <- qsize(600, 600)
dview$show()
dview$setScene(scene)

view <- qplotView(scene = scene, opengl = FALSE)
print(view)


tplotPainter <- function(x, ..., context = NULL) # 'x' is a trellis object
{
    function(layer, painter) {
        primitives <- qtpaint_primitives(layer, painter, context = context)
        tplot(x, ..., primitives = primitives)
    }
}

data(Chem97, package = "mlmRev")

tplotLayer <-
    qlayer(scene,
           paintFun = tplotPainter(histogram(~ gcsescore | factor(score),
                                             data = Chem97)))

scene$clear()

qlayer(scene,
       paintFun = tplotPainter(densityplot(~ gcsescore | factor(score), data = Chem97, 
                                           plot.points = FALSE, ref = TRUE)))


## can we combine quilt and qtpaint primitives?

qp <- quilt_primitives(dview)
lattice.options(backend = qp)

scene$clear()

histogram(~ gcsescore | factor(score), data = Chem97)

qlayer(scene,
       paintFun = tplotPainter(densityplot(~ gcsescore | factor(score), data = Chem97, 
                                           plot.points = FALSE, ref = TRUE),
                               context = qp$tget_context()))


1








