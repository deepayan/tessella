
## can we redraw one layer without redrawing the other?

## try both qtpaint first

library(qtbase)
library(qtpaint)

scene <- qscene() # or Qt$QGraphicsScene()

view <- qplotView(scene = scene, opengl = FALSE)
print(view)


redPainter <- function(layer, painter) {
    qdrawCircle(painter,
                rnorm(100, 300, 100), rnorm(100, 200, 80),
                5, fill = "red")
}

redLayer <- qlayer(scene, paintFun = redPainter,
                   cache = TRUE) ## TRUE required

greenPainter <- function(layer, painter) {
    qdrawCircle(painter,
                rnorm(10, 300, 100), rnorm(10, 200, 80),
                5, fill = "green")
}

greenLayer <- qlayer(scene, paintFun = greenPainter,
                     cache = TRUE)

yellowPainter <- function(layer, painter) {
    qdrawCircle(painter,
                rnorm(10, 300, 100), rnorm(10, 200, 80),
                5, fill = "yellow")
}

yellowLayer <- qlayer(scene, paintFun = yellowPainter,
                     cache = TRUE)


clickHandler <- function(layer, event) {
    ## pos <- event$pos()
    ## str(list(x = pos$x(), pos$y()))
    qupdate(greenLayer)
}

clickLayer <- qlayer(scene, mousePressFun = clickHandler)

## Perfect!

#################################################


## Now how about qtbase + qtpaint in combination?

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

## view <- qplotView(scene = scene, opengl = FALSE)
## print(view)

lattice.options(backend = quilt_primitives(dview))

densityplot(~ waiting, data = faithful, ref = TRUE)

## redPainter <- function(layer, painter) {
##     qdrawCircle(painter,
##                 rnorm(100, 300, 100), rnorm(100, 200, 80),
##                 5, fill = "red")
## }

## redLayer <- qlayer(scene, paintFun = redPainter,
##                    cache = TRUE) ## TRUE required

greenPainter <- function(layer, painter) {
    qdrawCircle(painter,
                rnorm(10, 300, 100), rnorm(10, 200, 80),
                5, fill = "green")
}

greenLayer <- qlayer(scene, paintFun = greenPainter,
                     cache = TRUE)

## yellowPainter <- function(layer, painter) {
##     qdrawCircle(painter,
##                 rnorm(10, 300, 100), rnorm(10, 200, 80),
##                 5, fill = "yellow")
## }

## yellowLayer <- qlayer(scene, paintFun = yellowPainter,
##                      cache = TRUE)


clickHandler <- function(layer, event) {
    ## pos <- event$pos()
    ## str(list(x = pos$x(), pos$y()))
    qupdate(greenLayer)
}

clickLayer <- qlayer(scene, mousePressFun = clickHandler)

