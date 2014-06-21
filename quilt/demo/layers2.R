


## Example 1: qtpaint layers

## Here I'm combining multiple qtpaint layers and updating one without
## redrawing others.  This works perfectly.

library(qtpaint)
library(qtbase)

makePainter <- function(fill)
{
    function(layer, painter) {
        qdrawCircle(painter,
                    500 * runif(10),
                    500 * runif(10), 10,
                    fill = fill)
    }
}

scene <- qscene()

redLayer <- qlayer(scene, makePainter("red"), cache = TRUE)
greenLayer <- qlayer(scene, makePainter("green"), cache = TRUE)
yellowLayer <- qlayer(scene, makePainter("yellow"), cache = TRUE)

view <- qplotView(scene = scene, opengl = FALSE)
view$size <- qsize(500, 500)
print(view)

qupdate(scene) ## redraws all layers
qupdate(greenLayer) ## redraws only green layer 



## Example 2: qtbase + qtpaint layers

## Now I want to do the same, but with one static layer rendered using
## standard Qt QGraphicsView tools.



library(qtpaint)
library(qtbase)

makePainter <- function(fill)
{
    function(layer, painter) {
        qdrawLine(painter,
                  500 * runif(10),
                  500 * runif(10), #10,
                  stroke = fill)
    }
}

scene <- qscene()

p <- Qt$QPen()
b <- Qt$QBrush(qcolor("orange"))
for (i in 1:10)
{
    scene$addEllipse(500 * runif(1), 500 * runif(1),
                     50, 50, p, b)
}

greenLayer <- qlayer(scene, makePainter("green"), cache = TRUE)
yellowLayer <- qlayer(scene, makePainter("yellow"), cache = TRUE)

view <- qplotView(scene = scene, opengl = FALSE)
view$size <- qsize(500, 500)
print(view)

qupdate(scene) ## redraws all layers
qupdate(greenLayer) ## redraws only green layer 



## Example 3: qtbase + qtpaint layers

## More complicated


library(qtpaint)
library(qtbase)
library(qtutils)

makePainter <- function(fill)
{
    function(layer, painter) {
        qdrawCircle(painter,
                    500 * runif(10),
                    500 * runif(10), 10,
                    fill = fill)
    }
}

scene <- qscene()

qsceneDevice(5, 5, rscene = scene)
library(lattice)

densityplot(~ waiting, data = faithful, ref = TRUE)

## barchart(VADeaths)


## p <- Qt$QPen()
## b <- Qt$QBrush(qcolor("orange"))
## for (i in 1:10)
## {
##     scene$addEllipse(500 * runif(1), 500 * runif(1),
##                      50, 50, p, b)
## }

greenLayer <- qlayer(scene, makePainter("green"), cache = TRUE)
yellowLayer <- qlayer(scene, makePainter("yellow"), cache = TRUE)

view <- qplotView(scene = scene, opengl = FALSE)
view$size <- qsize(500, 500)
print(view)

qupdate(scene) ## redraws all layers
qupdate(greenLayer) ## redraws only green layer 

greenLayer$setZValue(200)

for (i in 1:10)
{
    Sys.sleep(0.1)
    qupdate(greenLayer)
}



