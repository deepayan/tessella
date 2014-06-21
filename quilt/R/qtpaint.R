
## Primitives implemented using the qtpaint API.  Needs some thought
## on how the two models interact.

## demo

if (FALSE)
{

    df <- data.frame(x = rnorm(100, 300, 100),
                     y = rnorm(100, 200, 80))
    scatterPainter <- function(layer, painter) {
        ## message("layer: ")
        ## str(layer)
        ## print(as.integer(layer$ize))
        ## message("painter: ")
        ## str(painter)
        qdrawCircle(painter, df$x, df$y, 5)
    }

    linePainter <- function(layer, painter) {
        i <- sample(nrow(df), 10, replace = TRUE)
        qdrawLine(painter, df$x[i], df$y[i], "red")
    }

    
    library(qtpaint)
    library(qtbase)
    scene <- qscene()
    scatterLayer <- qlayer(scene, paintFun = scatterPainter)
    lineLayer <- qlayer(scene, paintFun = linePainter)

    ## for user scales:
    ## scatterLayer <- qlayer(scene, paintFun = scatterPainter,
    ##                    limits = qrect(0, 0, 600, 400))

    ## In the above, we first create a scene for the layer and then
    ## create the layer. The first argument to \Rfunction{qlayer} is
    ## for the ``parent,'' which can be either the scene (for a root
    ## layer) or another layer. It is not necessary to provide a
    ## parent at construction time, but it is usually most convenient.

    ## view <- qplotView(scene = scene, opengl = FALSE)
    ## print(view)

    dview <- Qt$QGraphicsView()
    dview$setRenderHint(Qt$QPainter$Antialiasing, TRUE)
    dview$size <- qsize(600, 600)
    print(dview)

    dview$setScene(scene)



}



qtpaint_primitives <- function(layer, painter, context = NULL)
{
    force(layer)
    force(painter)

    if (is.null(context))
    {
        g <- as.matrix(layer$geometry)
        context <-
            tcontext(0, 0, ## g[1,1], g[1,2],
                     diff(g[, 1]), diff(g[, 2]), invert.y = TRUE)
    }
    
    ## if (is.null(dview))
    ## {
    ##     dview <- Qt$QGraphicsView()
    ##     dview$setRenderHint(Qt$QPainter$Antialiasing, TRUE)
    ##     dview$size <- qsize(600, 600)
    ##     view$show()
    ## }
    ## if (is.null(dview$scene()))
    ## {
    ##     scene <- Qt$QGraphicsScene() # new scene 
    ##     dview$setScene(scene)
    ## }
    
    ## tget_context <- function(view = dview, scene = dview$scene())
    ## {
    ##     force(scene)
    ##     c <- tcontext(0, 0, view$width, view$height, invert.y = TRUE)
    ##     c$scene <- scene # extra 'user data'
    ##     c
    ## }

    tget_context <- function()
    {
        context
    }

    tinitialize <- function(context, newpage = TRUE)
    {
        ## if (newpage) context$scene$clear()
    }
    tfinalize <- function()
    {
        ## something to ensure display? Like
        Sys.sleep(0.01)
    }

    ## Only points (i.e., no other type)
    tpoints <- function(x, y,
                        pch = 1, col = 1, fill = "transparent", cex = 1,
                        lwd = 1, lty = 1, # for point boundary
                        ..., vp)
    {
        circle <- qglyphCircle() # FIXME: others depending on
                                 # pch. What if vectorized? Can be
                                 # list
        qtpaint::qdrawGlyph(painter, circle, 
                            x2pixel(x, vp), y2pixel(y, vp),
                            cex = cex, stroke = col, fill = fill)
    }
    tlines <- function(x, y,
                       lty = 1, lwd = 1, col = 1,
                       ..., vp)
    {
        qtpaint::qdrawLine(painter, x2pixel(x, vp), y2pixel(y, vp),
                           stroke = col)
    }
    tsegments <- function(x0, y0, x1 = x0, y1 = y0,
                          lty = 1, lwd = 1, col = 1,
                          ..., vp)
    {
        qtpaint::qdrawSegment(painter, 
                              x2pixel(x0, vp), y2pixel(y0, vp),
                              x2pixel(x1, vp), y2pixel(y1, vp),
                              stroke = col)
    }
    tpolygon <- function(x, y,
                         col = "black", fill = "transparent",
                         lty = 1, lwd = 1, #lend, lmitre,
                         fillOddEven = FALSE, ..., vp)
    {
        qtpaint::qdrawPolygon(painter, 
                              x2pixel(x, vp), y2pixel(y, vp),
                              stroke = col, fill = fill)
    }
    ttext <- function(x, y, labels = seq_along(x),
                      adj = c(0.5, 0.5), cex = 1, col = 1, rot = 0,
                      font = 1, family = "",
                      ..., vp)
    {
        adj <- rep(adj, length = 2) # not vectorized
        halign <- if (adj[1] < 0.5) "left" else if (adj[1] > 0.5) "right" else "center"
        valign <- if (adj[2] < 0.5) "bottom" else if (adj[1] > 0.5) "top" else "center"
        qtpaint::qdrawText(painter, text = labels,
                           x2pixel(x, vp), y2pixel(y, vp),
                           halign = halign, valign = valign,
                           rot = rot, cex = cex, color = col)
    }
    trect <- function(xleft, ybottom, xright, ytop,
                      fill = "transparent",
                      col = "black",
                      lty = 1, lwd = 1, 
                      ..., vp)
    {
        qtpaint::qdrawRect(painter, 
                           x2pixel(xleft, vp), y2pixel(ybottom, vp),
                           x2pixel(xright, vp), y2pixel(ytop, vp),
                           stroke = col, fill = fill)
    }
    tclip <- function(vp) 
    {
        ## clip(vp$x, vp$x + vp$w, vp$y, vp$y + vp$h)
    }
    tunclip <- function(vp) 
    {
        ## clip(vp$context$x, vp$context$x + vp$context$w, vp$context$y, vp$context$y + vp$context$h)
    }
    

    ## helper function to compute bounding box after rotation
    bbox_rot <- function(w, h, rot)
    {
        rad <- rot / 180 * base::pi
        sr <- sin(rad)
        cr <- cos(rad)
        m <- rbind(c(0, w, w, 0),
                   c(0, 0, h, h))
        r <- rbind(c(cr, -sr),
                   c(sr,  cr))
        v <- r %*% m
        apply(v, 1, function(x) diff(range(x)))
    }

    ## FIXME: should we change to use qtextExtents()-based metrics?
    ## They would be more accurate, but need to know the paint
    ## context, whereas this does not.

    tstrheight <- function(s, cex = 1, font = 1, family = "", rot = 0, ...)
    {
        txtitem <- Qt$QGraphicsSimpleTextItem()
        settext_fun <- txtitem$setText
        sapply(s,
               function(x) {
                   settext_fun(x)
                   bb <- txtitem$boundingRect()
                   bbox_rot(bb$width(), bb$height(), rot)[2]
               })
    }
    tstrwidth <- function(s, cex = 1, font = 1, family = "", rot = 0, ...)
    {
        txtitem <- Qt$QGraphicsSimpleTextItem()
        settext_fun <- txtitem$setText
        sapply(s,
               function(x) {
                   settext_fun(x)
                   bb <- txtitem$boundingRect()
                   bbox_rot(bb$width(), bb$height(), rot)[1]
               })
    }

    environment()
}
