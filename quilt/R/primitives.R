
## .QuiltEnv <- new.env(parent = emptyenv())

## quilt <- function(x, page = 1, ..., view, scene,
##                   container = .QuiltEnv$container, wait = TRUE)
## {
##     if (missing(view)) 
##     {
##         view <- Qt$QGraphicsView()
##         view$setRenderHint(Qt$QPainter$Antialiasing, TRUE)
##         view$size <- qsize(700, 600)
##         view
##     }
##     if (is.null(container)) view$show() else
##     {
##         container$setCurrentIndex(container$addTab(view, "foo"))
##     }
##     if (missing(scene)) 
##     {
##         scene <- Qt$QGraphicsScene() # new scene 
##         view$setScene(scene) # (what happen to old scene?)
##     }
##     else scene$clear()
##     cont <- quilt_context(view, scene)
##     gvp <- tviewport(cont)
##     gvp$ylim <- c(0, 1)
##     tessella.page(x, page = 1, vp = gvp, ..., primitives = quilt_primitives())
##     view$update()
##     if (wait) Sys.sleep(0.01)
##     invisible(x)
## }

## quilt_context <- function(view, scene = view$scene())
## {
##     .Deprecated("quilt_primitives")
##     c <- tcontext(0, 0, view$width, view$height, invert.y = TRUE)
##     c$scene <- scene
##     c
## }



## To make this more efficient, we probably need to do a lot of this
## in compiled code.  The plan is to retain an R-level prototype, but
## also implement C++-level alternatives.

## So far we have only ported tpolygon and trect to C++


## FIXME: need to make all items have a common parent for stacking
## purposes.  Also check other tune-ups in QT() device - because that
## seems to work better when adding qtpaint layers on top.

quilt_primitives <- 
qtbase_primitives <- function(dview = NULL, compiled = TRUE)
{
    force(dview)
    force(compiled)
    if (is.null(dview))
    {
        dview <- Qt$QGraphicsView()
        dview$setRenderHint(Qt$QPainter$Antialiasing, TRUE)
        dview$size <- qsize(600, 600)
        dview$show()
    }
    if (is.null(dview$scene()))
    {
        scene <- Qt$QGraphicsScene() # new scene 
        dview$setScene(scene)
    }
    
    tget_context <- function(view = dview, scene = dview$scene())
    {
        force(scene)
        c <- tcontext(0, 0, view$width, view$height, invert.y = TRUE)
        c$scene <- scene # extra 'user data'
        c
    }

    tinitialize <- function(context, newpage = TRUE)
    {
        if (newpage) context$scene$clear()
    }
    tfinalize <- function()
    {
        ## something to ensure display? Like
        Sys.sleep(0.01)
    }

    maxlength <- function(...)
    {
        max(sapply(list(...), length))
    }
    anyna <- function(...)
    {
        as.logical(do.call(pmax, lapply(list(...), is.na)))
    }
    ## shortcut: may not be most efficient, but simple to code 
    mypen <- function(col = "black", lwd = 1, lty = 1)
    {
        qpen(qbrush(color = col), width = lwd, style = as.integer(lty))
    }
    mybrush <- function(fill = "transparent")
    {
        qbrush(color = fill)
    }

    ## standard R definition to qfont arguments
    fontdef <- function(cex = 1, font = 1, family = "", pointsize = 10)
    {
        ## font=1 - normal, 2 - bold, etc.
        ans <- list(pointsize = pointsize * cex)
        if (!is.null(font) && font != 1)
        {
            if (font %in% c(2, 4)) ans$weight <- Qt$QFont$Bold
            else if (font %in% c(3, 4)) ans$italic <- TRUE
            else warning("'font' has unrecognized value ", font)
        }
        if (!is.null(family) && nzchar(family)) ans$family <- family
        ans
    }

    mytext <- function(scene, x, y, s, adj, rot, brush = qbrush(), font = list(), html = FALSE)
    {
        ## return(NULL)
        if (html)
        {
            txtitem <- Qt$QGraphicsTextItem()
            settext_fun <- txtitem$setHtml
            txtitem$setDefaultTextColor(brush$color())
        }
        else
        {
            txtitem <- Qt$QGraphicsSimpleTextItem()
            settext_fun <- txtitem$setText
            txtitem$setBrush(brush)
        }
        ## font can have: "family", "pointsize", "weight", "italic"
        ## qstr <- Qt$QString$fromUtf8(s) # doesn't work
        
        ## i <- scene$addText(qstr, do.call(qfont, font))
        settext_fun(s)
        txtitem$setFont(do.call(qfont, font))
        brect <- txtitem$boundingRect()
        ## transformation
        t <- Qt$QTransform()
        t$translate(x, y);
        t$rotate(-rot);
        ## OR based on Qt$QFontMetrics(font)$ascent()? see QT() device
        t$translate(-adj[1] * brect$width(), -(1-adj[2]) * brect$height())
        txtitem$setTransform(t);
        ## txtitem$rotate(-rot);
        ## txtitem$translate(-adj[1] * brect$width(), -(1-adj[2]) * brect$height())
        ## txtitem$setPos(x, y)
        scene$addItem(txtitem);
    }

    ## circlePch <- (function()
    ##           {
    ##               p <- Qt$QPainterPath()
    ##               p$addEllipse(-5, -5, 10, 10)
    ##               p
    ##           })()

    ## Only points (i.e., no other type)
    tpoints <- function(x, y,
                        pch = 1, col = 1, fill = "transparent", cex = 1,
                        lwd = 1, lty = 1, # for point boundary
                        ..., vp)
    {
        ## graphics::points(x2pixel(x, vp), y2pixel(y, vp),
        ##                  type = "p",
        ##                  pch = pch, col = col, bg = fill, cex = cex,
        ##                  lwd = lwd, lty = lty)
        n <- maxlength(x, y)
        vpar <- maxlength(pch, col, fill, lwd, lty) > 1
        r <- 5 * cex # radius
        r <- rep(r, length.out = n)
        d <- 2 * r # diameter
        x <- rep(x2pixel(x, vp), length.out = n) - r
        y <- rep(y2pixel(y, vp), length.out = n) - r
        if (vpar)
        {
            ## pch <- rep(pch, length.out = n) # FIXME: ignored
            col <- rep(col, length.out = n)
            fill <- rep(fill, length.out = n)
            lwd <- rep(lwd, length.out = n)
            lty <- rep(lty, length.out = n)
            for (i in which(!anyna(x, y)))
            {
                vp$context$scene$addEllipse(x[i], y[i], d[i], d[i], mypen(col[i], lwd[i], lty[i]), mybrush(fill[i]))
                ## circlePch$setPos(x[i], y[i])
                ## vp$context$scene$addItem(circlePch);
            }
        }
        else
        {
            p <- mypen(col, lwd, lty)
            b <- mybrush(fill)
            for (i in which(!anyna(x, y)))
            {
                vp$context$scene$addEllipse(x[i], y[i], d[i], d[i], p, b)
                ## pitem <- vp$context$scene$addPath(circlePch, p, b)
                ## pitem$setPos(x[i], y[i])
            }
        }
    }
    tlines <- function(x, y,
                       lty = 1, lwd = 1, col = 1,
                       ..., vp)
    {
        ## ## allow lend, ljoin, lmitre?
        ## graphics::lines(x2pixel(x, vp), y2pixel(y, vp),
        ##                 type = "l",
        ##                 lty = lty, lwd = lwd, col = col)
        n <- maxlength(x, y)
        x <- rep(x2pixel(x, vp), length.out = n)
        y <- rep(y2pixel(y, vp), length.out = n)
        p <- mypen(col, lwd, lty)
        isna <- is.na(x[-1]) | is.na(y[-1]) | is.na(x[-n]) | is.na(x[-n])
        for (i in 1L + which(!anyna(x[-1], y[-1], x[-n], x[-n])))
        {
            vp$context$scene$addLine(x[i-1], y[i-1], x[i], y[i], p)
        }
    }
    tsegments <- function(x0, y0, x1 = x0, y1 = y0,
                          lty = 1, lwd = 1, col = 1,
                          ..., vp)
    {
        ## graphics::segments(x2pixel(x0, vp), y2pixel(y0, vp),
        ##                    x2pixel(x1, vp), y2pixel(y1, vp),
        ##                    lty = lty, lwd = lwd, col = col)
        n <- maxlength(x0, y0, x1, y1)
        vpar <- maxlength(col, lwd, lty) > 1
        x0 <- rep(x2pixel(x0, vp), length.out = n)
        y0 <- rep(y2pixel(y0, vp), length.out = n)
        x1 <- rep(x2pixel(x1, vp), length.out = n)
        y1 <- rep(y2pixel(y1, vp), length.out = n)
        if (vpar)
        {
            col <- rep(col, length.out = n)
            lwd <- rep(lwd, length.out = n)
            lty <- rep(lty, length.out = n)
            for (i in which(!anyna(x0, y0, x1, y1)))
            {
                vp$context$scene$addLine(x0[i], y0[i], x1[i], y1[i], mypen(col[i], lwd[i], lty[i]))
            }
        }
        else
        {
            p <- mypen(col, lwd, lty)
            for (i in which(!anyna(x0, y0, x1, y1)))
            {
                vp$context$scene$addLine(x0[i], y0[i], x1[i], y1[i], p)
            }
        }
    }
    tpolygon <- function(x, y,
                         col = "black", fill = "transparent", lty = 1, lwd = 1, #lend, lmitre,
                         fillOddEven = FALSE, ..., vp)
    {
        ## graphics::polygon(x2pixel(x, vp), y2pixel(y, vp),
        ##                   border = col, col = fill, lty = lty, lwd = lwd, fillOddEven = fillOddEven)

        n <- maxlength(x, y)
        vpar <- maxlength(col, fill, lwd, lty) > 1
        x <- rep(x2pixel(x, vp), length.out = n)
        y <- rep(y2pixel(y, vp), length.out = n)

        ## need to break by NA-s (taking care of consective NAs)

        wna <- which(is.na(x) | is.na(y))
        if (length(wna) == 0) wna <- n + 1
        else if (wna[length(wna)] != n) wna <- c(wna, n + 1)
        npoly <- length(wna)
        iend <- as.integer(wna - 1L)
        istart <- as.integer(c(1L, wna[-npoly] + 1L))

        if (compiled) 
        {
            .Call(quilt_tpolygon, vp$context$scene,
                  x, y, npoly, istart, iend, 
                  col2rgb(col, alpha = TRUE), col2rgb(fill, alpha = TRUE),
                  as.integer(lty), as.double(lwd))
        }
        else if (vpar)
        {
            col <- rep(col, length.out = n)
            fill <- rep(fill, length.out = n)
            lwd <- rep(lwd, length.out = n)
            lty <- rep(lty, length.out = n)
            for (i in seq_len(npoly))
            {
                if (iend[i] - istart[i] > 0)
                {
                    pind <- istart[i]:iend[i]
                    vp$context$scene$addPolygon(qpolygon(x[pind], y[pind]),
                                                mypen(col[i], lwd[i], lty[i]),
                                                mybrush(fill[i]))
                }
            }
        }
        else
        {
            p <- mypen(col, lwd, lty)
            b <- mybrush(fill)
            for (i in seq_len(npoly))
            {
                if (iend[i] - istart[i] > 0)
                {
                    pind <- istart[i]:iend[i]
                    vp$context$scene$addPolygon(qpolygon(x[pind], y[pind]), p, b)
                }
            }
        }
    }
    ttext <- function(x, y, labels = seq_along(x),
                      adj = c(0.5, 0.5), cex = 1, col = 1, rot = 0, font = 1, family = "",
                      ..., html = FALSE, vp)
    {
        ## graphics::text(x2pixel(x, vp), y2pixel(y, vp),
        ##                labels = labels, adj = adj, cex = cex, col = col, srt = rot, font = font, family = family)

        n <- maxlength(x, y)
        x <- rep(x2pixel(x, vp), length.out = n)
        y <- rep(y2pixel(y, vp), length.out = n)
        labels <- rep(labels, length.out = n)
        vpar <- maxlength(cex, col, rot) > 1
        adj <- rep(adj, length = 2) # not vectorized
        if (vpar)
        {
            col <- rep(col, length.out = n)
            cex <- rep(cex, length.out = n)
            rot <- rep(rot, length.out = n)
            for (i in which(!anyna(x, y, labels)))
            {
                mytext(vp$context$scene, x[i], y[i], labels[i], adj = adj, rot = rot[i],
                       brush = mybrush(col[i]), font = fontdef(cex = cex[i], font = font, family = family))
            }
        }
        else
        {
            b <- mybrush(col)
            f <- fontdef(cex = cex, font = font, family = family)
            for (i in which(!anyna(x, y, labels)))
            {
                mytext(vp$context$scene, x[i], y[i], labels[i], adj = adj, rot = rot, brush = b, font = f)
            }
        }
    }
    trect <- function(xleft, ybottom, xright, ytop,
                      fill = "transparent",
                      col = "black",
                      lty = 1, lwd = 1, 
                      ..., vp)
    {
        ## graphics::rect(x2pixel(xleft, vp), y2pixel(ybottom, vp),
        ##                x2pixel(xright, vp), y2pixel(ytop, vp),
        ##                col = fill, border = col, lty = lty, lwd = lwd)
        n <- maxlength(xleft, ybottom, xright, ytop)
        vpar <- maxlength(col, fill, lwd, lty) > 1
        xleft <- rep(x2pixel(xleft, vp), length.out = n)
        ybottom <- rep(y2pixel(ybottom, vp), length.out = n)
        xright <- rep(x2pixel(xright, vp), length.out = n)
        ytop <- rep(y2pixel(ytop, vp), length.out = n)
        if (compiled)
        {
            .Call(quilt_trect, vp$context$scene,
                  xleft, ybottom, xright - xleft, ytop - ybottom, 
                  col2rgb(col, alpha = TRUE), col2rgb(fill, alpha = TRUE),
                  as.integer(lty), as.double(lwd))
        }
        else if (vpar)
        {
            col <- rep(col, length.out = n)
            fill <- rep(fill, length.out = n)
            lwd <- rep(lwd, length.out = n)
            lty <- rep(lty, length.out = n)
            for (i in which(!anyna(xleft, ybottom, xright, ytop)))
            {
                vp$context$scene$addRect(qrect(xleft[i], ybottom[i], xright[i], ytop[i]),
                                         mypen(col[i], lwd[i], lty[i]),
                                         mybrush(fill[i]))
            }
        }
        else
        {
            p <- mypen(col, lwd, lty)
            b <- mybrush(fill)
            for (i in which(!anyna(xleft, ybottom, xright, ytop)))
            {
                vp$context$scene$addRect(qrect(xleft[i], ybottom[i], xright[i], ytop[i]), p, b)
            }
        }
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



    tstrheight <- function(s, cex = 1, font = 1, family = "", rot = 0, ..., html = FALSE)
    {
        ## NOTE: QGraphicsTextItem() seems to give bigger height than
        ## QGraphicsSimpleTextItem()
        if (html)
        {
            txtitem <- Qt$QGraphicsTextItem()
            settext_fun <- txtitem$setHtml
        }
        else
        {
            txtitem <- Qt$QGraphicsSimpleTextItem()
            settext_fun <- txtitem$setText
        }
        txtitem$setFont(do.call(qfont, fontdef(cex = cex, font = font, family = family)))
        sapply(s,
               function(x) {
                   settext_fun(x)
                   bb <- txtitem$boundingRect()
                   bbox_rot(bb$width(), bb$height(), rot)[2]
               })
    }
    tstrwidth <- function(s, cex = 1, font = 1, family = "", rot = 0, ..., html = FALSE)
    {
        if (html)
        {
            txtitem <- Qt$QGraphicsTextItem()
            settext_fun <- txtitem$setHtml
        }
        else
        {
            txtitem <- Qt$QGraphicsSimpleTextItem()
            settext_fun <- txtitem$setText
        }
        txtitem$setFont(do.call(qfont, fontdef(cex = cex, font = font, family = family)))
        sapply(s,
               function(x) {
                   settext_fun(x)
                   bb <- txtitem$boundingRect()
                   bbox_rot(bb$width(), bb$height(), rot)[1]
               })
    }

    environment()
}
