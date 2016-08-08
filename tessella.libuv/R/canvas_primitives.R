
canvas_primitives <- function(app, guessTextDims = TRUE)
{
    force(app)
    force(guessTextDims)
    cwidth <- 800
    cheight <- 800

    updateSize <- function()
    {
        ## figure out how to do this by querying canvas(es)
        cwidth <<- 800
        cheight <<- 800
    }
    
    ## sends a javascript command
    ssend <- function(s, ...) app$wsock$send(sprintf(s, ...))

    ## sends a command and waits for response.
    ## Not sure about the event loops that make this work
    ssendAndGetResponse <- function(s, ...)
    {
        app$lastMessage <- NULL
        app$wsock$send(sprintf(s, ...)) # use httpuv::service() instead?
        while (is.null(app$lastMessage)) { Sys.sleep(0.0001) }
        ans <- try(eval(parse(text = app$lastMessage)), silent = FALSE)
        if (inherits(ans, "try-error")) stop("Failed to evaluate: ", app$lastMessage)
        ans
    }

    vector2json <- function(x)
    {
        x[is.na(x)] <- NaN # or drop NA-s elsewhere?
        sprintf("[%s]", paste(as.character(x), collapse=","))
    }

    color2json <- function(col)
    {
        ## CSS colors: #AABBCC is OK, but #AABBCC88 is not, need rgba(r,g,b,0.5)
        rgba <- col2rgb(col, alpha = TRUE)
        if (rgba[4] == 255L)
            sprintf("#%s", toupper(paste(as.hexmode(rgba[1:3]), collapse="")))
        else
            sprintf("rgba(%d,%d,%d,%g)", rgba[1], rgba[2], rgba[3], round(rgba[4]/255, 2))
    }

    tget_context <- function()
    {
        c <- tcontext(0, 0, cwidth, cheight, invert.y = TRUE)
        c
    }

    tinitialize <- function(context, newpage = TRUE)
    {
        if (newpage) ssend("newpage();")
    }
    tfinalize <- function()
    {
        ssend("update();")
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

    ## Only points (i.e., no other type)
    tpoints <- function(x, y,
                        col = 1, fill = "transparent", cex = 1,
                        ..., vp)
    {
        if (missing(vp)) stop("'vp' is missing")
        ssend("setPar('stroke', '%s');", color2json(col))
        ssend("setPar('fill', '%s');", color2json(fill))
        ssend("points(%s, %s, %s);",
              vector2json(x2pixel(x, vp)),
              vector2json(y2pixel(y, vp)),
              vector2json(cex))
    }
    tlines <- function(x, y,
                       col = 1, # FIXME add lwd
                       ..., vp)
    {
        if (missing(vp)) stop("'vp' is missing")
        ssend("setPar('stroke', '%s');", color2json(col))
        ssend("lines(%s, %s);", vector2json(x2pixel(x, vp)), vector2json(y2pixel(y, vp)))
    }
    tsegments <- function(x0, y0, x1 = x0, y1 = y0,
                          col = 1, # FIXME add lwd
                          ..., vp)
    {
        if (missing(vp)) stop("'vp' is missing")
        n <- max(length(x0), length(y0), length(x1), length(y1))
        x0 <- rep(x0, length.out = n)
        y0 <- rep(y0, length.out = n)
        x1 <- rep(x1, length.out = n)
        y1 <- rep(y1, length.out = n)
        ssend("setPar('stroke', '%s');", color2json(col))
        ssend("segments(%s,%s,%s,%s);",
              vector2json(x2pixel(x0, vp)), vector2json(y2pixel(y0, vp)),
              vector2json(x2pixel(x1, vp)), vector2json(y2pixel(y1, vp)))
    }
    tpolygon <- function(x, y,
                         col = "black", fill = "transparent", lty = 1, lwd = 1,
                         fillOddEven = FALSE, ..., vp)
    {
        if (missing(vp)) stop("'vp' is missing")
        ssend("setPar('stroke', '%s');", color2json(col))
        ssend("setPar('fill', '%s');", color2json(fill))
        ssend("polygon(%s, %s);", vector2json(x2pixel(x, vp)), vector2json(y2pixel(y, vp)))
    }

    tstrdim <- function(s, cex = 1, family = "sans-serif")
    {
        ssend("setPar('family', '%s');", family)
        ssend("setPar('cex', %g);", cex)
        ## rest probably not important, but used in setFont()
        ssend("setPar('fill', '%s');", color2json(1))
        if (guessTextDims)
            c(cex*8*nchar(s), cex*10)
        else
            ssendAndGetResponse("textdims('%s');", s)
    }

    ttext <- function(x, y, labels = seq_along(x),
                      adj = NULL, pos = NULL, offset = 0.5,
                      cex = 1, col = 1, rot = 0, family = "",
                      ..., vp)
    {
        if (missing(vp)) stop("'vp' is missing")
        n <- max(length(x), length(y))
        if (length(x) < n) x <- rep(x, length.out = n)
        if (length(y) < n) y <- rep(y, length.out = n)
        if (length(labels) < n) labels <- rep(labels, length.out = n)
        xp <- x2pixel(x, vp)
        yp <- y2pixel(y, vp)
        off <- offset * 10 # hardcoded 10 pixel for offset=1
        if (!is.null(pos)) {
            switch(pos, {
                yp <- yp - off
                adj <- c(0.5, 1)
            }, {
                xp <- xp - off
                adj <- c(1, 0.5)
            }, {
                yp <- yp + off
                adj <- c(0.5, 0)
            }, {
                xp <- xp + off
                adj <- c(0, 0.5)
            }, stop("Invalid value of 'pos'"))
        }
        else
            adj <-
                if (is.null(adj)) c(0.5, 0.5)
                else rep(as.numeric(adj), length.out = 2)
        ssend("setPar('family', '%s');", family)
        ssend("setPar('cex', %g);", cex)
        ssend("setPar('fill', '%s');", color2json(col))
        for (i in seq_along(labels))
        {
            tdim <- tstrdim(labels[i])
            ## FIXME: this is crude; need to work harder to get rotated text right
            ## 
            ## calculate bottom-left corner (= xp[i], yp[i] if adj=0)
            lx <- xp[i] - tdim[1] * adj[1]
            by <- yp[i] + tdim[2] * adj[2]
            ty <- by - tdim[2]
            ## we use center-aligned in cancas
            ## cat(sprintf("text(%g, %g, '%s', %g);\n",
            ##             lx + 0.5 * tdim[1], ty + 0.5 * tdim[2], labels[i], rot))
            ssend("text(%g, %g, '%s', %g);",
                  lx + 0.5 * tdim[1], ty + 0.5 * tdim[2], labels[i], rot)
        }
    }
    trect <- function(xleft, ybottom, xright, ytop,
                      fill = "transparent",
                      col = "black",
                      lty = 1, lwd = 1, 
                      ..., vp)
    {
        ## str(list(xleft, ybottom, xright, ytop))
        if (missing(vp)) stop("'vp' is missing")
        ssend("setPar('stroke', '%s');", color2json(col))
        ssend("setPar('fill', '%s');", color2json(fill))
        ssend("rect(%s, %s, %s, %s);",
              vector2json(x2pixel(xleft, vp)), vector2json(y2pixel(ybottom, vp)),
              vector2json(x2pixel(xright, vp)), vector2json(y2pixel(ytop, vp)))
    }
    tclip <- function(vp) 
    {
        ssend("clip(%g, %g, %g, %g);", vp$x, vp$y, vp$x + vp$w, vp$y + vp$h)
    }
    tunclip <- function(vp) 
    {
        ssend("unclip();")
    }
    tstrheight <- function(s, cex = 1, font = 1, family = "sans-serif", rot = 0, ...)
    {
        n <- length(s)
        rot <- rep(rot, length.out = n)
        sapply(seq_len(n), function(k) {
            tdim <- tstrdim(s[k], cex = cex, family = family)
            w <- tdim[1]
            h <- tdim[2]
            if (rot[k] == 0 || rot[k] == 180) return(h)
            if (rot[k] == 90 || rot[k] == 270) return(w)
            bbox_rot(w, h, rot[k])[2]
        })
    }
    tstrwidth <- function(s, cex = 1, font = 1, family = "sans-serif", rot = 0, ...)
    {
        n <- length(s)
        rot <- rep(rot, length.out = n)
        sapply(seq_len(n), function(k) {
            tdim <- tstrdim(s[k], cex = cex, family = family)
            w <- tdim[1]
            h <- tdim[2]
            if (rot[k] == 0 || rot[k] == 180) return(w)
            if (rot[k] == 90 || rot[k] == 270) return(h)
            bbox_rot(w, h, rot[k])[1]
        })
    }

    environment()
}
