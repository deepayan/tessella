

## Proof-of-concept backend built into the package, using
## (traditional) graphics.

graphics_context <- function()
{
    .Deprecated("graphics_primitives")
    grdim <- round(mean(par("cra") / par("cin"))) * par("din")
    tcontext(0, 0, grdim[1], grdim[2])
}

graphics_primitives <- function()
{
    opar <- NULL
    tdpi <- round(mean(par("cra") / par("cin")))[1] 

    tget_context <- function()
    {
        ## grdim <- round(mean(par("cra") / par("cin"))) * par("din")
        grdim <- tdpi * par("din")
        tcontext(0, 0, grdim[1], grdim[2])
    }

    tinitialize <- function(context, newpage = TRUE)
    {
        dev.hold()
        if (newpage) plot.new()
        opar <<- par(mar = c(0, 0, 0, 0),
                     usr = with(context, c(x, x + w, y, y + h)))
    }
    tfinalize <- function()
    {
        dev.flush()
        par(opar)
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
                        pch = 1, col = 1, fill = "transparent", cex = 1,
                        lwd = 1, lty = 1, # for point boundary
                        ..., vp)
    {
        if (missing(vp)) stop("'vp' is missing")
        graphics::points(x2pixel(x, vp), y2pixel(y, vp),
                         type = "p",
                         pch = pch, col = col, bg = fill, cex = cex,
                         lwd = lwd, lty = lty)
    }
    tlines <- function(x, y,
                       lty = 1, lwd = 1, col = 1,
                       ..., vp)
    {
        if (missing(vp)) stop("'vp' is missing")
        ## allow lend, ljoin, lmitre?
        graphics::lines(x2pixel(x, vp), y2pixel(y, vp),
                        type = "l",
                        lty = lty, lwd = lwd, col = col)
    }
    tsegments <- function(x0, y0, x1 = x0, y1 = y0,
                          lty = 1, lwd = 1, col = 1,
                          ..., vp)
    {
        if (missing(vp)) stop("'vp' is missing")
        graphics::segments(x2pixel(x0, vp), y2pixel(y0, vp),
                           x2pixel(x1, vp), y2pixel(y1, vp),
                           lty = lty, lwd = lwd, col = col)
    }
    tpolygon <- function(x, y,
                         col = "black", fill = "transparent", lty = 1, lwd = 1, #lend, lmitre,
                         fillOddEven = FALSE, ..., vp)
    {
        if (missing(vp)) stop("'vp' is missing")
        graphics::polygon(x2pixel(x, vp), y2pixel(y, vp),
                          border = col, col = fill, lty = lty, lwd = lwd, fillOddEven = fillOddEven)
    }
    ttext <- function(x, y, labels = seq_along(x),
                      adj = NULL, pos = NULL, offset = 0.5, cex = 1, col = 1, rot = 0, font = 1, family = "",
                      ..., vp)
    {
        if (missing(vp)) stop("'vp' is missing")
        if (length(x) == 0 || length(labels) == 0) 
        {
            warning("0-length x or labels")
            return()
        }
        xp <- x2pixel(x, vp)
        yp <- y2pixel(y, vp)
        off <- offset * tstrheight("M", cex = cex, font = font, family = family, ...)
        if (!is.null(pos))
        {
            switch(pos,
               { #1
                   yp <- yp - off
                   adj <- c(.5, 1)
               },
               { #2
                   xp <- xp - off
                   adj <- c(1, .5)
               },
               { #3
                   yp <- yp + off
                   adj <- c(.5, 0)
               },
               { #4
                   xp <- xp + off
                   adj <- c(0, .5)
               },
                   stop("Invalid value of 'pos'"))
        }
        graphics::text(xp, yp, 
                       labels = labels, adj = adj, cex = cex, col = col, srt = rot, font = font, family = family)
    }
    trect <- function(xleft, ybottom, xright, ytop,
                      fill = "transparent",
                      col = "black",
                      lty = 1, lwd = 1, 
                      ..., vp)
    {
        if (missing(vp)) stop("'vp' is missing")
        graphics::rect(x2pixel(xleft, vp), y2pixel(ybottom, vp),
                       x2pixel(xright, vp), y2pixel(ytop, vp),
                       col = fill, border = col, lty = lty, lwd = lwd)
    }
    tclip <- function(vp) 
    {
        if (missing(vp)) stop("'vp' is missing")
        graphics::clip(vp$x, vp$x + vp$w, vp$y, vp$y + vp$h)
    }
    tunclip <- function(vp) 
    {
        if (missing(vp)) stop("'vp' is missing")
        graphics::clip(vp$context$x, vp$context$x + vp$context$w, vp$context$y, vp$context$y + vp$context$h)
    }
    tstrheight <- function(s, cex = 1, font = 1, family = "", rot = 0, ...)
    {
        h <- tdpi * graphics::strheight(s, units = "inches", cex = cex, font = font, family = family)
        if (rot == 0 || rot == 180) return(h)
        w <- tdpi * graphics::strwidth(s, units = "inches", cex = cex, font = font, family = family)
        if (rot == 90 || rot == 270) return(w)
        n <- length(h)
        rot <- rep(rot, length = n)
        sapply(seq_len(n), function(k) {
            bbox_rot(w[k], h[k], rot[k])[2]
        })
    }
    tstrwidth <- function(s, cex = 1, font = 1, family = "", rot = 0, ...)
    {
        w <- tdpi * graphics::strwidth(s, units = "inches", cex = cex, font = font, family = family)
        if (rot == 0 || rot == 180) return(w)
        h <- tdpi * graphics::strheight(s, units = "inches", cex = cex, font = font, family = family)
        if (rot == 90 || rot == 270) return(h)
        n <- length(h)
        rot <- rep(rot, length = n)
        sapply(seq_len(n), function(k) {
            bbox_rot(w[k], h[k], rot[k])[1]
        })
    }

    environment()
}


