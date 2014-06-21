

## A 'master' panel function for (x,y) data, modeled on
## lattice::lplot.xy (similar to geaphics::plot.xy)

tessella.plot.xy <-
    function(x, y = NULL,
             type = c("p", "l", "o", "b", "c", "s", "S", "h", "H"),
             pch = 1, lty = 1, col = 1, cex = 1, lwd = 1,
             font = 1, family = "",
             col.line = col, col.symbol = col, alpha = 1, fill = NULL,
             origin = 0,
             ..., vp)
{
    xy <- xy.coords(x, y, recycle = TRUE)
    x <- xy$x
    y <- xy$y
    if (length(x) == 0) return()
    acol <- function(col) adjustcolor(col, alpha.f = alpha)

    type <- match.arg(type)
    switch(type,
           p = {
               tpoints(x = x, y = y, 
                       col = acol(col.symbol), cex = cex, lwd = lwd,
                       fill = fill, pch = pch, vp = vp)
           },
           c = ,
           l = {
               tlines(x = x, y = y,
                      lty = lty, col = acol(col.line), lwd = lwd, vp = vp)
           },
           o = ,
           b = {
               tpoints(x = x, y = y, 
                       col = acol(col.symbol), cex = cex, lwd = lwd,
                       fill = fill, pch = pch, vp = vp)
               tlines(x = x, y = y,
                      lty = lty, col = acol(col.line), lwd = lwd, vp = vp)
           },
           s = ,
           S = {
               ord <- seq_along(x) ## sort.list(x)
               if ((n <- length(x)) > 1)
               {
                   xx <- numeric(2*n-1)
                   yy <- numeric(2*n-1)
                   xx[2*1:n-1] <- x[ord]
                   yy[2*1:n-1] <- y[ord]
                   xx[2*1:(n-1)] <- x[ord][if (type=="s") -1 else -n]
                   yy[2*1:(n-1)] <- y[ord][if (type=="s") -n else -1]
                   tlines(x = xx, y = yy,
                          lty = lty, col = acol(col.line), lwd = lwd, vp = vp)
               }
           },
           h = {
               ylim <- vp$ylim
               zero <-
                   if (min(ylim) > origin) min(ylim)
                   else if (max(ylim) < origin) max(ylim)
                   else origin
               tsegments(x, y, x, zero,
                         lty = lty, col = acol(col.line), lwd = lwd, vp = vp)
           },
           H = {
               xlim <- vp$xlim
               zero <-
                   if (min(xlim) > origin) min(xlim)
                   else if (max(xlim) < origin) max(xlim)
                   else origin
               tsegments(x, y, zero, y,
                         lty = lty, col = acol(col.line), lwd = lwd, vp = vp)
           })
    return()
}




tessella.fill <-
    function(fill = "grey", col = "black", lty = 1, lwd = 1, ..., vp)
{
    trect(vp$xlim[1], vp$ylim[1], vp$xlim[2], vp$ylim[2],
          col = col, fill = fill, lty = lty, lwd = lwd, vp = vp)
}


tessella.grid <-
    function(h = 3, v = 3, col = "grey",
             xlim = vp$xlim, ylim = vp$ylim, # explicit limits, for (pretty) dates etc
             ..., vp)
{
    h <- h[1]; v <- v[1]
    if (h == -1) h <- -5
    if (v == -1) v <- -5
    if (h < 0)
        tessella.abline(h = pretty(ylim, n = -h), col = col, ..., vp = vp)
    if (v < 0)
        tessella.abline(v = pretty(xlim, n = -v), col = col, ..., vp = vp)
    if (h > 0)
        tessella.abline(h = do.breaks(ylim, h + 1)[-c(1, h + 2)], col = col, ..., vp = vp)
    if (v > 0)
        tessella.abline(v = do.breaks(xlim, v + 1)[-c(1, v + 2)], col = col, ..., vp = vp)
}


