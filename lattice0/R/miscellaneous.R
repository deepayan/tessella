
### Copyright (C) 2001-2009 Deepayan Sarkar <Deepayan.Sarkar@R-project.org> 
###
### This file is part of the lattice package for R.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA



## For historical reasons (i.e., S-compatibility) there are several
## places in lattice where an argument that is supposed to be a
## function may be specified as a character string.  It is not
## entirely clear how namespace ambiguity is resolved, but it appears
## that bindings in the lattice namespace are preferred over the
## global environment.


getFunctionOrName <- function(FUN)
     ## Try lattice namespace first? Does that happens automatically?
{
    if (is.function(FUN)) FUN
    else if (is.character(FUN)) get(FUN)
    else eval(FUN)
}


checkArgsAndCall <- function(FUN, args) ## unnamed arguments not allowed
{
    if (!("..." %in% names(formals(FUN))))
        args <- args[intersect(names(args), names(formals(FUN)))]
    do.call(FUN, args)
}


    

logLimits <- function(lim, base)
{
    if (is.list(lim))
        lapply(lim, log, base)
    else log(lim, base)
}


chooseFace <- function(fontface = NULL, font = 1)
{
    if (is.null(fontface)) font else fontface
}


lpretty <- function(x, ...)
{
    eps <- 1e-10
    at <- pretty(x[is.finite(x)], ...)
    ifelse(abs(at-round(at, 3))<eps, round(at, 3), at)
}


oneway <-
    function(formula, data, location = mean,
             spread = function(x) sqrt(var(x)))
{
    if(missing(data)) data <- sys.frame(sys.parent())
    form <- latticeParseFormula(formula, data)
    y <- form$left
    x <- form$right
    if (!is.shingle(x)) x <- as.factor(x)
    is.f.x <- is.factor(x)
    num.l.x <- nlevels(x) 
    foo <- list()
    if (is.f.x) {
        foo$location <-
            if (is.function(location)) as.vector(tapply(X=y, INDEX=list(x), FUN = location))
            else rep(location, num.l.x)
        foo$spread <- 
            if (is.function(spread)) as.vector(tapply(X=y, INDEX=list(x), FUN = spread))
            else rep(spread, num.l.x)
        foo$fitted.values <- numeric(length(y))
        sc <- numeric(length(y))
        for (i in seq_along(y)){
            foo$fitted.values[i] <- foo$location[as.numeric(x)[i]]
            sc[i] <- foo$spread[as.numeric(x)[i]]
        }
        foo$residuals <- y - foo$fitted.values
        foo$scaled.residuals <- foo$residuals/sc
    }
    else stop("x must be (coercible to be) a factor")
    foo
}



is.characterOrExpression <- function(x)
    is.character(x) || is.expression(x)




## This converts character to factor, numeric to shingle, and
## in addition, takes subsets
as.factorOrShingle <- function(x, subset = TRUE, drop = FALSE)
{
    x <-
        if (is.numeric(x))
            as.shingle(x)
        else ##if (is.character(x)) or logical or ??
            as.factor(x)
    x[subset, drop = drop]
}



## this is a generalization of range(x), used as the xlim/ylim
## components of prepanel functions.  It should deals with factors,
## numerics, and date-time classes.
scale.limits <- function(x)
{
    if (is.factor(x)) levels(x)
    else if (is.numeric(x)) range(x, finite = TRUE)
    else range(x, na.rm = TRUE)
}




## update elements of a list recursively. Used in updating trellis or
## lattice settings using trellis.par.set and lattice.options
## respectively

updateList <- function(x, val)
{
    if (is.null(x)) x <- list()
    modifyList(x, val)
}


##     function(x, val)
## {
##     if (is.null(x)) x <- list()
##     if (!is.list(x)) stop("x must be NULL or a list")
##     if (!is.list(val)) stop("val must be a list")
##     xnames <- names(x)
##     for (v in names(val))
##     {
##         existing <- v %in% xnames
##         if (existing && is.list(x[[v]]) && is.list(val[[v]]))
##             x[[v]] <- updateList(x[[v]], val[[v]])
##         else 
##             x[[v]] <- val[[v]]
##     }
##     x
## }


## Next 3 are convenience functions following those available in Trellis

do.breaks  <- function(endpoints, nint)
{
    if (length(endpoints)!=2) stop("error")
    endpoints[1] + diff(endpoints) * 0:nint / nint
}


Rows <- function(x, which)
{
    for (i in seq_along(x)) x[[i]] <-
        rep(x[[i]], length.out = max(which, length(which)))[which]
    x
}





## panel functions corresponding to standard base functions

panel.points <- function(...) lpoints(...)
panel.lines <- function(...) llines(...)
panel.segments <- function(...) lsegments(...)
panel.text <- function(...) ltext(...)
panel.arrows <- function(...) larrows(...)
panel.rect <- function(...) lrect(...)
panel.polygon <- function(...) lpolygon(...)




primName <- function(name, identifier = NULL, name.type = "panel", group = 0) {
    trellis.grobname(name = ifelse(is.null(identifier), name,
                       paste(identifier, name, sep=".")),
                     type = name.type,
                     group = group)
}



## The rest are grid-ified versions of standard base 'incremental
## graphics' functions.  Maybe it's better to push wrappers like
## panel.points, panel.lines, etc.



lpolygon <-
    function(x, y = NULL,
             ## density = NULL,
             ## angle = 45,
             border = "black",
             col = "transparent",
             ## lty = NULL,
             fill = NULL, # capture so that doesn't overload 'fill=col' in gpar()
             alpha = 1,
             font, fontface, ## gpar() doesn't like these
             ...,
             vp = lattice.getStatus("current.viewport")) 
{
    if (sum(!is.na(x)) < 1) return()
    border <- 
        if (all(is.na(border)))
            "transparent"
        else if (is.logical(border))
        {
            if (border) "black"
            else "transparent"
        }
        else border
    xy <- xy.coords(x, y, recycle = TRUE)

    ## new version (uses grid.polygon concept of id)

    with(xy,
     {
         tpolygon(x, y, col = border, fill = adjustcolorv(col, alpha), ..., vp = vp)
         ## n <- length(x)
         ## w <- which(is.na(x) | is.na(y))
         ## id.lengths <- diff(c(0, w, n))
         ## grid.polygon(x = x,
         ##              y = y,
         ##              id.lengths = id.lengths,
         ##              default.units = "native",
         ##              name = primName("polygon", identifier, name.type, group),
         ##              gp =
         ##              gpar(fill = col,
         ##                   col = border,
         ##                   ...))
     })
}




lsegments <-
    function(x0 = NULL, y0 = NULL, x1, y1,
             x2 = NULL, y2 = NULL,
             col = add.line$col,
             alpha = add.line$alpha,
             lty = add.line$lty,
             lwd = add.line$lwd,

             font, fontface, ## gpar() doesn't like these

             ...,
             vp = lattice.getStatus("current.viewport"))
{
    if (missing(x0)) x0 <- x2
    if (missing(y0)) y0 <- y2
    add.line <- trellis.par.get("add.line")
    ml <- max(length(x0), length(x1), length(y0), length(y1))
    x0 <- rep(x0, length.out = ml)
    x1 <- rep(x1, length.out = ml)
    y0 <- rep(y0, length.out = ml)
    y1 <- rep(y1, length.out = ml)
    tsegments(x0 = x0, x1 = x1,
              y0 = y0, y1 = y1,
              lty=lty, col = col, lwd = lwd,
              alpha = alpha, ..., vp = vp)
}


lrect <-
    function(xleft, ybottom, xright, ytop,
             x = (xleft + xright) / 2,
             y = (ybottom + ytop) / 2,
             width = xright - xleft,
             height = ytop - ybottom,
             col = "transparent",
             border = "black",
             lty = 1, lwd = 1, alpha = 1,
             ...,
             vp = lattice.getStatus("current.viewport"))
{
    border <- 
        if (all(is.na(border)))
            "transparent"
        else if (is.logical(border))
        {
            if (border) "black"
            else "transparent"
        }
        else border
    trect(x - width / 2, y - height / 2, x + width / 2, y + height / 2, 
          width = width, height = height,
          fill = adjustcolorv(col, alpha), col = border,
          lty = lty, lwd = lwd, ..., vp = vp)
}




larrows <-
    function(x0 = NULL, y0 = NULL, x1, y1, x2 = NULL, y2 = NULL,
             angle = 30, code = 2, length = 0.25, unit = "inches",
             ends = switch(code, "first", "last", "both"),
             type = "open",
             col = add.line$col,
             alpha = add.line$alpha,
             lty = add.line$lty,
             lwd = add.line$lwd,
             fill = NULL, ...,
             identifier = NULL,
             name.type = "panel")
{
    warning("larrows has not yet been ported to tessella")
    if (missing(x0)) x0 <- x2
    if (missing(y0)) y0 <- y2
    add.line <- trellis.par.get("add.line")
    ml <- max(length(x0), length(x1), length(y0), length(y1))
    x0 <- rep(x0, length.out = ml)
    x1 <- rep(x1, length.out = ml)
    y0 <- rep(y0, length.out = ml)
    y1 <- rep(y1, length.out = ml)
    gp <- gpar(col = col, lty=lty, lwd = lwd, alpha = alpha, fill = fill, ...)
    panel.segments(x0 = x0, x1 = x1,
                   y0 = y0, y1 = y1,
                   col = col, alpha = alpha, lty = lty, lwd = lwd, ...)
                   ## arrow = if (is.null(ends)) NULL else 
                   ## arrow(angle = angle,
                   ##      length = unit(length, unit),
                   ##      ends = ends,
                   ##      type = type))
}


ltext <- function(x, ...) UseMethod("ltext")

ltext.default <-
    function(x, y = NULL, labels = seq_along(x),
             col = add.text$col,
             alpha = add.text$alpha,
             cex = add.text$cex,
             srt = 0,
             lineheight = add.text$lineheight,
             font = add.text$font,
             fontfamily = add.text$fontfamily,
             fontface = add.text$fontface,
             adj = c(.5, .5),
             pos = NULL,
             offset = 0.5,
             ...,
             vp = lattice.getStatus("current.viewport"))
{
    add.text <- trellis.par.get("add.text")
    ttext(x, y, labels = labels,
          adj = adj, pos = pos, offset = offset, cex = cex,
          col = adjustcolorv(col, alpha), rot = srt, font = font, family = fontfamily,
          fontface = fontface, lineheight = lineheight, # FIXME: unused
          ..., vp = vp)
}



llines <- function (x, ...) UseMethod("llines")


llines.default <-
    function(x, y = NULL, type = "l",
             col = plot.line$col,
             alpha = plot.line$alpha,
             lty = plot.line$lty,
             lwd = plot.line$lwd, ...,
             vp = lattice.getStatus("current.viewport"))
{
    plot.line <- trellis.par.get("plot.line")
    tessella.plot.xy(x, y, type = type,
                     col = col, lty = lty, lwd = lwd, alpha = alpha, ..., vp = vp)
}


lpoints <- function (x, ...) UseMethod("lpoints")

lpoints.default <-
    function(x, y = NULL, type = "p",
             col = plot.symbol$col,
             pch = plot.symbol$pch,
             alpha = plot.symbol$alpha,
             fill = plot.symbol$fill,
             font = plot.symbol$font,
             fontfamily = plot.symbol$fontfamily,
             fontface = plot.symbol$fontface,
             cex = plot.symbol$cex, ...,
             vp = lattice.getStatus("current.viewport"))
{
    plot.symbol <- trellis.par.get("plot.symbol")
    tessella.plot.xy(x, y, 
                     type = type,
                     col = col,
                     pch = pch,
                     alpha = alpha,
                     fill = fill,
                     font = font,
                     fontfamily = fontfamily,
                     fontface = fontface,
                     cex = cex,
                     ..., vp = vp)
}






lplot.xy <-
    function(xy,
             type = c("p", "l", "o", "b", "c", "s", "S", "h", "H"),
             ..., vp = lattice.getStatus("current.viewport"))
{
    x <- xy$x
    y <- xy$y
    type <- match.arg(type)
    tessella.plot.xy(x, y, type = type, ..., vp = vp)
}



