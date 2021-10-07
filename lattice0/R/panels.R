

### Copyright (C) 2001-2011  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
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

## the following functions don't do much error checking yet


panel.refline <- function(...) panel.abline(..., reference = TRUE)

panel.abline <-
    function(a = NULL, b = 0,
             h = NULL,
             v = NULL,
             reg = NULL,
             coef = NULL,
             col,
             col.line = add.line$col,
             lty = add.line$lty,
             lwd = add.line$lwd,
             alpha = add.line$alpha,
             type, ...,
             reference = FALSE,
             vp = lattice.getStatus("current.viewport"))
{
    add.line <- if (reference) trellis.par.get("reference.line") else trellis.par.get("add.line")
    if (!missing(col) && missing(col.line)) col.line <- col
    cpl <- current.panel.limits(vp)
    ## mostly copied from abline
    if (!is.null(reg))
    {
        if (!is.null(a))
            warning("'a' is overridden by 'reg'")
        a <- reg
    }
    if (is.object(a) || is.list(a))
    {
        p <- length(coefa <- as.vector(coef(a)))
        if (p > 2)
            warning("only using the first two of ", p, "regression coefficients")
        islm <- inherits(a, "lm")
        noInt <- if (islm)
            !as.logical(attr(stats::terms(a), "intercept"))
        else p == 1
        if (noInt) {
            a <- 0
            b <- coefa[1]
        }
        else {
            a <- coefa[1]
            b <- if (p >= 2)
                coefa[2]
            else 0
        }
    }
    if (!is.null(coef))
    {
        if (!is.null(a))
            warning("'a' and 'b' are overridden by 'coef'")
        a <- coef[1]
        b <- coef[2]
    }
    ## draw y = a + bx if appropriate
    if (!is.null(a))
    {
        coeff <- c(a, b)
        xx <- cpl$xlim
        yy <- cpl$ylim

        ## There are six = choose(4, 2) possibilities (plus edge cases):
        ##      I           II           III          IV            V           VI
        ## 4----C----3  4---------3  4---------3  4---------3  4---------3  4---------3
        ## |         |  | /       |  |         |  |  \      |  |     \   |  |         |
        ## |      ---|  |/        |  |         |  |   \     |  |      \  |  |         |
        ## D   ---   B  |         |  |        /|  |    \    |  |       \ |  |         |
        ## |---      |  |         |  |       / |  |     \   |  |        \|  |\        |
        ## |         |  |         |  |      /  |  |      \  |  |         |  | \       |
        ## 1----A----2  1---------2  1---------2  1---------2  1---------2  1---------2

        sign.dist.from.line <- function(x, y)
        {
            as.integer(sign(y - coeff[1] - coeff[2] * x)) ## / sqrt(1 + coeff[2]^2)
        }
        sign.corners <-
            with(cpl,
                 sign.dist.from.line(c(xlim[1], xlim[2], xlim[2], xlim[1]),
                                     c(ylim[1], ylim[1], ylim[2], ylim[2])))
        A <- prod(sign.corners[c(1, 2)]) <= 0
        B <- prod(sign.corners[c(2, 3)]) <= 0
        C <- prod(sign.corners[c(3, 4)]) <= 0
        D <- prod(sign.corners[c(4, 1)]) <= 0

        yfun <- function(x) coeff[1] + coeff[2] * x
        xfun <- function(y) (y - coeff[1]) / coeff[2]
        drawfun <- function(x0, y0, x1, y1, ...)
        {
            tsegments(x0, y0, x1, y1,
                      col = adjustcolorv(col.line, alpha),
                      lty = lty,
                      lwd = lwd,
                      ..., vp = vp)
        }
        if (D && B) # Case I
            drawfun(xx[1], yfun(xx[1]), xx[2], yfun(xx[2]), ...)
        else if (D && C) # Case II
            drawfun(xx[1], yfun(xx[1]), xfun(yy[2]), yy[2], ...)
        else if (A && B) # Case III
            drawfun(xfun(yy[1]), yy[1], xx[2], yfun(xx[2]), ...)
        else if (A && C) # Case IV
            drawfun(xfun(yy[1]), yy[1], xfun(yy[2]), yy[2], ...)
        else if (B && C) # Case  V
            drawfun(xfun(yy[1]), yy[1], xfun(yy[2]), yy[2], ...)
        else if (A && D) # Case VI
            drawfun(xx[1], yfun(xx[1]), xfun(yy[1]), yy[1], ...)
    }
    if (length(h <- as.numeric(h)) > 0)
        tsegments(x0 = cpl$xlim[1], x1 = cpl$xlim[2],
                  y0 = h, y1 = h,
                  col = adjustcolorv(col.line, alpha),
                  lty = lty, lwd = lwd, vp = vp)
    if (length(as.numeric(v)) > 0)
        tsegments(x0 = v, x1 = v,
                  y0 = cpl$ylim[1], y1 = cpl$ylim[2],
                  col = adjustcolorv(col.line, alpha),
                  lty = lty, lwd = lwd, vp = vp)
    invisible()
}



panel.curve <-
    function (expr, from, to, n = 101,
              curve.type = "l",
              col = add.line$col,
              lty = add.line$lty,
              lwd = add.line$lwd,
              type, ## ignored, to avoid type meant for panel.xyplot etc
              ...,
              vp = lattice.getStatus("current.viewport"))
    ## curve has a log option. Unfortunately there is no easy way to
    ## read in the lattice log options (specified via scales) into the
    ## panel function. Maybe some day if grid natively supports log
    ## scales and lattice is redesigned to take advantage of that
{
    add.line <- trellis.par.get("add.line")
    sexpr <- substitute(expr)
    if (is.name(sexpr))
    {
        fcall <- paste(sexpr, "(x)")
        expr <- parse(text = fcall)
    }
    else
    {
        if (!(is.call(sexpr) && match("x", all.vars(sexpr), nomatch = 0)))
            stop("'expr' must be a function or an expression containing 'x'")
        expr <- sexpr
    }
    lims <- current.panel.limits()$xlim
    if (missing(from)) from <- min(lims)
    if (missing(to)) to <- max(lims)
    x <- seq(from, to, length.out = n)
    y <- eval(expr, envir = list(x = x), enclos = parent.frame())
    panel.lines(x, y, type = curve.type, col = col, lty = lty, lwd = lwd, ...,
                vp = vp)
}






panel.rug <-
    function(x = NULL, y = NULL,
             regular = TRUE,
             start = if (regular) 0 else 0.97,
             end = if (regular) 0.03 else 1,
             xstart = cpl$xlim[1] + start * (diff(cpl$xlim)),
             xend = cpl$xlim[1] + end * (diff(cpl$xlim)),
             ystart = cpl$ylim[1] + start * (diff(cpl$ylim)),
             yend = cpl$ylim[1] + end * (diff(cpl$ylim)),
             col = plot.line$col,
             col.line = col,
             lty = plot.line$lty,
             lwd = plot.line$lwd,
             alpha = plot.line$alpha,
             ...,
             vp = lattice.getStatus("current.viewport"))
{
    if (!any(is.finite(x))) x <- NULL
    if (!any(is.finite(y))) y <- NULL
    cpl <- current.panel.limits(vp)
    plot.line <- trellis.par.get("plot.line")
    if (!is.null(x))
    {
        tsegments(x0 = x, x1 = x,
                  y0 = ystart,
                  y1 = yend,
                  col = adjustcolorv(col.line, alpha),
                  lty = lty, lwd = lwd, vp = vp)
    }
    if (!is.null(y))
    {
        tsegments(y0 = y, y1 = y,
                  x0 = xstart, 
                  x1 = xend, 
                  col = adjustcolorv(col.line, alpha),
                  lty = lty, lwd = lwd, vp = vp)
    }
}



panel.fill <-
    function(col = trellis.par.get("background")$col,
             border = "transparent", ..., fill = col, 
             vp = lattice.getStatus("current.viewport"))
{
    tessella.fill(fill = fill, col = border, ..., vp = vp)
}


panel.grid <-
    function(h = 3, v = 3,
             col,
             col.line = reference.line$col,
             lty = reference.line$lty,
             lwd = reference.line$lwd,
             x = NULL, y = NULL, ...,
             vp = lattice.getStatus("current.viewport"))
{
    limits <- current.panel.limits(vp)
    reference.line <- trellis.par.get("reference.line")
    if (!missing(col) && missing(col.line)) col.line <- col
    h <- as.integer(h)
    v <- as.integer(v)
    if (h > 0) panel.abline(h = do.breaks(vp$ylim, h + 1)[-c(1, h + 2)],
                            col = col.line, lty = lty, lwd = lwd, vp = vp)
    if (v > 0) panel.abline(v = do.breaks(vp$xlim, v + 1)[-c(1, v + 2)],
                            col = col.line, lty = lty, lwd = lwd, vp = vp)

    ## Cheating here a bit for h=-1, v=-1. Can't think of any neat way to
    ## get the actual `at' values of the panel


    if (h < 0)
    {
        if (h == -1) n <- 5 else n <- -h
        scale <- limits$ylim
        if (!is.null(y)) {
            ## class() <- "factor" is an error
            if (inherits(y, "factor"))
                y <- as.character(y)
            mostattributes(scale) <- attributes(y)
        }
        at <- pretty(scale, n = n)
        at <- at[at > min(scale) & at < max(scale)]
        panel.abline(h = at, col = col.line, lty = lty, lwd = lwd, vp = vp)
    }
    if (v < 0)
    {
        if (v == -1) n <- 5 else n <- -v
        scale <- limits$xlim
        if (!is.null(x)) {
            ## class() <- "factor" is an error
            if (inherits(x, "factor"))
                x <- as.character(y)
            mostattributes(scale) <- attributes(x)
        }
        at <- pretty(scale, n = n) ## FIXME: use pretty eventually
        at <- at[at > min(scale) & at < max(scale)]
        panel.abline(v = at, col = col.line, lty = lty, lwd = lwd, vp = vp)
    }
}



panel.lmline <- function(x, y, ...)
{
    if (length(x) > 1) panel.abline(lm(as.numeric(y) ~ as.numeric(x)), ...)
}


prepanel.lmline <-
    function(x, y, ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)

    if (length(x) > 1) {
        coeff <- coef(lm(y~x))
        tem <- coeff[1] + coeff[2] * range(x, finite = TRUE)
        list(xlim = range(x, finite = TRUE),
             ylim = range(y, tem, finite = TRUE),
             dx = diff(range(x, finite = TRUE)),
             dy = diff(tem, finite = TRUE))
    }
    else prepanel.null()
}



panel.linejoin <-
panel.average <-
    function(x, y, fun = mean,
             horizontal = TRUE,
             lwd = reference.line$lwd,
             lty = reference.line$lty,
             col,
             col.line = reference.line$col,
             type = "l", ## ignored
             ...)
{
    ## FIXME: pretty sure this can be made more readable using tapply (or aggregate)
    x <- as.numeric(x)
    y <- as.numeric(y)

    reference.line = trellis.par.get("reference.line")
    if (!missing(col))
    {
        if (missing(col.line)) col.line <- col
    }
    if (horizontal)
    {
        vals <- unique(sort(y))
        yy <- seq_along(vals)
        xx <- numeric(length(yy))
        for (i in yy)
            xx[i] <- fun(x[y == vals[i]])
        panel.lines(xx, vals[yy], col = col.line, lty = lty, lwd = lwd, ...)
    }
    else
    {
        vals <- unique(sort(x))
        xx <- seq_along(vals)
        yy <- numeric(length(xx))
        for (i in xx)
            yy[i] <- fun(y[x == vals[i]])
        panel.lines(vals[xx], yy, col = col.line, lty = lty, lwd = lwd, ...)
     }
}


panel.mathdensity <-
    function(dmath = dnorm,
             args = list(mean = 0, sd = 1),
             n = 50,
             col,
             col.line = reference.line$col,
             lwd = reference.line$lwd,
             lty = reference.line$lty,
             type,
             ...)
{
    reference.line <- trellis.par.get("reference.line")
    if (!missing(col) && missing(col.line)) col.line <- col
    x <- do.breaks(endpoints = current.panel.limits()$xlim, nint = n)
    y <- do.call("dmath", c(list(x = x), args))
    panel.lines(x = x, y = y, col = col.line, lwd = lwd, lty = lty, ...)
}

