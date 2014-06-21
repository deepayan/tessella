
## Structures for abstracting viewports and layouts. Go S4?

## A "tlayout" has columns and rows, with fixed (pixel) or null
## (expandable) dimensions.  Positive means pixel units, negative
## means null (proportional to absolute value)

tlayout <- function(widths = -1, heights = -1, parent = NULL, respect.aspect = FALSE)
{
    ## parent = NULL means parent viewport is not yet specified.  If
    ## non-NULL, we can convert the null units into pixel values.  We
    ## will still keep the originals so that we can recompute if
    ## (properties of) parent viewport change(s).

    ## If respect.aspect=TRUE, then relative (pixel) dimensions of
    ## negative x and y units should be respected.

    ## FIXME: should we be more parsimonious with refreshLayout()
    ## calls? Currently there may be many redundant calls, but not
    ## sure if there are benefits.  We may keep a flag, and not re-do
    ## if set.
    
    ans <- list(owidths = widths, oheights = heights,
                widths = NULL, heights = NULL,
                respect.aspect = respect.aspect,
                parent = parent)
    class(ans) <- "tlayout"
    ## if (!is.null(parent)) ans <- refreshLayout(ans)
    ans
}


## refreshLayout() computes actual width/height based on available
## space for "null" units, i.e., negative values indicating
## space-filling lengths. NA's (which indicate that the owner doesn't
## care) are taken as 0.

refreshLayout <- function(x)
{
    if (is.null(x$parent))
        x$widths <- x$heights <- NULL
    else
    {
        respect <- x$respect.aspect
        x$widths <- x$owidths
        x$heights <- x$oheights
        x$widths[is.na(x$widths)] <- 0
        x$heights[is.na(x$heights)] <- 0
        wtotal <- x$parent$w
        htotal <- x$parent$h
        ## print(list(wtotal, htotal))
        available.wnull <- wtotal - sum(x$owidths[x$owidths > 0])
        available.hnull <- htotal - sum(x$oheights[x$oheights > 0])
        ## print(available.wnull)
        ## print(available.hnull)
        ## str(x)
        ## print("-------done")
        if (available.wnull < 0 || available.hnull < 0) 
        {
            if (respect) stop("Not enough space available (", available.wnull, ", ", available.hnull, ")",
                              " and respect = TRUE")
            else
                warning("Not enough space available (", available.wnull, ", ", available.hnull, ")")
        }
        wnull <- -x$owidths[x$owidths < 0]
        hnull <- -x$oheights[x$oheights < 0]
        if (respect)
        {
            ## available - available.wnull, available.hnull. Which has extra?
            desired.aspect <- sum(hnull) / sum(wnull)
            ## available.hnull / available.wnull == desired.aspect # ideal
            if (available.hnull / available.wnull < desired.aspect)
            {
                ## more width available than necessary
                available.wnull <- available.hnull / desired.aspect
            }
            else
            {
                ## more height available than necessary 
                available.hnull <- available.wnull * desired.aspect
            }
        }
        x$widths[x$owidths < 0] <-
            available.wnull * prop.table(-x$owidths[x$owidths < 0])
        x$heights[x$oheights < 0] <-
            available.hnull * prop.table(-x$oheights[x$oheights < 0])
    }
    x
}


## All plotting is done in a context, represented as an abstract
## surface defined by bottom-left (or top-left if invert.y = TRUE)
## corner (x, y) and size (w, h) expected to represent raster (pixel)
## coordinates.  Primitives work in this coordinate system.  Layout
## heights/widths are also in terms of this coordinate system.

## The actual context (and primitives) will ultimately have to be
## provided by the rendering backend, but the functions here try to do
## as much of the common work as possible using this abstraction.

tcontext <- function(x, y, w, h, invert.y = FALSE)
{
    structure(list(x = x, y = y, w = w, h = h, invert.y = invert.y),
              class = "tcontext")
}

## A viewport can be either a subviewport of a parent viewport, or of
## a parent layout (which itself must have a parent viewport)

tviewport <- function(parent, ...)
{
    UseMethod("tviewport")
}

str.tviewport <- function(object, max.level = 1, give.attr = FALSE, ...)
{
    str(unclass(object), max.level = max.level, give.attr = give.attr, ...)
}

## The parent argument can be a "tcontext".  The resulting viewport
## fills the whole context by default.

tviewport.tcontext <-
    function(parent,
             x = parent$x, y = parent$y,
             w = parent$w, h = parent$h,
             xlim = c(0, 1), ylim = c(0, 1),
             ...)
{
    ans <-
        list(parent = NULL,
             context = parent,
             x = x, y = y, w = w, h = h,
             xlim = xlim, ylim = ylim)
    class(ans) <- "tviewport"
    ans
}

## The parent argument is usually another "tviewport".  The original
## context has to be recorded.

## [xywh] is supplied in parent's native coordinates, but stored in
## canvas coordinates. (FIXME: is that a good idea? For many layers of
## nesting, rounding errors might combine to produce visible effects.)
## So supplied (x,y) is always bottom-left in parent viewportt
## coordinates, but stored (x,y) is (top-left), (x+w,y+h) is
## (bottom-right) in canvas coordinates (unless invert.y = FALSE,
## when top <-> bottom).

## xlim|ylim are 2-vectors giving (bottom-left) and (top-right)
## corners (Cartesian coordinates)

## default (should) produce subviewport with full extents (useful to
## have a version with different limits)

tviewport.tviewport <-
    function(parent,
             x = parent$xlim[1], y = parent$ylim[1],
             w = diff(parent$xlim), h = diff(parent$ylim),
             xlim = c(0, 1), ylim = c(0, 1),
             ...)
{
    ans <-
        list(parent = parent,
             context = parent$context,
             xlim = xlim, ylim = ylim)
    ## ans$x <- parent$x + parent$w * (x - parent$xlim[1]) / diff(parent$xlim);
    ## ans$y <- parent$y + parent$h * (parent$ylim[2] - (y + h)) / diff(parent$ylim);
    ans$x <- x2pixel(x, parent)
    ans$y <- if (parent$context$invert.y) y2pixel(y + h, parent) else y2pixel(y, parent)
    ans$w <- if (diff(parent$xlim)) parent$w * w / diff(parent$xlim) else 0
    ans$h <- if (diff(parent$ylim)) parent$h * h / diff(parent$ylim) else 0
    ## str(parent, max.level = 1)
    class(ans) <- "tviewport"
    ans
}


## layouts are always assumed to number rows increasing top-to-bottom

tviewport.tlayout <-
    function(parent, columns, rows,
             xlim = c(0, 1), ylim = c(0, 1), ...)
{
    ## For now, let us put the restriction that the parent layout must
    ## have a non-null parent viewport
    if (is.null(parent$parent)) stop("parent layout has no parent")
    parent <- refreshLayout(parent)
    ## parent$widths and parent$heights are now in pixels,
    ## parent$parent$x/y are also in pixels, in context coordinates
    ## (which may have y-axis inverted)
    ans <-
        list(parent = parent$parent,
             context = parent$parent$context,
             playout = parent,
             columns = columns, rows = rows,
             xlim = xlim, ylim = ylim)
    xx <- cumsum(c(0, parent$widths))[range(columns) + c(0, 1)]
    yy <- cumsum(c(0, parent$heights))[range(rows) + c(0, 1)]
    ans$x <- parent$parent$x + xx[1]
    ## ## ans$y <- parent$parent$y + yy[1]
    ## ans$y <- parent$parent$y + parent$parent$h - yy[2]
    ans$y <- 
        if (parent$parent$context$invert.y)
            parent$parent$y + yy[1]
        else
            parent$parent$y + parent$parent$h - yy[2]
    ans$w <- diff(xx)
    ans$h <- diff(yy)
    class(ans) <- "tviewport"
    ans
}


x2pixel <- function(x, vp)
{
    vp$x + vp$w * (x - vp$xlim[1]) / diff(vp$xlim)
}

y2pixel <- function(y, vp)
{
    pixoffset <- vp$h * (y - vp$ylim[1]) / diff(vp$ylim)
    if (vp$context$invert.y) 
        vp$y + vp$h - pixoffset
    else 
        vp$y + pixoffset
    ## if (vp$context$invert.y) vp$context$h - yy
    ## else yy
}

pixel2x <- function(x, vp)
{
    vp$xlim[1] + diff(vp$xlim) * (x - vp$x) / vp$w 
}

pixel2y <- function(y, vp)
{
    if (vp$context$invert.y) 
        vp$ylim[1] + diff(vp$ylim) * (vp$y + vp$h - y) / vp$h
    else
        vp$ylim[1] + diff(vp$ylim) * (y - vp$y) / vp$h
}

