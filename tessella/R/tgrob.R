
## We also need a way to find the (minimum) required width/height of
## complex objects such as legends.  For this, we should not assume
## anything more than the ability to compute width/height of text
## strings.

## We will implement this as follows:

## - Have "tgrob" objects that can both 'draw' themselves and report their 'dimensions'.
## - 'Dimensions' means minimum height/width and preferred height/width.
## - Have interface(s) to combine multiple "tgrob"s.

## Basic structure of a "tgrob": list of functions -
## - draw(vp) - user data will be in environment; only vp will be provided
## - minheight, minwidth
## - height, width - means preferred; negative means expandable, proportionally, NA means doesn't care
## - function to combine: takes list-matrix of "tgrob"s, returns "tgrob"


## NOTE: We initially had minheight, height, etc functions, but seems
## overkill.  One potential advantage is that (with suitable rewriting
## of the code) "text" grobs could delay reporting their dimensions,
## and so tgrob-s could be created without the need for primitives to
## be available until actual drawing time.  But this could be done
## anyway later (without changing the API) by writing a suitable
## dollar method.


## We need a NA height/width because we need a rule to combine them (see composite_tgrob)

## This will be much simpler than grid.  In particular, dimensions are
## determined at object creation time, not at drawing time.

tgrob <- function(draw = function(vp) {},
                  minwidth = 0,
                  minheight = 0,
                  width = NA_real_,
                  height = NA_real_,
                  name = "", vp = NULL, ...)
{
    structure(list(draw = draw,
                   minwidth = minwidth, minheight = minheight,
                   width = width, height = height, name = name,
                   env = function() environment(draw)),
              class = "tgrob")
}


print.tgrob <- function(x, ...)
{
    cat(sprintf("'tgrob' object with\n    name: '%s'\n", x$name))
    cat(sprintf("    width: %g (minwidth: %g)\n", x$width, x$minwidth))
    cat(sprintf("    height: %g (minheight: %g)\n", x$height, x$minheight))
    cat(sprintf("Objects in $env():\n"))
    cat(ls(x$env()), fill = TRUE)
    invisible(x)
}


## example:

rectKey <- function(labels, rfill, rwidth = 20, ..., name = "")
{
    n <- length(labels)
    rfill <- rep(rfill, length = n)
    tw <- tstrwidth(labels, ...)
    th <- tstrheight(labels, ...)
    minwidth <- width <- (max(tw) + rwidth)
    minheight <- height <- (sum(th))
    draw <- function(vp)
    {
        ## make (full) viewport with (pixel) limits
        pvp <- tviewport(vp, xlim = c(0, vp$w), ylim = c(0, vp$h))
        ## trect(pvp$xlim[1], pvp$ylim[1], pvp$xlim[2], pvp$ylim[2], vp = pvp, col = "red")
        ## FIXME: expand or exact? If latter, justification
        ## myvp <- tviewport(vp, xlim = c(0, vp$w), ylim = c(0, vp$h))
        ## For now, exact, justified at bottom(top?)-left
        myvp <- tviewport(pvp,
                          x = 0, y = 0, w = minwidth, h = minheight,
                          xlim = c(0, minwidth),
                          ylim = c(0, minheight))
        ## trect(myvp$xlim[1], myvp$ylim[1], myvp$xlim[2], myvp$ylim[2], vp = myvp, col = "green")
        at <- myvp$ylim[2] * seq(from = 0.5 / n, by = 1/n, length.out = n)
        ttext(tw/2, at, labels = labels, ..., vp = myvp)
        trect(xleft = max(tw), ybottom = at - at[1],
              xright = max(tw) + rwidth, ytop = at + at[1],
              fill = rfill, vp = myvp)
    }
    tgrob(draw = draw, minwidth = minwidth, minheight = minheight,
          width = width, height = height, name = name)
}


## The tool that makes it possible to combine multiple tgrob-s into a
## composite one.  Input 'x' is a list with dim() giving layout.  No
## further arguments are needed.

## The composite tgrob needs a draw() function, which will basically
## create a suitable tlayout inside vp and draw components.  For that
## we need layout widths and heights.  This is a 2-step process.
## First we need to combine to width/height per column/row, and then
## use them to define the layout.  When combining, we have to decide
## how.  If all (ignoring NA) are positive/negative, no problem, take
## max (of absolute).  If mixed, ignore negatives.  Note that this
## means even one 0 (and all else -1) will result in 0.

## FIXME: think this through later.

max_lengths <- function(x)
{
    x <- x[!is.na(x)]
    if (length(x) == 0) return (NA_real_)
    i <- x < 0
    if (all(i)) min(x)
    else max(x)
}


## The other part is to combine width/heights to obtain one for the
## composite tgrob.  Here also, summing should suffice if all positive
## or all negative. (Even when aspect is important?  Not sure.  FIXME.)

## However, things are not that simple when mixed.  Even one negative
## should mean result negative.  But the reasonable thing to do when
## respect.aspect=TRUE is not at all clear.

## For now, we sum negatives if any, otherwise sum all (i.e., positives).



sum_lengths <- function(x)
{
    x <- x[!is.na(x)]
    if (length(x) == 0) return (NA_real_)
    i <- x < 0
    if (any(i)) sum(x[i])
    else sum(x)
}



composite_tgrob <- function(x, d = dim(x), adj = c(0.5, 0.5), name = "")
{
    ## Sugar: if x[i,j] == NULL, make them into tgrob().  Are there more efficient options? FIXME.
    x[sapply(x, is.null)] <- list(tgrob())
    ## if more space available that necessary, 'adj' tells how to justify.
    ## where should (i, j) entry go? table-like or graph-like? let's say table-like
    stopifnot(length(d) == 2)
    if (is.null(dim(x))) dim(x) <- d
    adj <- rep(adj, length = 2)
    get_comp <- function(s)
    {
        structure(sapply(x, function(obj) obj[[s]]), dim = d)
    }
    minw <- get_comp("minwidth")
    minh <- get_comp("minheight")
    w <- get_comp("width")
    h <- get_comp("height")
    marginal.minw <- apply(minw, 2, max_lengths)
    marginal.minh <- apply(minh, 1, max_lengths)
    marginal.w <- apply(w, 2, max_lengths)
    marginal.h <- apply(h, 1, max_lengths)
    marginal.w[is.na(marginal.w)] <- 0
    marginal.h[is.na(marginal.h)] <- 0
    new.minw <- sum_lengths(marginal.minw)
    new.minh <- sum_lengths(marginal.minh)
    new.w <- sum_lengths(marginal.w)
    new.h <- sum_lengths(marginal.h)
    draw <- function(vp)
    {
        fvp <- tviewport(vp, xlim = c(0, vp$w), ylim = c(0, vp$h))
        ## we have some leeway if (new.w > 0 && new.w < fvp$xlim[2]), otherwise snug fit whatever 'adj' is.
        svp.x <- if (new.w > 0) (adj[1] * (vp$w - new.w)) else 0
        svp.w <- if (new.w > 0) new.w else vp$w
        svp.y <- if (new.h > 0) (adj[2] * (vp$h - new.h)) else 0
        svp.h <- if (new.h > 0) new.h else vp$h
        svp <- tviewport(fvp, x = svp.x, y = svp.y, w = svp.w, h = svp.h)
        mlayout <- 
            tlayout(widths = marginal.w,
                    heights = marginal.h,
                    parent = svp)
        for (i in seq_len(d[1]))
            for (j in seq_len(d[2]))
            {
                ## cat("composite_tgrob->draw: ", i, ",", j, "\n")
                if (marginal.w[j] != 0 && marginal.h[i] != 0)
                {
                    tvp <- tviewport(mlayout, columns = j, rows = i)
                    x[i,j][[1]]$draw(tvp)
                }
            }
    }
    tgrob(draw = draw,
          minwidth = new.minw,
          minheight = new.minh,
          width = new.w,
          height = new.h,
          name = name)
}

