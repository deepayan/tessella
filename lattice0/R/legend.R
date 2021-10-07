

### Copyright (C) 2001-2006  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
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




construct.legend <-
    function(legend = NULL, key = NULL, fun = "draw.key")
{
    if (is.null(legend) && is.null(key)) return(NULL)
    if (is.null(legend)) legend <- list()
    if (!is.null(key))
    {
        space <- key$space
        x <- y <- corner <- NULL

        if (is.null(space))
        {
            if (any(c("x", "y", "corner") %in% names(key)))
            {
                stopifnot(is.null(x) || (length(x) == 1 && x >= 0 && x <= 1))
                stopifnot(is.null(y) || (length(y) == 1 && y >= 0 && y <= 1))
                stopifnot(is.null(corner) ||
                          (length(corner) == 2 &&
                           all(corner %in% c(0, 1))))
                space <- "inside"
                x <- key$x
                y <- key$y
                corner <- key$corner
                ## check for valid values
            }
            else
                space <- "top"
        }
        if (space != "inside" && space %in% names(legend))
            stop(gettextf("component '%s' duplicated in key and legend", space))

        key.legend <- list(fun = fun, args = list(key = key))
        key.legend$x <- x
        key.legend$y <- y
        key.legend$corner <- corner

        legend <- c(list(key.legend), legend)
        names(legend)[1] <- space
    }
    legend
}







## A convenience function for deciding whether an ``automatic'' legend
## should be drawn following instructions contained in the 'auto.key'
## argument.

needAutoKey <- function(auto.key, groups = NULL)
{
    ((!is.null(groups) && (isTRUE(auto.key) || is.list(auto.key))) || 
     (is.list(auto.key) && !is.null(auto.key$text)))
    ## old behaviour was:
    ## !is.null(groups) && (is.list(auto.key) || isTRUE(auto.key))
}


## convenience function for auto.key
drawSimpleKey <- function(...)
    draw.key(simpleKey(...))





## convenience function for the most common type of key

simpleKey <-
    function(text, points = TRUE,
             rectangles = FALSE,
             lines = FALSE,
             col = add.text$col,
             cex = add.text$cex,
             alpha = add.text$alpha,
             font = add.text$font,
             fontface = add.text$fontface,
             fontfamily = add.text$fontfamily,
             lineheight = add.text$lineheight,
             ...)
{
    add.text <- trellis.par.get("add.text")
    foo <- seq_along(text)
    ans <-
        list(text = list(lab = text),
             col = col, cex = cex, alpha = alpha,
             font = font,
             fontface = fontface,
             fontfamily = fontfamily,
             ...)
    if (points) ans$points <-
        Rows(trellis.par.get("superpose.symbol"), foo)
    if (rectangles) ans$rectangles <- 
        Rows(trellis.par.get("superpose.polygon"), foo)
    if (lines) ans$lines <-
        updateList(Rows(trellis.par.get("superpose.symbol"), foo), ## for pch
                   Rows(trellis.par.get("superpose.line"), foo))
    ans
}
             


componentName <- function(name, x, y) {
    trellis.grobname(paste(name, x, y, sep="."), type="key")
}


draw.key <- function(key, ...)
{
    if (!is.list(key)) stop("key must be a list")

    max.length <- 0

    ## maximum of the `row-lengths' of the above
    ## components. There is some scope for confusion
    ## here, e.g., if col is specified in key as a
    ## length 6 vector, and then lines=list(lty=1:3),
    ## what should be the length of that lines column ?
    ## If 3, what happens if lines=list() ?
    ## (Strangely enough, S+ accepts lines=list()
    ## if col (etc) is NOT specified outside, but not
    ## if it is)
    
    process.key <-
        function(align = TRUE,
                 title = NULL,
                 rep = TRUE,
                 background = trellis.par.get("background")$col,
                 alpha.background = 1,
                 border = FALSE,
                 transparent = FALSE, 
                 divide = 3,
                 col = "black",
                 alpha = 1,
                 lty = 1,
                 lwd = 1,
                 font = 1, 
                 pch = 8,
                 fill = "transparent",
                 adj = 0,
                 type = "l", 
                 size = 5,
                 height = 1,
                 angle = 0, 
                 density = -1,
                 reverse.rows = FALSE, ## invert rows (e.g. for barchart, stack = FALSE)
                 between = 2,
                 between.columns = 3,
                 columns = 1,
                 cex = 1,
                 cex.title = 1.5 * max(cex),                 
                 padding.text = 1,
                 lineheight = 1,
                 fontface = NULL, 
                 fontfamily = NULL,
                 
                 ## to avoid partial matching, anything starting with
                 ## lines, points, rect, text must come after ...

                 ...,
                 lines.title = 2)
        {
            list(reverse.rows = reverse.rows,
                 between = between,
                 align = align,
                 title = title,
                 rep = rep,
                 background = background,
                 alpha.background = alpha.background,
                 border = border,
                 transparent = transparent, 
                 columns = columns,
                 divide = divide,
                 between.columns = between.columns,
                 cex = cex,
                 cex.title = cex.title,
                 padding.text = padding.text,
                 col = col,
                 alpha = alpha,
                 lty = lty,
                 lwd = lwd,
                 lineheight = lineheight,
                 font = font,
                 fontface = fontface,
                 fontfamily = fontfamily,
                 pch = pch,
                 adj = adj,
                 type = type, 
                 size = size,
                 height = height,
                 angle = angle, 
                 density = density,
                 lines.title = lines.title,
                 ...)
        }

    fontsize.points <- trellis.par.get("fontsize")$points
    key <- do.call("process.key", key)

    key.length <- length(key)
    key.names <- names(key)    # Need to update
    if (is.logical(key$border)) 
        key$border <-
            if (key$border) "black"
            else "transparent"

    components <- list()

    for(i in 1:key.length)
    {
        curname <- pmatch(key.names[i], c("text", "rectangles", "lines", "points"))

        if (is.na(curname))
        {
            ;## do nothing
        }
        else if (curname == 1) # "text"
        {
            if (!(is.characterOrExpression(key[[i]][[1]])))
                stop("first component of text must be vector of labels")
            pars <-
                list(labels = key[[i]][[1]],
                     col = key$col,
                     alpha = key$alpha,
                     adj = key$adj,
                     cex = key$cex,
                     lineheight = key$lineheight,
                     font = key$font,
                     fontface = key$fontface,
                     fontfamily = key$fontfamily)
            key[[i]][[1]] <- NULL
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- length(pars$labels)
            for (j in 1:length(pars))
                if (is.character(pars))
                    pars[[j]] <- rep(pars[[j]], length.out = tmplen)
            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "text", pars = pars, length = tmplen)
        }
        else if (curname == 2) # "rectangles"
        {
            pars <-
                list(col = key$col,
                     border = "black",
                     alpha = key$alpha,
                     size = key$size,
                     height = key$height,
                     angle = key$angle,
                     density = key$density)
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- max(unlist(lapply(pars,length)))
            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "rectangles", pars = pars, length = tmplen)
        }
        else if (curname == 3) # "lines"
        {
            pars <-
                list(col = key$col,
                     alpha = key$alpha,
                     size = key$size,
                     lty = key$lty,
                     cex = key$cex,
                     pch = key$pch,
                     fill = key$fill,
                     lwd = key$lwd,
                     type = key$type)
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- max(unlist(lapply(pars,length)))
            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "lines", pars = pars, length = tmplen)
        }
        else if (curname == 4) # "points"
        {
            pars <- list(col = key$col,
                         alpha = key$alpha,
                         cex = key$cex,
                         pch = key$pch,
                         lwd = key$lwd,
                         fill = key$fill,
                         font = key$font,
                         fontface = key$fontface,
                         fontfamily = key$fontfamily)
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- max(unlist(lapply(pars,length)))
            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "points", pars = pars, length = tmplen)
        }
    }

    number.of.components <- length(components)
    ## number of components named one of "text",
    ## "lines", "rectangles" or "points"
    if (number.of.components == 0)
        stop("Invalid key, need at least one component named lines, text, rect or points")

    ## The next part makes sure all components have same length,
    ## except text, which should be as long as the number of labels

    ## Update (9/11/2003): but that doesn't always make sense --- Re:
    ## r-help message from Alexander.Herr@csiro.au (though it seems
    ## that's S+ behaviour on Linux at least). Each component should
    ## be allowed to have its own length (that's what the lattice docs
    ## suggest too, don't know why). Anyway, I'm adding a rep = TRUE
    ## argument to the key list, which controls whether each column
    ## will be repeated as necessary to have the same length.

    for (i in seq_len(number.of.components))
    {
        if (key$rep && (components[[i]]$type != "text"))
            components[[i]]$length <- max.length
        components[[i]]$pars <-
            lapply(components[[i]]$pars, rep, length.out = components[[i]]$length)
        if (key$reverse.rows) 
            components[[i]]$pars <- 
                lapply(components[[i]]$pars, rev)
    }
    column.blocks <- key$columns
    rows.per.block <- ceiling(max.length/column.blocks)
    if (column.blocks > max.length) warning("not enough rows for columns")
    key$between <- rep(key$between, length.out = number.of.components)
    
    if (!key$align) stop("Sorry, align=FALSE is not supported")

    ## FIXME more features needed, see comments in uses below
    rectGrob <- function(gp = list()) 
    {
        gp <- modifyList(trellis.par.get("plot.polygon"), gp)
        tgrob(draw = function(vp) panel.fill(fill = gp$col, border = gp$border, lty = gp$lty, lwd = gp$lwd, vp = vp),
              minheight = 5, height = 15,
              minwidth = 10, width = 20)
    }
    textGrob <- function(label, gp = list(), rot = 0, adj = c(0.5, 0.5)) 
    {
        ## str(list(label))
        force(label)
        adj <- rep(adj, length = 2)
        gp <- modifyList(trellis.par.get("add.text"), gp)
        hh <- tstrheight(label, rot = rot, cex = gp$cex, font = gp$font, family = gp$family)
        ww <- tstrwidth(label, rot = rot, cex = gp$cex, font = gp$font, family = gp$family)
        tgrob(draw = function(vp) panel.text(x = (1-adj[1]) * vp$xlim[1] + adj[1] * vp$xlim[2],
                                             y = (1-adj[2]) * vp$ylim[1] + adj[2] * vp$ylim[2],
                                             labels = label, adj = adj,
                                             srt = rot, col = gp$col, cex = gp$cex,
                                             font = gp$font, fontfamily = gp$family, vp = vp),
              minheight = 1.2 * hh, height = 1.2 * hh,
              minwidth = ww, width = ww)
    }
    linesGrob <- function(gp = list()) 
    {
        gp <- modifyList(trellis.par.get("plot.line"), gp)
        tgrob(draw = function(vp) panel.segments(vp$xlim[1], mean(vp$ylim), vp$xlim[2], mean(vp$ylim),
                                                 col = gp$col, lty = gp$lty, lwd = gp$lwd,
                                                 vp = vp),
              minheight = 10, height = 10,
              minwidth = 20, width = 20)
    }
    pointsGrob <- function(gp = list()) 
    {
        gp <- modifyList(trellis.par.get("plot.symbol"), gp)
        tgrob(draw = function(vp) panel.points(mean(vp$xlim), mean(vp$ylim), 
                                               col = gp$col, cex = gp$cex, vp = vp),
              minheight = 10, height = 10,
              minwidth = 20, width = 20)
    }
    nullGrob <- 
        tgrob(minheight = 5, height = 5,
              minwidth = 5, width = 5)
        
    ## Setting up the layout

    ## The problem of allocating space for text (character strings
    ## or expressions) is dealt with as follows: 

    ## Each row and column will take exactly as much space as
    ## necessary. As steps in the construction, a matrix
    ## textMatrix (of same dimensions as the layout) will contain
    ## either 0, meaning that entry is not text, or n > 0, meaning
    ## that entry has the text given by textList[[n]], where
    ## textList is a list consisting of character strings or
    ## expressions.

    n.row <- rows.per.block + 1
    n.col <- column.blocks * (1 + 3 * number.of.components) - 1

    ## key.gf <- vector(mode = "list", length = n.row * n.col)
    
    key.gf <- rep(list(nullGrob), n.row * n.col)
    dim(key.gf) <- c(n.row, n.col)

    if (FALSE)
    {

        if (length(key$title) > 0)
        {
            stopifnot(length(key$title) == 1, is.characterOrExpression(key$title))
            heights.x[1] <- key$lines.title * key$cex.title * tstrheight(key$title)
        }
        else heights.x[1] <- 0

        for (i in 1:column.blocks)
        {
            widths.x[(1:number.of.components-1)*3+1 +
                     (i-1)*3*number.of.components + i-1] <-
                         key$between/2
            widths.x[(1:number.of.components-1)*3+1 +
                     (i-1)*3*number.of.components + i+1] <-
                         key$between/2
        }
        index <- 1
        for (i in 1:number.of.components)
        {
            cur <- components[[i]]
            id <- (1:column.blocks - 1) *
                (number.of.components * 3 + 1) + i * 3 - 1
            if (cur$type == "text")
            {
            }
            else if (cur$type == "rectangles")
            {
                widths.x[id] <- max(cur$pars$size)
            }
            else if (cur$type == "lines")
            {
                widths.x[id] <- max(cur$pars$size)
            }
            else if (cur$type == "points")
            {
                widths.x[id] <- max(cur$pars$cex)
            }
        }


        key.layout <-
            grid.layout(nrow = n.row, ncol = n.col,
                        widths = layout.widths,
                        heights = layout.heights,
                        respect = FALSE,
                        just = if (is.null(key$just)) "center" else key$just)

        if (!key$transparent)
            key.gf <-
                placeGrob(key.gf,
                          rectGrob(gp =
                                   gpar(fill = key$background,
                                        alpha = key$alpha.background,
                                        col = key$border)),
                          row = NULL, col = NULL)
        else
            key.gf <-
                placeGrob(key.gf,
                          rectGrob(gp=gpar(col=key$border)),
                          row = NULL, col = NULL)

        ## Title (FIXME: allow color, font, alpha-transparency here? 
        if (!is.null(key$title))
            key.gf <-
                placeGrob(key.gf, 
                          textGrob(label = key$title,
                                   gp = gpar(cex = key$cex.title,
                                             lineheight = key$lineheight),
                                   name = trellis.grobname("title",
                                     type="key")),
                          row = 1, col = NULL)
    }

    for (i in 1:number.of.components)
    {
        cur <- components[[i]]
        for (j in seq_len(cur$length))
        {
            colblck <- ceiling(j / rows.per.block)
            xx <- (colblck - 1) *
                (number.of.components*3 + 1) + i*3 - 1
            yy <- j %% rows.per.block + 1
            if (yy == 1) yy <- rows.per.block + 1
            componentx <- (colblck - 1)*(number.of.components) + i 
            componenty <- (j - 1) %% rows.per.block + 1
            if (cur$type == "text")
            {
                key.gf[yy, xx][[1]] <- 
                    textGrob(label = cur$pars$labels[j],
                             adj = c(cur$pars$adj[j], 0.5),
                             ## hjust = cur$pars$adj[j],
                             gp =
                             list(col = cur$pars$col[j],
                                  alpha = cur$pars$alpha[j],
                                  lineheight = cur$pars$lineheight[j],
                                  fontfamily = cur$pars$fontfamily[j],
                                  fontface = chooseFace(cur$pars$fontface[j], cur$pars$font[j]),
                                  cex = cur$pars$cex[j]))
            }
            else if (cur$type == "rectangles")
            {
                key.gf[yy, xx][[1]] <-
                    rectGrob(## height = cur$pars$height[j],
                             ## width = cur$pars$size[j] / max(cur$pars$size),
                             gp = list(alpha = cur$pars$alpha[j],
                                       col = cur$pars$col[j],
                                       border = cur$pars$border[j]))
            }
            else if (cur$type == "lines")
            {
                key.gf[yy, xx][[1]] <-
                    linesGrob(## type = type[j],
                              gp = list(col = cur$pars$col[j],
                                        alpha = cur$pars$alpha[j],
                                        lty = cur$pars$lty[j],
                                        lwd = cur$pars$lwd[j]))
            }
            else if (cur$type == "points")
            {
                key.gf[yy, xx][[1]] <-
                    pointsGrob(gp = list(col = cur$pars$col[j],
                                         alpha = cur$pars$alpha[j],
                                         cex = cur$pars$cex[j],
                                         lwd = cur$pars$lwd[j],
                                         fill = cur$pars$fill[j],
                                         fontfamily = cur$pars$fontfamily[j],
                                         fontface = chooseFace(cur$pars$fontface[j], cur$pars$font[j]),
                                         fontsize = fontsize.points,
                                         pch = cur$pars$pch[j]))
            }
        }
    }
    ans <- composite_tgrob(key.gf)
    if (length(key$title) > 0)
    {
        stopifnot(length(key$title) == 1, is.characterOrExpression(key$title))
        composite_tgrob(list(textGrob(key$title, gp = list(cex = key$cex.title, lineheight = key$lineheight)),
                             ans),
                        d = c(2, 1))
    }
    else ans
}



draw.colorkey <- function(key)
{
    if (!is.list(key)) stop("key must be a list")
    ## return(rectKey(labels = month.name, rfill = rainbow(12), rwidth = 20, cex = 0.8))
    
    process.key <-
        function(col = regions$col,
                 alpha = regions$alpha,
                 at,
                 tick.number = 7,
                 width = 2,
                 height = 1,
                 space = "right",
                 raster = FALSE,
                 interpolate = FALSE,
                 ...)
        {
            regions <- trellis.par.get("regions")
            list(col = col,
                 alpha = alpha,
                 at = at,
                 tick.number = tick.number,
                 width = width,
                 height = height,
                 space = space,
                 raster = raster,
                 interpolate = interpolate,
                 ...)
        }

    axis.line <- trellis.par.get("axis.line")
    axis.text <- trellis.par.get("axis.text")

    key <- do.call(process.key, key)

    ## made FALSE later if labels explicitly specified
    check.overlap <- TRUE
    
    ## Note: there are two 'at'-s here, one is key$at, which specifies
    ## the breakpoints of the rectangles, and the other is key$lab$at
    ## (optional) which is the positions of the ticks. We will use the
    ## 'at' variable for the latter, 'atrange' for the range of the
    ## former, and keyat explicitly when needed

    ## Getting the locations/dimensions/centers of the rectangles
    key$at <- sort(key$at) ## should check if ordered
    numcol <- length(key$at)-1
    key$col <-
        level.colors(x = seq_len(numcol) - 0.5,
                     at = seq_len(numcol + 1) - 1,
                     col.regions = key$col,
                     colors = TRUE)

    ## FIXME: need to handle DateTime classes properly

    atrange <- range(key$at, finite = TRUE)
    scat <- as.numeric(key$at) ## problems otherwise with DateTime objects (?)

    if (key$raster && !isTRUE(all.equal(diff(range(diff(scat))), 0)))
        warning("'at' values are not equispaced; output may be wrong")

    ## recnum <- length(scat)-1
    reccentre <- (scat[-1] + scat[-length(scat)]) / 2
    recdim <- diff(scat)

    cex <- axis.text$cex
    col <- axis.text$col
    font <- axis.text$font
    fontfamily <- axis.text$fontfamily
    fontface <- axis.text$fontface
    lineheight <- axis.text$lineheight
    rot <- 0

    if (is.null(key$lab))
    {
        at <- lpretty(atrange, key$tick.number)
        at <- at[at >= atrange[1] & at <= atrange[2]]
        labels <- format(at, trim = TRUE)
    }
    else if (is.characterOrExpression(key$lab) && length(key$lab)==length(key$at))
    {
        check.overlap <- FALSE
        at <- key$at
        labels <- key$lab
    }
    else if (is.list(key$lab))
    {
        at <- if (!is.null(key$lab$at)) key$lab$at else lpretty(atrange, key$tick.number)
        at <- at[at >= atrange[1] & at <= atrange[2]]
        labels <- if (!is.null(key$lab$lab)) {
            check.overlap <- FALSE
            key$lab$lab
        } else format(at, trim = TRUE)
        if (!is.null(key$lab$cex)) cex <- key$lab$cex
        if (!is.null(key$lab$col)) col <- key$lab$col
        if (!is.null(key$lab$font)) font <- key$lab$font
        if (!is.null(key$lab$fontface)) fontface <- key$lab$fontface
        if (!is.null(key$lab$fontfamily)) fontfamily <- key$lab$fontfamily
        if (!is.null(key$lab$lineheight)) lineheight <- key$lab$lineheight
        if (!is.null(key$lab$rot)) rot <- key$lab$rot
        
    }
    else stop("malformed colorkey")

    labscat <- at
    do.labels <- (length(labscat) > 0)
    widpix <- key$width * 12 ## 'width' of rectangle part in 'pixels'

    if (key$space == "right")
    {
        draw <- function(vp)
        {
            ## make (full) viewport with more useful limits
            fvp <- tviewport(vp, xlim = c(0, vp$w), ylim = c(0, 1))
            ## subviewport for rectangles part
            pvp <- tviewport(fvp, x = 0, w = widpix, y = (1 - key$height) / 2, h = key$height, xlim = c(0, widpix), ylim = atrange)
            panel.rect(xleft = rep(0, length(reccentre)), xright = rep(0, length(reccentre)) + widpix, y = reccentre, height = recdim, 
                       col = key$col, border = "transparent", alpha = key$alpha, vp = pvp)
            panel.fill(border = axis.line$col, lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha, fill = "transparent", vp = pvp)
            if (do.labels)
                panel.axis(side = "right", at = labscat, labels = TRUE, draw.labels = TRUE, check.overlap = FALSE, outside = TRUE, ticks = TRUE, rot = rot,
                           ## tck = as.numeric(ticks),
                           text.col = col, text.cex = cex, text.fontfamily = fontfamily, text.fontface = fontface, text.lineheight = lineheight,
                           line.col = axis.line$col, line.lty = axis.line$lty, line.lwd = axis.line$lwd, line.alpha = axis.line$alpha, vp = pvp)
        }
        # FIXME make this axis$tick1 + axis$pad1 etc etc instead of 10
        wid <- widpix + (if (do.labels) (max(tstrwidth(labels, cex = cex, rot = rot, font = fontface, family = fontfamily)) + 10) else 0)
        p <- tgrob(draw = draw, minwidth = wid)
    }
    else if (key$space == "left")
    {
        draw <- function(vp)
        {
            fvp <- tviewport(vp, xlim = c(0, vp$w), ylim = c(0, 1))
            pvp <- tviewport(fvp, x = fvp$xlim[2] - widpix, w = widpix, y = (1 - key$height) / 2, h = key$height, xlim = c(0, widpix), ylim = atrange)
            panel.rect(xleft = rep(0, length(reccentre)), xright = rep(0, length(reccentre)) + widpix, y = reccentre, height = recdim, 
                       col = key$col, border = "transparent", alpha = key$alpha, vp = pvp)
            panel.fill(border = axis.line$col, lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha, fill = "transparent", vp = pvp)
            if (do.labels)
                panel.axis(side = "left", at = labscat, labels = TRUE, draw.labels = TRUE, check.overlap = FALSE, outside = TRUE, ticks = TRUE, rot = rot,
                           ## tck = as.numeric(ticks),
                           text.col = col, text.cex = cex, text.fontfamily = fontfamily, text.fontface = fontface, text.lineheight = lineheight,
                           line.col = axis.line$col, line.lty = axis.line$lty, line.lwd = axis.line$lwd, line.alpha = axis.line$alpha, vp = pvp)
        }
        wid <- widpix + (if (do.labels) (max(tstrwidth(labels, cex = cex, rot = rot, font = fontface, family = fontfamily)) + 10) else 0)
        p <- tgrob(draw = draw, minwidth = wid)
    }
    else if (key$space == "top")
    {
        draw <- function(vp)
        {
            fvp <- tviewport(vp, xlim = c(0, 1), ylim = c(0, vp$h))
            pvp <- tviewport(fvp, x = (1 - key$height) / 2, y = 0, w = key$height, h = widpix, ylim = c(0, widpix), xlim = atrange)
            panel.rect(ybottom = rep(0, length(reccentre)), ytop = rep(0, length(reccentre)) + widpix, x = reccentre, width = recdim, 
                       col = key$col, border = "transparent", alpha = key$alpha, vp = pvp)
            panel.fill(border = axis.line$col, lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha, fill = "transparent", vp = pvp)
            if (do.labels)
                panel.axis(side = "top", at = labscat, labels = TRUE, draw.labels = TRUE, check.overlap = FALSE, outside = TRUE, ticks = TRUE, rot = rot,
                           text.col = col, text.cex = cex, text.fontfamily = fontfamily, text.fontface = fontface, text.lineheight = lineheight,
                           line.col = axis.line$col, line.lty = axis.line$lty, line.lwd = axis.line$lwd, line.alpha = axis.line$alpha, vp = pvp)
        }
        wid <- widpix + (if (do.labels) (max(tstrheight(labels, cex = cex, rot = rot, font = fontface, family = fontfamily)) + 10) else 0)
        p <- tgrob(draw = draw, minheight = wid)
    }
    else if (key$space == "bottom")
    {
        draw <- function(vp)
        {
            fvp <- tviewport(vp, xlim = c(0, 1), ylim = c(0, vp$h))
            pvp <- tviewport(fvp, x = (1 - key$height) / 2, y = fvp$ylim[2] - widpix, w = key$height, h = widpix, ylim = c(0, widpix), xlim = atrange)
            panel.rect(ybottom = rep(0, length(reccentre)), ytop = rep(0, length(reccentre)) + widpix, x = reccentre, width = recdim, 
                       col = key$col, border = "transparent", alpha = key$alpha, vp = pvp)
            panel.fill(border = axis.line$col, lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha, fill = "transparent", vp = pvp)
            if (do.labels)
                panel.axis(side = "bottom", at = labscat, labels = TRUE, draw.labels = TRUE, check.overlap = FALSE, outside = TRUE, ticks = TRUE, rot = rot,
                           text.col = col, text.cex = cex, text.fontfamily = fontfamily, text.fontface = fontface, text.lineheight = lineheight,
                           line.col = axis.line$col, line.lty = axis.line$lty, line.lwd = axis.line$lwd, line.alpha = axis.line$alpha, vp = pvp)
        }
        wid <- widpix + (if (do.labels) (max(tstrheight(labels, cex = cex, rot = rot, font = fontface, family = fontfamily)) + 10) else 0)
        p <- tgrob(draw = draw, minheight = wid)
    }
    p
}

