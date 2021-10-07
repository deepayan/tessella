


show.settings <- function(x = NULL)
{
    old.settings <- trellis.par.get()
    on.exit(trellis.par.set(old.settings))

    if (!is.null(x)) trellis.par.set(x)
    theme <- trellis.par.get()

    n.row <- 13
    n.col <- 9
    heights.x <- rep(1, n.row)
    heights.units <- rep("lines", n.row)
    heights.units[c(2, 5, 8, 11)] <- "null"
    widths.x <- rep(1, n.row)
    widths.units <- rep("lines", n.row)
    widths.units[c(2, 4, 6, 8)] <- "null"
    page.layout <-
        grid.layout(nrow = n.row, ncol = n.col,
                    widths = unit(widths.x, widths.units),
                    heights = unit(heights.x, heights.units))
    if (!lattice.getStatus("print.more")) grid.newpage()
    lattice.setStatus(print.more = FALSE)
    grid.rect(gp = gpar(fill = theme$background$col,
              col = "transparent"))
    pushViewport(viewport(layout = page.layout,
                          gp = gpar(fontsize = theme$fontsize$text)))
    gp.box <-
        gpar(col = theme$axis.line$col,
             lty = theme$axis.line$lty,
             lwd = theme$axis.line$lwd,
             alpha = theme$axis.line$alpha,
             fill = "transparent")

    ## superpose.symbol
    superpose.symbol <- theme$superpose.symbol
    len <- max(2, sapply(superpose.symbol, length))
    pushViewport(viewport(layout.pos.row = 2,
                          layout.pos.col = 2,
                          yscale = c(0,len+1),
                          xscale = c(0,len+1)))
    panel.superpose(x = rep(1:len, len),
                    y = rep(1:len, each = len),
                    groups = gl(len, len),
                    subscripts = 1:(len*len))
    popViewport()
    grid.text(label = "superpose.symbol",
              vp = viewport(layout.pos.row = 3, layout.pos.col = 2))


    ## superpose.line
    superpose.line <- theme$superpose.line
    len <- max(2, sapply(superpose.line, length))
    pushViewport(viewport(layout.pos.row = 2,
                          layout.pos.col = 4,
                          yscale = c(0,len+1),
                          xscale = c(0, 1)))
    panel.superpose(x = rep(c(0,1), len),
                    y = rep(1:len, each = 2),
                    groups = gl(len, 2),
                    subscripts = 1:(2*len),
                    type = "l")
    popViewport()
    grid.text(label = "superpose.line",
              vp = viewport(layout.pos.row = 3, layout.pos.col = 4))

    ## strip.background
    strip.background <- theme$strip.background
    strip.border <- theme$strip.border
    len <-
        max(sapply(strip.background, length),
            sapply(strip.border, length))
    pushViewport(viewport(layout.pos.row = 2,
                          layout.pos.col = 6,
                          yscale = c(0, len+1),
                          xscale = c(0, 1)))
    grid.rect(y = unit(1:len, "native"),
              height = unit(0.5, "native"),
              gp =
              gpar(fill = strip.background$col,
                   alpha = strip.background$alpha,
                   col = strip.border$col,
                   lty = strip.border$lty,
                   lwd = strip.border$lwd))
    popViewport()
    grid.text(label = "strip.background",
              vp = viewport(layout.pos.row = 3, layout.pos.col = 6))

    ## strip.shingle
    strip.shingle <- theme$strip.shingle
    len <- max(sapply(strip.shingle, length))
    pushViewport(viewport(layout.pos.row = 2,
                          layout.pos.col = 8,
                          yscale = c(0,len+1),
                          xscale = c(0,1)))
    grid.rect(y = unit(1:len, "native"),
              height = unit(0.5, "native"),
              gp =
              gpar(fill = strip.shingle$col,
                   alpha = strip.shingle$alpha,
                   col = "transparent", lwd = 0.0001))
    popViewport()
    grid.text(label = "strip.shingle",
              vp = viewport(layout.pos.row = 3, layout.pos.col = 8))

    ## dot.[symbol, line]
    pushViewport(viewport(layout.pos.row = 5,
                          layout.pos.col = 2,
                          yscale = extend.limits(c(0,6)),
                          xscale = c(0,6)))
    panel.dotplot(x = 1:5, y = 1:5)
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "dot.[symbol, line]",
              vp = viewport(layout.pos.row = 6, layout.pos.col = 2))

    ## box.[dot, rectangle, umbrella]
    pushViewport(viewport(layout.pos.row = 5,
                          layout.pos.col = 4,
                          yscale = c(-2, 2),
                          xscale = c(0,6)))
    panel.bwplot(x = 1:5, y = rep(0, 5))
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "box.[dot, rectangle, umbrella]",
              vp = viewport(layout.pos.row = 6, layout.pos.col = 4))

    ## add.[line, text]
    add.text <- theme$add.text
    add.line <- theme$add.line
    pushViewport(viewport(layout.pos.row = 5,
                          layout.pos.col = 6,
                          yscale = c(-1,1),
                          xscale = c(0,1)))
    x <- seq(.1, .9, length.out = 50)
    y <- .9 * sin(.1+11*x)
    llines(x = x, y = y, type = "l", col = add.line$col,
           lty = add.line$lty, lwd = add.line$lwd)
    ltext(labels = c("Hello", "World"),
          x = c(.25, .75), y = c(-.5, .5))
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "add.[line, text]",
              vp = viewport(layout.pos.row = 6, layout.pos.col = 6))

    ## reference.line
    pushViewport(viewport(layout.pos.row = 5,
                          layout.pos.col = 8,
                          yscale = c(0,4),
                          xscale = c(0,4)))
    panel.grid()
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "reference.line",
              vp = viewport(layout.pos.row = 6, layout.pos.col = 8))

    ## plot.[symbol, line]
    plot.symbol <- theme$plot.symbol
    plot.line <- theme$plot.line
    pushViewport(viewport(layout.pos.row = 8,
                          layout.pos.col = 2,
                          yscale = c(-1.1,1.1),
                          xscale = c(-.1,1.1)))
    x <- seq(.1, .9, length.out = 20)
    y <- .9 * sin(.1+11*x)
    panel.xyplot(x = x+.05, y = y+.1, type = "l")
    panel.xyplot(x = x-.05, y = y-.1)
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "plot.[symbol, line]",
              vp = viewport(layout.pos.row = 9, layout.pos.col = 2))

    ## plot.shingle[plot.polygon]
    plot.polygon <- theme$plot.polygon
    pushViewport(viewport(layout.pos.row = 8,
                          layout.pos.col = 4,
                          yscale = extend.limits(c(0,6)),
                          xscale = extend.limits(c(1,10))))
    grid.rect(x = c(3.5, 4.5, 5.5, 6.5, 7.5), width = rep(5,5),
              y = c(1,2,3,4,5), height = rep(.5, ,5),
              default.units = "native",
              gp =
              gpar(fill = plot.polygon$col,
                   col = plot.polygon$border,
                   alpha = plot.polygon$alpha,
                   lty = plot.polygon$lty,
                   lwd = plot.polygon$lwd))
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "plot.shingle[plot.polygon]",
              vp = viewport(layout.pos.row = 9, layout.pos.col = 4))

    ## histogram[plot.polygon]
    pushViewport(viewport(layout.pos.row = 8,
                          layout.pos.col = 6,
                          yscale = extend.limits(c(0,7)),
                          xscale = extend.limits(c(0.5,7.5))))
    panel.histogram(x = rep(1:7, 1:7), breaks = 0:7 + 0.5, type = "count")
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "histogram[plot.polygon]",
              vp = viewport(layout.pos.row = 9, layout.pos.col = 6))

    ## barchart[plot.polygon]
    pushViewport(viewport(layout.pos.row = 8,
                          layout.pos.col = 8,
                          yscale = extend.limits(c(0.5,6.5)),
                          xscale = c(-1,7)))
    panel.barchart(x = 6:1, y = 1:6)
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "barchart[plot.polygon]",
              vp = viewport(layout.pos.row = 9, layout.pos.col = 8))


    ## superpose.polygon
    superpose.polygon <- trellis.par.get("superpose.polygon")
    len <- max(2, sapply(superpose.polygon, length))
    pushViewport(viewport(layout.pos.row = 11,
                          layout.pos.col = 2,
                          yscale = extend.limits(c(-.45, .45)),
                          xscale = c(-1, len+1)))
    panel.barchart(x = len:1, y = rep(0, len),
                   groups = gl(len, 1),
                   subscripts = 1:len,
                   stack = FALSE)
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "superpose.polygon",
              vp = viewport(layout.pos.row = 12, layout.pos.col = 2))

    ## regions
    regions <- theme$regions
    len <- length(regions$col)
    pushViewport(viewport(layout.pos.row = 11,
                          layout.pos.col = 4,
                          xscale = c(0,len+1)))
    grid.rect(x = 1:len, width = 1,
              default.units = "native",
              gp =
              gpar(col = "transparent",
                   fill = regions$col,
                   alpha = regions$alpha))
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "regions",
              vp = viewport(layout.pos.row = 12, layout.pos.col = 4))
    invisible()
}

