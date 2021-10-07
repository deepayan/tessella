
gpar <- function(...) list(...)

adjustcolorv <- function(col, alpha)
{
    if (length(ua <- unique(alpha)) == 1) adjustcolor(col, ua)
    else
    {
        n <- length(col)
        if (length(alpha) != n) alpha <- rep(alpha, length.out = n)
        mapply(adjustcolor, col, alpha)
    }
}

evaluate.legend <- function(legend)
{
    if (is.null(legend)) return(NULL)
    for (i in seq_along(legend))
    {
        fun <- legend[[i]]$fun
        fun <- getFunctionOrName(fun) ## OK in case fun is a grob?
        if (is.function(fun)) fun <- do.call("fun", legend[[i]]$args)
        legend[[i]]$obj <- fun
        legend[[i]]$args <- NULL
        legend[[i]]$fun <- NULL
    }
    legend
}

plot.trellis <- function(x, ...)
{
    tplot(x, ...)
}


## an alternative rendering function using the tessella API

tplot <-
    function(x,

             position = NULL, split = NULL,
             more = FALSE, newpage = TRUE,
             packet.panel = packet.panel.default,
             draw.in = NULL,
             panel.height = lattice.getOption("layout.heights")$panel,
             panel.width = lattice.getOption("layout.widths")$panel,
             save.object = lattice.getOption("save.object"),
             panel.error = lattice.getOption("panel.error"),
             prefix = NULL,
             ...,

             pages = seq_len(number.of.pages), # FIXME: always correct?
             primitives = lattice.getOption("backend"))

{
    if (is.null(primitives)) primitives <- tessella.getOption("backend")
    if (is.function(primitives)) primitives <- primitives()
    a <- attr(attach(primitives), "name")
    on.exit(detach(a, character.only = TRUE))

    ## if necessary, save current settings and apply temporary
    ## settings in x$par.settings

    if (!is.null(x$par.settings))
    {
        ## save current state, restore later
        opar <- trellis.par.get() ## get("lattice.theme", envir = .LatticeEnv)
        trellis.par.set(theme = x$par.settings)
        on.exit(trellis.par.set(opar, strict = 2L), add = TRUE)
    }

    ## do the same for lattice.options

    if (!is.null(x$lattice.options))
    {
        ## save current state, restore later
        oopt <- lattice.options(x$lattice.options)
        on.exit(lattice.options(oopt), add = TRUE)
    }

    ## We'll also allow arguments to print.trellis (or plot.trellis)
    ## to be included within a trellis object.  Partial matching is
    ## not done.

    if (!is.null(x$plot.args))
    {
        supplied <- names(x$plot.args)
        ## Can't think of a clean way, so...
        if ("position"     %in% supplied && missing(position))     position     <- x$plot.args$position
        if ("split"        %in% supplied && missing(split))        split        <- x$plot.args$split
        if ("more"         %in% supplied && missing(more))         more         <- x$plot.args$more
        if ("newpage"      %in% supplied && missing(newpage))      newpage      <- x$plot.args$newpage
        if ("packet.panel" %in% supplied && missing(packet.panel)) packet.panel <- x$plot.args$packet.panel
        if ("draw.in"      %in% supplied && missing(draw.in))      draw.in      <- x$plot.args$draw.in
        if ("panel.height" %in% supplied && missing(panel.height)) panel.height <- x$plot.args$panel.height
        if ("panel.width"  %in% supplied && missing(panel.width))  panel.width  <- x$plot.args$panel.width
        if ("save.object"  %in% supplied && missing(save.object))  save.object  <- x$plot.args$save.object
        if ("panel.error"  %in% supplied && missing(panel.error))  panel.error  <- x$plot.args$panel.error
        if ("prefix"       %in% supplied && missing(prefix))       prefix       <- x$plot.args$prefix
    }

    panel.error <- getFunctionOrName(panel.error)
    new <-  newpage && !lattice.getStatus("print.more") && is.null(draw.in)
    if (!is.null(draw.in))
    {
        warning("'draw.in' not yet implemented.")
        ## depth <- downViewport(draw.in)
        ## on.exit(upViewport(depth), add = TRUE)
    }

    on.exit(lattice.setStatus(print.more = more), add = TRUE)
    usual  <- (is.null(position) && is.null(split))

    ## This means this plot will be the first one on a new page, so reset counter
    if (new) lattice.setStatus(plot.index = 0L)

    ## get default prefix (is it useful without interactions?)
    pindex <- 1L + lattice.getStatus("plot.index")
    if (is.null(prefix)) prefix <- sprintf("plot_%02g", pindex)
    lattice.setStatus(plot.index = pindex)

    ## Set this here so that panel and axis functions can use it, but
    ## set it again before exiting (in case some user action has
    ## changed it in the meantime) so that further calls to
    ## trellis.focus() etc. can use it.

    lattice.setStatus(current.prefix = prefix)
    on.exit(lattice.setStatus(current.prefix = prefix), add = TRUE)

    ## Initialize list for this prefix
    .LatticeEnv$lattice.status[[prefix]] <- .defaultLatticePrefixStatus()
    ## Equivalently
    ## lattice.setStatus(structure(list(.defaultLatticePrefixStatus()),
    ##                             names = prefix))

    ## save the current object, if so requested.  This used to be done
    ## at the end, so that it wouldn't happen if there were errors
    ## during printing.  However, doing this now will allow things
    ## like trellis.panelArgs() to work in panel/axis functions, which
    ## I think is a better trade-off.

    ## FIXME: the problem with this is that if, e.g., the panel
    ## function calls print.trellis(), then the last object is not
    ## what one would expect.

    if (save.object)
    {
        lattice.setStatus(current.plot.saved = TRUE, prefix = prefix)
        ## assign("last.object", x, envir = .LatticeEnv)
        lattice.setStatus(last.object = x, prefix = prefix)
    }
    else
        lattice.setStatus(current.plot.saved = FALSE, prefix = prefix)
    
    
    ## used.condlevels corresponds to the indexed and permuted object.
    ## These also have to be integer indices rather than character
    ## labels (necessary for 'packet.panel' computations).
    ## original.condlevels is required to interpret the results of
    ## packet.panel and associate them with packets in the original
    ## object (which are defined in terms of the original levels)

    original.condlevels <- 
        used.condlevels <-
            lapply(x$condlevels, function(x) seq_along(x))
    used.condlevels <- 
        mapply("[", used.condlevels, x$index.cond,
               MoreArgs = list(drop = FALSE),
               SIMPLIFY = FALSE)
    used.condlevels <- used.condlevels[x$perm.cond]

    ## an array giving packet numbers corresponding to
    ## original.condlevels.  The idea is to be able to figure out the
    ## packet given levels of the conditioning variables.  The packets
    ## are naturally thought of as an array.  The packet number simply
    ## counts positions of this array in the standard order
    ## (i.e. lower dimensions vary faster).

    adim <- sapply(original.condlevels, length)
    packet.array <- seq_len(prod(adim))
    dim(packet.array) <- adim

    cond.max.levels <- sapply(used.condlevels, length)
    number.of.cond <- length(cond.max.levels)

    panel.layout <-
        compute.layout(x$layout, cond.max.levels, skip = x$skip)

    axis.line <- trellis.par.get("axis.line")
    axis.text <- trellis.par.get("axis.text")

    legend <- evaluate.legend(x$legend)
    ## legend is now a list of `tgrob's

    xaxis <-
        list(lty = 
             if (is.logical(x$x.scales$lty)) axis.line$lty
             else x$x.scales$lty,
             lwd =
             if (is.logical(x$x.scales$lwd)) axis.line$lwd
             else x$x.scales$lwd,
             col.line =
             if (is.logical(x$x.scales$col.line)) axis.line$col
             else x$x.scales$col.line,
             col.text =
             if (is.logical(x$x.scales$col)) axis.text$col
             else x$x.scales$col,
             alpha.line =
             if (is.logical(x$x.scales$alpha.line)) axis.line$alpha
             else x$x.scales$alpha.line,
             alpha.text =
             if (is.logical(x$x.scales$alpha)) axis.text$alpha
             else x$x.scales$alpha,
             font =
             if (is.logical(x$x.scales$font)) axis.text$font
             else x$x.scales$font,
             fontface =
             if (is.logical(x$x.scales$fontface)) axis.text$fontface
             else x$x.scales$fontface,
             fontfamily =
             if (is.logical(x$x.scales$fontfamily)) axis.text$fontfamily
             else x$x.scales$fontfamily,
             lineheight =
             if (is.logical(x$x.scales$lineheight)) axis.text$lineheight
             else x$x.scales$lineheight,
             cex =
             if (is.logical(x$x.scales$cex)) rep(axis.text$cex, length.out = 2)
             else x$x.scales$cex,
             rot =
             if (is.logical(x$x.scales$rot)) c(0, 0)
             else x$x.scales$rot)

    yaxis <-
        list(lty = 
             if (is.logical(x$y.scales$lty)) axis.line$lty
             else x$y.scales$lty,
             lwd =
             if (is.logical(x$y.scales$lwd)) axis.line$lwd
             else x$y.scales$lwd,
             col.line =
             if (is.logical(x$y.scales$col.line)) axis.line$col
             else x$y.scales$col.line,
             col.text =
             if (is.logical(x$y.scales$col)) axis.text$col
             else x$y.scales$col,
             alpha.line =
             if (is.logical(x$y.scales$alpha.line)) axis.line$alpha
             else x$y.scales$alpha.line,
             alpha.text =
             if (is.logical(x$y.scales$alpha)) axis.text$alpha
             else x$y.scales$alpha,
             font =
             if (is.logical(x$y.scales$font)) axis.text$font
             else x$y.scales$font,
             fontface =
             if (is.logical(x$y.scales$fontface)) axis.text$fontface
             else x$y.scales$fontface,
             fontfamily =
             if (is.logical(x$y.scales$fontfamily)) axis.text$fontfamily
             else x$y.scales$fontfamily,
             lineheight =
             if (is.logical(x$y.scales$lineheight)) axis.text$lineheight
             else x$y.scales$lineheight,
             cex =
             if (is.logical(x$y.scales$cex)) rep(axis.text$cex, length.out = 2)
             else x$y.scales$cex,
             rot =
             if (!is.logical(x$y.scales$rot)) x$y.scales$rot
             else if (x$y.scales$relation != "same" && is.logical(x$y.scales$labels)) c(90, 90)
             else c(0, 0))

    ## using device dimensions to calculate default layout:

    ## FIXME: Ideally to compute aspect ratio we should only consider
    ## the viewport (taking into account split, position, etc).
    context <- tget_context()
    
    if (panel.layout[1] == 0) 
    {
        ddim <- with(context, c(w, h)) # par("din") 
        device.aspect <- ddim[2] / ddim[1]
        panel.aspect <- 1 ## FIXME panel.height[[1]] / panel.width[[1]]

        plots.per.page <- panel.layout[2]
        m <- max (1, round(sqrt(panel.layout[2] * device.aspect / panel.aspect)))
        ## changes made to fix bug (PR#1744)
        n <- ceiling(plots.per.page/m)
        m <- ceiling(plots.per.page/n)
        panel.layout[1] <- n
        panel.layout[2] <- m
    }

    plots.per.page <- panel.layout[1] * panel.layout[2] 
    cols.per.page <- panel.layout[1]
    rows.per.page <- panel.layout[2]
    number.of.pages <- panel.layout[3]
    lattice.setStatus(current.plot.multipage = number.of.pages > 1, prefix = prefix)

    ## ## following now relegated to packet.panel 
    ## skip <- rep(x$skip, length.out = number.of.pages * rows.per.page * cols.per.page)

    x.alternating <- rep(x$x.scales$alternating, length.out = cols.per.page)
    y.alternating <- rep(x$y.scales$alternating, length.out = rows.per.page)
    x.relation.same <- x$x.scales$relation == "same"
    y.relation.same <- x$y.scales$relation == "same"

    main <- getLabelList(x$main, trellis.par.get("par.main.text"))
    sub <- getLabelList(x$sub, trellis.par.get("par.sub.text"))
    xlab <- getLabelList(x$xlab, trellis.par.get("par.xlab.text"), x$xlab.default)
    ylab <- getLabelList(x$ylab, trellis.par.get("par.ylab.text"), x$ylab.default) # , orient = 90
    xlab.top <- getLabelList(x$xlab.top, trellis.par.get("par.xlab.text"))
    ylab.right <- getLabelList(x$ylab.right, trellis.par.get("par.ylab.text")) # , orient = 90

    labels <- list(main = main, sub = sub, xlab = xlab, ylab = ylab, xlab.top = xlab.top, ylab.right = ylab.right)
    ## get par.strip.text

    par.strip.text <- trellis.par.get("add.text")
    par.strip.text$lines <- 1
    if (!is.null(x$par.strip.text)) 
        par.strip.text[names(x$par.strip.text)] <- x$par.strip.text

    layoutCalculations <-
        calculateTessellaLayout(x,
                                rows.per.page, cols.per.page,
                                number.of.cond,
                                panel.height, panel.width,
                                labels, 
                                x.alternating, y.alternating,
                                x.relation.same, y.relation.same,
                                xaxis, yaxis,
                                par.strip.text,
                                legend)

    lattice.setStatus(layout.details = layoutCalculations, prefix = prefix)
    lattice.setStatus(as.table = x$as.table, prefix = prefix)

    context <- tget_context()
    ## print(pages)
    for (page in pages)
        tplot_page(x, page = page,
                   panel.layout = panel.layout,
                   layoutCalculations = layoutCalculations,
                   labels = labels,
                   packet.panel = packet.panel,
                   used.condlevels = used.condlevels,
                   packet.array = packet.array,
                   xaxis = xaxis, yaxis = yaxis,
                   number.of.cond = number.of.cond,
                   par.strip.text = par.strip.text,
                   legend = legend,
                   panel.error = panel.error,
                   position = position, split = split, new = new,
                   prefix = prefix,
                   ## ...,
                   vp = tviewport(context))
}


tplot_page <- function(x, page = 1, panel.layout, layoutCalculations, labels,
                       packet.panel, used.condlevels, packet.array,
                       xaxis, yaxis, number.of.cond, par.strip.text, legend = NULL,
                       panel.error = NULL, position = NULL, split = NULL, new = TRUE,
                       ...,
                       prefix = NULL,
                       vp = NULL)
{
    if (is.null(vp))
    {
        context <- tget_context()
        vp <- tviewport(context)
    }
    else context <- vp$context
    tinitialize(context, newpage = new)
    on.exit(tfinalize())

    ## global.gpar <-
    ##     do.call(list,
    ##             updateList(trellis.par.get("grid.pars"),
    ##                        list(fontsize = trellis.par.get("fontsize")$text)))

    bg <- trellis.par.get("background")$col

    if (!is.null(position))
    {
        stopifnot (length(position) == 4)
        if (new)
        {
            ## grid.newpage()
            panel.fill(fill = bg, border = "transparent", vp = vp)
        }
        vp <- tviewport(tviewport(vp),                 # xlim, ylim = c(0, 1)
                        x = position[1], y = position[2],
                        w = position[3] - position[1],
                        h = position[4] - position[2]) # xlim, ylim = c(0, 1) also

        if (!is.null(split))
        {
            stopifnot (length(split) == 4)
            vp <- tviewport(vp, x = (split[1] - 1) / split[3], y = (split[2] - 1) / split[4],
                            w = 1/split[3], h = 1/split[4])
            ## pushViewport(viewport(layout = grid.layout(nrow = split[4], ncol = split[3]),
            ##                       name = trellis.vpname("split", prefix = prefix) ))
            ## pushViewport(viewport(layout.pos.row = split[2], layout.pos.col = split[1],
            ##                       name = trellis.vpname("split.location", prefix = prefix) ))
        }
    }
    else if (!is.null(split))
    {
        stopifnot(length(split) == 4)
        if (new)
        {
            ## grid.newpage()
            ## grid.rect(name = trellis.grobname("background", type=""),
            ##           gp = gpar(fill = bg, col = "transparent"))
            panel.fill(fill = bg, border = "transparent", vp = vp)
        }
        vp <- tviewport(vp, x = (split[1] - 1) / split[3], y = (split[2] - 1) / split[4],
                        w = 1/split[3], h = 1/split[4])
        ## pushViewport(viewport(layout = grid.layout(nrow = split[4], ncol = split[3]),
        ##                       name = trellis.vpname("split", prefix = prefix) ))
        ## pushViewport(viewport(layout.pos.row = split[2], layout.pos.col = split[1],
        ##                       name = trellis.vpname("split.location", prefix = prefix) ))
    }

    cols.per.page <- panel.layout[1]
    rows.per.page <- panel.layout[2]

    x.alternating <- rep(x$x.scales$alternating, length.out = cols.per.page)
    y.alternating <- rep(x$y.scales$alternating, length.out = rows.per.page)
    x.relation.same <- x$x.scales$relation == "same"
    y.relation.same <- x$y.scales$relation == "same"

    panel <- getFunctionOrName(x$panel) # shall use "panel" in do.call
    strip <- getFunctionOrName(x$strip)
    strip.left <- getFunctionOrName(x$strip.left)

    plot_layout <- 
        tlayout(widths = layoutCalculations$page.layout$widths,
                heights = layoutCalculations$page.layout$heights,
                respect.aspect = layoutCalculations$page.layout$respect,
                parent = vp)

    plotLabel <- function(label = NULL, ..., 
                          columns = names(layoutCalculations$pos.widths),
                          rows = names(layoutCalculations$pos.heights))
    {
        if (is.null(label)) return(invisible())
        c <- range(unlist(layoutCalculations$pos.widths[columns]))
        r <- range(unlist(layoutCalculations$pos.heights[rows]))
        lab_vp <- tviewport(plot_layout, c, r)
        ttext(0.5, 0.5, label, ..., vp = lab_vp)
        invisible()
    }
    do.call(plotLabel, c(labels$main, list(rows = "main")))
    do.call(plotLabel, c(labels$sub, list(rows = "sub")))
    do.call(plotLabel, c(labels$xlab, list(columns = c("panel", "axis.panel", "strip.left"), rows = "xlab")))
    do.call(plotLabel, c(labels$ylab, list(columns = "ylab", rows = c("panel", "axis.panel", "strip"), rot = 90)))
    do.call(plotLabel, c(labels$xlab.top, list(columns = c("panel", "axis.panel"), rows = "xlab.top")))
    do.call(plotLabel, c(labels$ylab.right, list(columns = "ylab.right", rows = c("panel", "axis.panel"))))

    ## preliminary loop through possible positions, doing some
    ## calculations that allow some helpful status variables to be
    ## set.

    ## these will also eventually be 'status' variables
    current.panel.positions <- matrix(0, rows.per.page, cols.per.page)
    current.packet.positions <- matrix(0, rows.per.page, cols.per.page)
    current.cond.levels <- vector(mode = "list", length = rows.per.page * cols.per.page)
    dim(current.cond.levels) <- c(rows.per.page, cols.per.page)

    ## first, initialize status variables

    current.panel.positions[, ] <- 0
    current.packet.positions[, ] <- 0
    current.cond.levels[, ] <- list(NULL)

    inverse.permutation <- order(x$perm.cond) # used later

    ## FIXME: panel.counter is supposed to keep track of how many
    ## panels have been plotted.  It should ideally keep track of
    ## multiple pages, but that is difficult if we refactor par-page
    ## drawing into a separate function (unless we return the value at
    ## the end of each page).  For now, it will provide per-page
    ## count.
    panel.counter <- 0
    
    for (row in seq_len(rows.per.page))
        for (column in seq_len(cols.per.page))
        {
            ## levels being used in this panel
            which.packet <- 
                packet.panel(layout = panel.layout,
                             condlevels = used.condlevels,
                             page = page,
                             row = row,
                             column = column,
                             skip = x$skip)

            if (!is.null(which.packet))
            {
                ## permute to restore original order
                which.packet <- which.packet[inverse.permutation]
                
                current.cond.levels[[row, column]] <- which.packet
                
                ## packet.number should be same as packet.array[which.packet]
                ##                                              ^^^^^^^^^^^
                ##                                          (length not fixed)

                packet.number <- 
                    do.call("[", c(list(x = packet.array), as.list(which.packet)))
                current.packet.positions[row, column] <- packet.number

                ## packet.number retrieves the appropriate entry of
                ## panel.args and [xy].limits. It has to be this way
                ## because otherwise non-trivial orderings will not
                ## work.
                
                ## But we also provide a simple incremental counter
                ## that may be used as a panel function argument
                
                panel.counter <- panel.counter + 1
                current.panel.positions[row, column] <- panel.counter
            }
        }

    lattice.setStatus(current.cond.levels = current.cond.levels, prefix = prefix)
    lattice.setStatus(current.panel.positions = current.panel.positions, prefix = prefix)
    lattice.setStatus(current.packet.positions = current.packet.positions, prefix = prefix)

    ## loop through positions again, doing the actual drawing
    ## this time

    strip.col.default.bg <-
        rep(trellis.par.get("strip.background")$col,
            length.out = number.of.cond)
    strip.col.default.fg <-
        rep(trellis.par.get("strip.shingle")$col,
            length.out = number.of.cond)
    strip.border <-
        lapply(trellis.par.get("strip.border"),
               function(x) rep(x, length.out = number.of.cond))
    ## str(strip.border)
    
    for (row in seq_len(rows.per.page))
        for (column in seq_len(cols.per.page))
        {
            lattice.setStatus(current.focus.row = row,
                              current.focus.column = column,
                              prefix = prefix)
            which.packet <- which.packet(prefix = prefix)
            if (!is.null(which.packet))
            {
                packet.number <- packet.number(prefix = prefix)
                ## this gives the row position from the bottom
                actual.row <- if (x$as.table)
                    (rows.per.page-row+1) else row

                pos.row <- layoutCalculations$pos.heights$panel[row]
                pos.col <- layoutCalculations$pos.widths$panel[column]
                
                xscale.comps <-
                    if (x.relation.same)
                        x$xscale.components(lim = x$x.limits, 
                                            ## (FIXME: needs work) packet.list = ...
                                            top = TRUE,
                                            
                                            ## rest passed on to
                                            ## calculateAxisComponents
                                            ## in the default
                                            ## case:
                                            at = x$x.scales$at,
                                            used.at = x$x.used.at,
                                            num.limit = x$x.num.limit,
                                            labels = x$x.scales$labels,
                                            logsc = x$x.scales$log,
                                            abbreviate = x$x.scales$abbreviate,
                                            minlength = x$x.scales$minlength,
                                            n = x$x.scales$tick.number,
                                            format.posixt = x$x.scales$format)
                    else 
                        x$xscale.components(lim = x$x.limits[[packet.number]], 
                                            ## FIXME: needs work packet.list = ...
                                            top = FALSE,

                                            ## rest passed on to
                                            ## calculateAxisComponents
                                            ## in the default
                                            ## case:

                                            at = if (is.list(x$x.scales$at))
                                            x$x.scales$at[[packet.number]]
                                            else x$x.scales$at,
                                            used.at = x$x.used.at[[packet.number]],
                                            num.limit = x$x.num.limit[[packet.number]],
                                            labels =
                                            if (is.list(x$x.scales$labels))
                                            x$x.scales$labels[[packet.number]]
                                            else x$x.scales$labels,
                                            logsc = x$x.scales$log,
                                            abbreviate = x$x.scales$abbreviate,
                                            minlength = x$x.scales$minlength,
                                            n = x$x.scales$tick.number,
                                            format.posixt = x$x.scales$format)

                yscale.comps <-
                    if (y.relation.same)
                        x$yscale.components(lim = x$y.limits, 
                                            ## FIXME: needs work packet.list = ...
                                            right = TRUE,

                                            ## rest passed on to
                                            ## calculateAxisComponents
                                            ## in the default
                                            ## case:
                                            at = x$y.scales$at,
                                            used.at = x$y.used.at,
                                            num.limit = x$y.num.limit,
                                            labels = x$y.scales$labels,
                                            logsc = x$y.scales$log,
                                            abbreviate = x$y.scales$abbreviate,
                                            minlength = x$y.scales$minlength,
                                            n = x$y.scales$tick.number,
                                            format.posixt = x$y.scales$format)
                    else 
                        x$yscale.components(lim = x$y.limits[[packet.number]], 
                                            ## FIXME: needs work packet.list = ...
                                            right = FALSE,

                                            ## rest passed on to
                                            ## calculateAxisComponents
                                            ## in the default
                                            ## case:

                                            at = if (is.list(x$y.scales$at))
                                            x$y.scales$at[[packet.number]]
                                            else x$y.scales$at,
                                            used.at = x$y.used.at[[packet.number]],
                                            num.limit = x$y.num.limit[[packet.number]],
                                            labels =
                                            if (is.list(x$y.scales$labels))
                                            x$y.scales$labels[[packet.number]]
                                            else x$y.scales$labels,
                                            logsc = x$y.scales$log,
                                            abbreviate = x$y.scales$abbreviate,
                                            minlength = x$y.scales$minlength,
                                            n = x$y.scales$tick.number,
                                            format.posixt = x$y.scales$format)

                xscale <- xscale.comps$num.limit
                yscale <- yscale.comps$num.limit

############################################
###      drawing panel background         ##
############################################

                clip_panel <- trellis.par.get("clip")$panel == "on"
                ## Create all the viewports we may need
                panel_vp <- 
                    tviewport(plot_layout, pos.col, pos.row,
                              xlim = xscale,
                              ylim = yscale)
                striptop_vp <- 
                    tviewport(plot_layout, pos.col, pos.row - 1,
                              xlim = xscale)
                stripleft_vp <- 
                    tviewport(plot_layout, pos.col - 1, pos.row,
                              ylim = yscale)

                if (clip_panel) tclip(panel_vp)
                panel.bg <- trellis.par.get("panel.background")
                if (!is.null(panel.bg$col) && (panel.bg$col != "transparent"))
                    panel.fill(col = panel.bg$col, border = "transparent")
                if (clip_panel) tunclip(panel_vp)
                
############################################
###        drawing the axes               ##
############################################


### Note: axes should always be drawn before the panel, so that
### background grids etc. can be drawn.

### FIXME: in the tessella-based implementation, we are not storing
### the viewports for later access.  Maybe we should, but for now, we
### are not.  So the trellis.focus() stuff won't work.
                
                ## X-axis above (viewport: either striptop or panel)

                lattice.setStatus(current.viewport = if (striptop_vp$h > 0) striptop_vp else panel_vp)
                x$axis(side = "top",
                       scales = x$x.scales,
                       components = xscale.comps,
                       as.table = x$as.table,
                       rot = xaxis$rot[2],
                       text.col = xaxis$col.text,
                       text.alpha = xaxis$alpha.text,
                       text.cex = xaxis$cex[2],
                       text.font = xaxis$font,
                       text.fontfamily = xaxis$fontfamily,
                       text.fontface = xaxis$fontface,
                       text.lineheight = xaxis$lineheight,
                       line.col = xaxis$col.line,
                       line.lty = xaxis$lty,
                       line.lwd = xaxis$lwd,
                       line.alpha = xaxis$alpha.line,
                       prefix = prefix)
                
                ## Y-axis to the left (viewport: either stripleft or panel)

                lattice.setStatus(current.viewport = if (stripleft_vp$w > 0) stripleft_vp else panel_vp)
                x$axis(side = "left",
                       scales = x$y.scales,
                       components = yscale.comps,
                       as.table = x$as.table,
                       rot = yaxis$rot[1],
                       text.col = yaxis$col.text,
                       text.alpha = yaxis$alpha.text,
                       text.cex = yaxis$cex[1],
                       text.font = yaxis$font,
                       text.fontfamily = yaxis$fontfamily,
                       text.fontface = yaxis$fontface,
                       text.lineheight = yaxis$lineheight,
                       line.col = yaxis$col.line,
                       line.lty = yaxis$lty,
                       line.lwd = yaxis$lwd,
                       line.alpha = yaxis$alpha.line,
                       prefix = prefix)

                ## viewport: panel for bottom/right and then panel
                lattice.setStatus(current.viewport = panel_vp)

                ## X-axis below 
                x$axis(side = "bottom",
                       scales = x$x.scales,
                       components = xscale.comps,
                       as.table = x$as.table,
                       rot = xaxis$rot[1],
                       text.col = xaxis$col.text,
                       text.alpha = xaxis$alpha.text,
                       text.cex = xaxis$cex[1],
                       text.font = xaxis$font,
                       text.fontfamily = xaxis$fontfamily,
                       text.fontface = xaxis$fontface,
                       text.lineheight = xaxis$lineheight,
                       line.col = xaxis$col.line,
                       line.lty = xaxis$lty,
                       line.lwd = xaxis$lwd,
                       line.alpha = xaxis$alpha.line,
                       prefix = prefix)

                ## Y-axis to the right
                x$axis(side = "right",
                       scales = x$y.scales,
                       components = yscale.comps,
                       as.table = x$as.table,
                       rot = yaxis$rot[2],
                       text.col = yaxis$col.text,
                       text.alpha = yaxis$alpha.text,
                       text.cex = yaxis$cex[2],
                       text.font = yaxis$font,
                       text.fontfamily = yaxis$fontfamily,
                       text.fontface = yaxis$fontface,
                       text.lineheight = yaxis$lineheight,
                       line.col = yaxis$col.line,
                       line.lty = yaxis$lty,
                       line.lwd = yaxis$lwd,
                       line.alpha = yaxis$alpha.line,
                       prefix = prefix)

                ## N.B.: We'll need panel_vp again later to draw a
                ## border around it.  However, this must be postponed
                ## till after the panel is drawn, since otherwise the
                ## border is liable to be obscured.
                        
                        
############################################
###        done drawing axes              ##
############################################



############################################
###        drawing the panel              ##
############################################

                ## lattice.setStatus(current.viewport = panel_vp)

                pargs <- c(x$panel.args[[packet.number]],
                           x$panel.args.common) #,

                ## FIXME: include prefix = prefix?  Could be
                ## important in very rare cases, but let's
                ## wait till someone actually has a relevant
                ## use-case.

                if (clip_panel) tclip(panel_vp)
                if (is.null(panel.error)) 
                {
                    checkArgsAndCall(panel, pargs)
                }
                else
                    tryCatch(checkArgsAndCall(panel, pargs),
                             error = function(e) panel.error(e))
                if (clip_panel) tunclip(panel_vp)

############################################
###       finished drawing panel          ##
############################################


#########################################################################
### Draw the box around panels.  This used to be done with clipping    ##
### on, which caused some subtle and apparently puzzling side effects. ##
#########################################################################

                axis.line <- trellis.par.get("axis.line")
                panel.fill(col = "transparent",
                           border = adjustcolorv(axis.line$col, axis.line$alpha),
                           lty = axis.line$lty,
                           lwd = axis.line$lwd,
                           vp = panel_vp)


#########################################
###      draw strip(s) on top         ###
#########################################

                if (!is.logical(strip)) # logical <==> FALSE
                {
                    lattice.setStatus(current.viewport = striptop_vp)
                    for(i in seq_len(number.of.cond))
                    {
                        ## Here, by which.given, I mean which
                        ## in the original order, not the
                        ## permuted order

                        which.given <- x$perm.cond[i]
                        which.panel <- which.packet
                        
                        lattice.setStatus(current.which.given = which.given,
                                          current.which.panel = which.panel,
                                          prefix = prefix)
                        
                        strip(which.given = which.given,
                              which.panel = which.panel,
                              var.name = names(x$condlevels),
                              factor.levels = as.character(x$condlevels[[x$perm.cond[i]]]),
                              shingle.intervals = if (is.list(x$condlevels[[x$perm.cond[i]]]))
                              do.call("rbind", x$condlevels[[x$perm.cond[i]]]) else NULL,
                              horizontal = TRUE,
                              bg = strip.col.default.bg[i],
                              fg = strip.col.default.fg[i],
                              par.strip.text = par.strip.text)
                    }
                }
                


#########################################
###        draw strip(s) on left      ###
#########################################

                if (!is.logical(strip.left)) # logical <==> FALSE
                {
                    lattice.setStatus(current.viewport = stripleft_vp)
                    for(i in seq_len(number.of.cond))
                    {
                        ## Here, by which.given, I mean which
                        ## in the original packet order, not
                        ## the permuted order

                        which.given <- x$perm.cond[i]
                        which.panel <- which.packet
                        
                        lattice.setStatus(current.which.given = which.given,
                                          current.which.panel = which.panel,
                                          prefix = prefix)
                                
                        strip.left(which.given = which.given,
                                   which.panel = which.panel,
                                   var.name = names(x$condlevels),
                                   factor.levels = as.character(x$condlevels[[x$perm.cond[i]]]),
                                   shingle.intervals = if (is.list(x$condlevels[[x$perm.cond[i]]]))
                                   do.call("rbind", x$condlevels[[x$perm.cond[i]]]) else NULL,
                                   horizontal = FALSE,
                                   bg = strip.col.default.bg[i],
                                   fg = strip.col.default.fg[i],
                                   par.strip.text = par.strip.text)
                    }
                }
            }
        }

    if (!is.null(legend))
    {
        plotLegend <- function(obj = NULL, ..., 
                               columns = names(layoutCalculations$pos.widths),
                               rows = names(layoutCalculations$pos.heights))
        {
            if (is.null(obj)) return(invisible())
            c <- range(unlist(layoutCalculations$pos.widths[columns]))
            r <- range(unlist(layoutCalculations$pos.heights[rows]))
            key_vp <- tviewport(plot_layout, c, r)
            obj$draw(key_vp)
            invisible()
        }
        
        locs <- names(legend)
        for (i in seq_along(legend))
        {
            key.space <- locs[i]
            key.gf <- legend[[i]]$obj
            switch(key.space,
                   left = plotLegend(legend[[i]]$obj, columns = "key.left", rows = c("panel", "axis.panel", "strip")),
                   right = plotLegend(legend[[i]]$obj, columns = "key.right", rows = c("panel", "axis.panel", "strip")),
                   bottom = plotLegend(legend[[i]]$obj, rows = "key.bottom", columns = c("panel", "axis.panel", "strip.left")),
                   top = plotLegend(legend[[i]]$obj, rows = "key.top", columns = c("panel", "axis.panel", "strip.left")),
                   inside = warning("legend 'inside' not yet implemented"))
            ## FIXME: inside
        }
    }

    ## page.layout <- $page.layout
    ## pos.heights <- layoutCalculations$pos.heights
    ## pos.widths <- layoutCalculations$pos.widths

    ## panel.layout <- panel.layout[,, page, drop = FALSE] # array of packets
    ## dim(panel.layout) <- dim(panel.layout)[1:2] # matrix for current page
    ## ldim <- dim(panel.layout)


    ## packets <- x$packets
    ## panel.vars <- x$panel.vars
    ## panel <- x$panel
    ## if (!is.list(panel)) panel <- list(panel)
    ## limits <- x$shared.env$limits
    ## panel.args <- ## everything in x except those that are not
    ##     x[setdiff(names(x), c("panel.layout", "panel"))]


}




calculateTessellaLayout <-
    function(x,
             rows.per.page, cols.per.page,
             number.of.cond,
             panel.height = NULL, panel.width = NULL,

             labels,
             ## main, sub,
             ## xlab, ylab, xlab.top, ylab.right,

             x.alternating, y.alternating,
             x.relation.same, y.relation.same,

             xaxis, yaxis,

             par.strip.text,

             legend)
    ## x: the trellis object
{
    ## The idea here is to create a layout with proper widths and
    ## heights (representing the requisite amounts of space required
    ## for different components of the plot -- see descriptions below)
    ## using the various units available in grid.

    ## Most of these components are fairly easy to define, with one
    ## exception -- namely those that involve axis labels. For
    ## instance, one (or more) _columns_ would usually contain the
    ## y-axis tick-labels. The width of this column is determined by
    ## ALL the y-labels; basically, the width of the column would be
    ## the MAXIMUM of the widths of the individual labels.

    ## This is in general not an easy problem, since relative width
    ## depends on the font used (also perhaps the device). Till
    ## lattice version 0.6, this was dealt with naively by treating
    ## the label with highest nchar() to be the widest. Unfortunately,
    ## this was no longer possible with labels that were
    ## expressions. So, after grid.text started supporting expression
    ## markups, the method of determining widths/heights for tick
    ## labels has changed. The new method essentially calculates the
    ## MAXIMUM of several grid UNIT objects (using calls like
    ## max(unit(...))) .

    ## The problem with this is that it is impossible to define the
    ## 'units' argument of those parts of the eventual layout when
    ## it's first being defined (it is not "null", "lines" or anything
    ## like that). So, those parts are calculated as separate units
    ## (via max.unit) and then inserted into the layout later.

    ## All this makes the code a bit difficult to follow. I just hope
    ## this gives some hints to whoever (probably me!) tries to
    ## decipher the following code on some later date.


    ## list giving positions (for indexing) of various components

    last.panel <- (rows.per.page - 1) * 4 + 10
    between.seq <- seq_len(rows.per.page-1)
    panel.seq <- seq_len(rows.per.page)

    if (!x$as.table)
    {
        between.seq <- rev(between.seq)
        panel.seq <- rev(panel.seq)
    }
    pos.heights <-
        list(top.padding       = 1,
             main              = 2,
             main.key.padding  = 3,
             key.top           = 4,
             xlab.top          = 5, #new
             key.axis.padding  = 6,
             axis.top          = 7,
             strip             = (panel.seq - 1) * 4 + 8,
             panel             = (panel.seq - 1) * 4 + 9,
             axis.panel        = (panel.seq - 1) * 4 + 10,
             between           = (between.seq - 1) * 4 + 11,
             axis.bottom       = last.panel + 1,
             axis.xlab.padding = last.panel + 2,
             xlab              = last.panel + 3,
             xlab.key.padding  = last.panel + 4,
             key.bottom        = last.panel + 5,
             key.sub.padding   = last.panel + 6,
             sub               = last.panel + 7,
             bottom.padding    = last.panel + 8)

    last.panel <- (cols.per.page - 1) * 4 + 9
    pos.widths <-
        list(left.padding      = 1,
             key.left          = 2,
             key.ylab.padding  = 3,
             ylab              = 4,
             ylab.axis.padding = 5,
             axis.left         = 6,
             axis.panel        = (seq_len(cols.per.page) - 1) * 4 + 7,
             strip.left        = (seq_len(cols.per.page) - 1) * 4 + 8,
             panel             = (seq_len(cols.per.page) - 1) * 4 + 9,
             between           = (seq_len(cols.per.page-1) - 1) * 4 + 10,
             axis.right        = last.panel + 1,
             axis.key.padding  = last.panel + 2,
             ylab.right        = last.panel + 3, # new
             key.right         = last.panel + 4,
             right.padding     = last.panel + 5)

    n.row <- sum(sapply(pos.heights, length))
    n.col <- sum(sapply(pos.widths, length))


    ## aspect != "fill"

    layout.respect <- !x$aspect.fill

    ## if (layout.respect)
    ## {
    ##     layout.respect <- matrix(0, n.row, n.col)
    ##     layout.respect[pos.heights$panel, pos.widths$panel] <- 1
    ## }

    ## Shall now construct height and width components needed for the
    ## layout. See ?unit before trying to follow this.

    ## placeholders:
    heights.x <- numeric(n.row)
    widths.x <- numeric(n.col)

    ## default dimensions
    heights.defaults <- lattice.getOption("layout.heights")
    widths.defaults <- lattice.getOption("layout.widths")

    ## finally settings values, act as multipliers (usually all 1)
    heights.settings <- trellis.par.get("layout.heights")
    widths.settings <- trellis.par.get("layout.widths")

    ## set default units. These may be scaled by user controlled
    ## [heights/widths].settings (usually just 1)

    for (nm in names(heights.defaults))
    {
        heights.x[pos.heights[[nm]]] <- heights.settings[[nm]] * heights.defaults[[nm]]$x
    }
    for (nm in names(widths.defaults))
    {
        widths.x[pos.widths[[nm]]] <- widths.settings[[nm]] * widths.defaults[[nm]]$x
    }

    ## fine tuning:

    heights.x[pos.heights[["panel"]]] <- -1 * x$aspect.ratio
    widths.x[pos.widths[["panel"]]] <- -1
    
    if (rows.per.page > 1)
        heights.x[pos.heights[["between"]]] <-
            rep(x$y.between, length.out = rows.per.page - 1) * 10 # FIXME - make constants customizable
    if (cols.per.page > 1)
        widths.x[pos.widths[["between"]]] <-
            rep(x$x.between, length.out = cols.per.page - 1) * 10

    getHeight <- function(x)
    {
        1.2 * tstrheight(x$label, cex = x$cex, font = x$font)
    }
    if (!is.null(labels$main)) heights.x[pos.heights[["main"]]] <- heights.settings[["main"]] * getHeight(labels$main)
    if (!is.null(labels$sub)) heights.x[pos.heights[["sub"]]] <- heights.settings[["sub"]] * getHeight(labels$sub)
    if (!is.null(labels$xlab)) heights.x[pos.heights[["xlab"]]] <- heights.settings[["xlab"]] * getHeight(labels$xlab)
    if (!is.null(labels$ylab)) widths.x[pos.widths[["ylab"]]] <- widths.settings[["ylab"]] * getHeight(labels$ylab)
    if (!is.null(labels$xlab.top)) heights.x[pos.heights[["xlab.top"]]] <- heights.settings[["xlab.top"]] * getHeight(labels$xlab.top)
    if (!is.null(labels$ylab.right)) widths.x[pos.widths[["ylab.right"]]] <- widths.settings[["ylab.right"]] * getHeight(labels$ylab.right)

    if (!is.null(legend))
    {
        ## update data as necessary. FIXME: need to change x too?
        nl <- names(legend)
        if ("left" %in% nl)
        {
            widths.x[ pos.widths[["key.left"]] ] <- widths.settings[["key.left"]] * legend$left$obj$minwidth
        }
        if ("right" %in% nl)
        {
            widths.x[ pos.widths[["key.right"]] ] <- widths.settings[["key.right"]] * legend$right$obj$minwidth
        }
        if ("top" %in% nl)
        {
            heights.x[ pos.heights[["key.top"]] ] <- heights.settings[["key.top"]] * legend$top$obj$minheight
        }
        if ("bottom" %in% nl)
        {
            heights.x[ pos.heights[["key.bottom"]] ] <- heights.settings[["key.top"]] * legend$bottom$obj$minheight
        }
    }

    heights.x[pos.heights[["strip"]]] <-
        if (is.logical(x$strip)) 0  # which means strip = F, strips not to be drawn
        else heights.x[pos.heights[["strip"]]] * par.strip.text$cex * par.strip.text$lines * number.of.cond

    widths.x[pos.widths[["strip.left"]]] <-
        if (is.logical(x$strip.left)) 0  # which means strip = F, strips not to be drawn
        else widths.x[pos.widths[["strip.left"]]] * par.strip.text$cex * par.strip.text$lines * number.of.cond


    ## All the upcoming scary code about labels is only to determine
    ## how much space to leave for them. Much of these calculations
    ## will be repeated later before actually drawing them. 

    if (x$x.scales$draw)
    {
        axis.units <- lattice.getOption("axis.units")[["outer"]][c("top", "bottom")]
        axis.settings <- trellis.par.get("axis.components")[c("top", "bottom")]

        if (x.relation.same)
        {
            ## this means we need to allocate space for
            ## pos.heights[axis.top] and pos.heights[axis.bottom]

            lab.comps <- 
                x$xscale.components(x$x.limits,
                                    at = x$x.scales$at,
                                    used.at = x$x.used.at,
                                    num.limit = x$x.num.limit,
                                    labels = x$x.scales$labels,
                                    logsc = x$x.scales$log,
                                    abbreviate = x$x.scales$abbreviate,
                                    minlength = x$x.scales$minlength,
                                    format.posixt = x$x.scales$format,
                                    n = x$x.scales$tick.number)

            ## top

            if (!is.logical(lab.comps$top) || lab.comps$top)
            {
                lab.comps.top <- 
                    if (is.logical(lab.comps$top)) # must be TRUE
                        lab.comps$bottom
                    else
                        lab.comps$top
                lab <- lab.comps.top$labels$labels
                tick.x <- max(0, x$x.scales$tck[2] * axis.units$top$tick$x * axis.settings$top$tck * lab.comps.top$ticks$tck)
                pad1.x <- axis.units$top$pad1$x * axis.settings$top$pad1
                pad2.x <- axis.units$top$pad2$x * axis.settings$top$pad2
                ## FIXME: dims may depend on other gpars like font (but
                ## implementing that maybe overkill)
                lab.x <- if (length(lab) > 0) max(tstrheight(lab, cex = xaxis$cex[2], rot = xaxis$rot[2])) else 0
                heights.x[pos.heights[["axis.top"]]] <- 
                    heights.settings[["axis.top"]] * (lab.x * (any(x.alternating==2 | x.alternating==3)) + 
                                                      tick.x + pad1.x + pad2.x)
            }
            
            ## bottom
            lab.comps.bottom <- lab.comps$bottom
            lab <- lab.comps.bottom$labels$labels
            tick.x <- max(0, x$x.scales$tck[1] * axis.units$bottom$tick$x * axis.settings$bottom$tck * lab.comps.bottom$ticks$tck)
            pad1.x <- axis.units$bottom$pad1$x * axis.settings$bottom$pad1
            pad2.x <- axis.units$bottom$pad2$x * axis.settings$bottom$pad2
            lab.x <- if (length(lab) > 0) max(tstrheight(lab, cex = xaxis$cex[1], rot = xaxis$rot[1])) else 0
            heights.x[pos.heights[["axis.bottom"]]] <- 
                heights.settings[["axis.bottom"]] * (lab.x * (any(x.alternating==1 | x.alternating==3)) + 
                                                     tick.x + pad1.x + pad2.x)
        }
        else
        { # relation != same

            ## Basically need to allocate space for the tick labels.
            ## Technically, could have different heights for different
            ## rows, but don't want to go there (probably won't look
            ## good anyway). So, boils down to finding all the
            ## labels. Note that from R 2.0.0 onwards, the user can
            ## scale space for each axis individually, so such fine
            ## control is still possible

            pad1.x <- axis.units$bottom$pad1$x * axis.settings$bottom$pad1
            pad2.x <- axis.units$bottom$pad2$x * axis.settings$bottom$pad2

            ## we'll take max of ALL panels, even those that may not
            ## be present in this particular page or even this
            ## particular plot

            lab.xvec <- numeric(length(x$x.limits))
            tick.xvec <- numeric(length(x$x.limits))
            for (i in seq_along(x$x.limits))
            {
                lab.comps <-
                    x$xscale.components(x$x.limits[[i]],
                                        at = if (is.list(x$x.scales$at)) x$x.scales$at[[i]] else x$x.scales$at,
                                        used.at = x$x.used.at[[i]],
                                        num.limit = x$x.num.limit[[i]],
                                        labels = if (is.list(x$x.scales$labels))
                                        x$x.scales$labels[[i]] else x$x.scales$labels,
                                        logsc = x$x.scales$log,
                                        abbreviate = x$x.scales$abbreviate,
                                        minlength = x$x.scales$minlength,
                                        n = x$x.scales$tick.number,
                                        format.posixt = x$x.scales$format)
                lab <- lab.comps$bottom$labels$labels
                lab.xvec[i] <- if (length(lab) > 0) max(tstrheight(lab, cex = xaxis$cex[1], rot = xaxis$rot[1])) else 0
                tick.xvec[i] <- max(0, x$x.scales$tck[1] * axis.units$bottom$tick$x * axis.settings$bottom$tck * lab.comps$bottom$ticks$tck)
            }
            lab.x <- max(lab.xvec)
            tick.x <- max(tick.xvec)
            heights.x[pos.heights[["axis.panel"]]] <-
                heights.settings[["axis.bottom"]] * (lab.x + tick.x + pad1.x + pad2.x)
        }
    }

    ## same for y-axes now

    if (x$y.scales$draw)
    {
        axis.units <- lattice.getOption("axis.units")[["outer"]][c("right", "left")]
        axis.settings <- trellis.par.get("axis.components")[c("right", "left")]

        if (y.relation.same)
        {
            ## this means we need to allocate space for
            ## pos.widths[axis.right] and pos.widths[axis.left]

            lab.comps <- 
                x$yscale.components(x$y.limits,
                                    at = x$y.scales$at,
                                    used.at = x$y.used.at,
                                    num.limit = x$y.num.limit,
                                    labels = x$y.scales$labels,
                                    logsc = x$y.scales$log,
                                    abbreviate = x$y.scales$abbreviate,
                                    minlength = x$y.scales$minlength,
                                    format.posixt = x$y.scales$format,
                                    n = x$y.scales$tick.number)

            ## right

            if (!is.logical(lab.comps$right) || lab.comps$right)
            {
                lab.comps.right <- 
                    if (is.logical(lab.comps$right)) # must be TRUE
                        lab.comps$left
                    else
                        lab.comps$right
                lab <- lab.comps.right$labels$labels
                tick.x <- max(0, x$y.scales$tck[2] * axis.units$right$tick$x * axis.settings$right$tck * lab.comps.right$ticks$tck)
                pad1.x <- axis.units$right$pad1$x * axis.settings$right$pad1
                pad2.x <- axis.units$right$pad2$x * axis.settings$right$pad2
                ## FIXME: dims may depend on other gpars like font (but
                ## implementing that maybe overkill)
                lab.x <- if (length(lab) > 0) max(tstrwidth(lab, cex = yaxis$cex[2], rot = yaxis$rot[2])) else 0
                widths.x[pos.widths[["axis.right"]]] <- 
                    widths.settings[["axis.right"]] * (lab.x * (any(y.alternating==2 | y.alternating==3)) + 
                                                      tick.x + pad1.x + pad2.x)
            }
            
            ## left
            lab.comps.left <- lab.comps$left
            lab <- lab.comps.left$labels$labels
            tick.x <- max(0, x$y.scales$tck[1] * axis.units$left$tick$x * axis.settings$left$tck * lab.comps.left$ticks$tck)
            pad1.x <- axis.units$left$pad1$x * axis.settings$left$pad1
            pad2.x <- axis.units$left$pad2$x * axis.settings$left$pad2
            lab.x <- if (length(lab) > 0) max(tstrwidth(lab, cex = yaxis$cex[1], rot = yaxis$rot[2])) else 0
            widths.x[pos.widths[["axis.left"]]] <- 
                widths.settings[["axis.left"]] * (lab.x * (any(y.alternating==1 | y.alternating==3)) + 
                                                     tick.x + pad1.x + pad2.x)
        }
        else
        { # relation != same

            ## Basically need to allocate space for the tick labels.
            ## Technically, could have different widths for different
            ## rows, but don't want to go there (probably won't look
            ## good anyway). So, boils down to finding all the
            ## labels. Note that from R 2.0.0 onwards, the user can
            ## scale space for each axis individually, so such fine
            ## control is still possible

            pad1.x <- axis.units$left$pad1$x * axis.settings$left$pad1
            pad2.x <- axis.units$left$pad2$x * axis.settings$left$pad2

            ## we'll take max of ALL panels, even those that may not
            ## be present in this particular page or even this
            ## particular plot

            lab.xvec <- numeric(length(x$y.limits))
            tick.xvec <- numeric(length(x$y.limits))
            for (i in seq_along(x$y.limits))
            {
                lab.comps <-
                    x$yscale.components(x$y.limits[[i]],
                                        at = if (is.list(x$y.scales$at)) x$y.scales$at[[i]] else x$y.scales$at,
                                        used.at = x$y.used.at[[i]],
                                        num.limit = x$y.num.limit[[i]],
                                        labels = if (is.list(x$y.scales$labels))
                                        x$y.scales$labels[[i]] else x$y.scales$labels,
                                        logsc = x$y.scales$log,
                                        abbreviate = x$y.scales$abbreviate,
                                        minlength = x$y.scales$minlength,
                                        n = x$y.scales$tick.number,
                                        format.posixt = x$y.scales$format)
                lab <- lab.comps$left$labels$labels
                lab.xvec[i] <- if (length(lab) > 0) max(tstrwidth(lab, cex = yaxis$cex[1], rot = yaxis$rot[1])) else 0
                tick.xvec[i] <- max(0, x$y.scales$tck[1] * axis.units$left$tick$x * axis.settings$left$tck * lab.comps$left$ticks$tck)
            }
            lab.x <- max(lab.xvec)
            tick.x <- max(tick.xvec)
            widths.x[pos.widths[["axis.panel"]]] <-
                widths.settings[["axis.left"]] * (lab.x + tick.x + pad1.x + pad2.x)
        }
    }

    ## Having determined heights and widths, now construct the layout:

    page.layout <- list(widths = widths.x, heights = heights.x, respect = layout.respect)
    list(page.layout = page.layout,
         pos.heights = pos.heights,
         pos.widths = pos.widths)
}


