


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



## accessors for a grid layouts nrow and ncol

layoutNRow <- function(x) x$nrow
layoutNCol <- function(x) x$ncol

## other accessors, for during (and sometimes after) plotting

current.row <- function(prefix = lattice.getStatus("current.prefix"))
    lattice.getStatus("current.focus.row", prefix = prefix)

current.column <- function(prefix = lattice.getStatus("current.prefix"))
    lattice.getStatus("current.focus.column", prefix = prefix)

trellis.currentLayout <-
    function(which = c("packet", "panel"),
             prefix = lattice.getStatus("current.prefix"))
{
    which <- match.arg(which)
    switch(which,
           packet = lattice.getStatus("current.packet.positions", prefix = prefix),
           panel = lattice.getStatus("current.panel.positions", prefix = prefix))
}

panel.number <- function(prefix = lattice.getStatus("current.prefix"))
{
    trellis.currentLayout("panel",
                          prefix = prefix)[current.row(prefix = prefix),
                                           current.column(prefix = prefix)]
}

packet.number <- function(prefix = lattice.getStatus("current.prefix"))
{
    trellis.currentLayout("packet",
                          prefix = prefix)[current.row(prefix = prefix),
                                           current.column(prefix = prefix)]
}

which.packet <- function(prefix = lattice.getStatus("current.prefix"))
{
    lattice.getStatus("current.cond.levels",
                      prefix = prefix)[[current.row(prefix = prefix),
                                        current.column(prefix = prefix)]]
}


## utility to create a full-fledged list describing a label from parts
## (used for main, sub, xlab, ylab)

getLabelList <- function(label, text.settings, default.label = NULL)
{
    if (!is.null(label))
    {
        ans <-
            list(label = 
                 if (is.characterOrExpression(label)) label
                 else if (is.list(label) && (is.null(names(label)) ||
                                             names(label)[1] == "")) label[[1]]
                 else default.label)
        if ("label" %in% names(text.settings)) text.settings$label <- NULL
        ans <- updateList(ans, text.settings)
        if (is.list(label) && !is.null(names(label)))
        {
            if (names(label)[1] == "") label <- label[-1]
            ans <- updateList(ans, label)
        }
    }
    else ans <- NULL
    if (is.null(ans$label) ||
        (is.character(ans) && ans$label == "")) ans <- NULL
    ans
}


drawInViewport <-
    function(obj, vp)
{
    pushViewport(vp)
    grid.draw(obj)
    upViewport()
}




grobFromLabelList <- function(lab, name = "label", orient = 0)
{
    if (is.null(lab) || (is.character(lab) && (lab == "" || length(lab) == 0))) return (NULL)
    if (inherits(lab, "grob")) return(lab)
    process.lab <-
        function(label, rot = orient,
                 x = NULL, y = NULL,
                 just = "centre",
                 hjust = NULL, vjust = NULL,
                 check.overlap = FALSE,
                 font = NULL, fontfamily = NULL, fontface = NULL,
                 ...)
        {
            ans <-
                list(label = label, rot = rot, x = x, y = y,
                     just = just, hjust = hjust, vjust = vjust,
                     check.overlap = check.overlap)
            ans$gplist <- 
                gpar(fontfamily = fontfamily,
                     fontface = chooseFace(fontface, font),
                     ...)
            ans
        }
    lab <- do.call(process.lab, lab)
    if (with(lab, is.null(label) || (is.character(label) && (label == "" || length(label) == 0))))
        return (NULL)
    if (is.null(lab$x))
        lab$x <-
            if (orient == 0) ppoints(n = length(lab$label), a = 0.5)
            else 0.5 
    if (is.null(lab$y))
        lab$y <-
            if (orient == 90) ppoints(n = length(lab$label), a = 0.5)
            else 0.5
    textGrob(label = lab$label,
             x = lab$x,
             y = lab$y,
             name = name,
             just = lab$just,
             hjust = lab$hjust,
             vjust = lab$vjust,
             check.overlap = lab$check.overlap,
             rot = lab$rot,
             gp = lab$gplist)
    ##     gpar(col = lab$col,
    ##                   fontfamily = lab$fontfamily,
    ##                   fontface = chooseFace(lab$fontface, lab$font),
    ##                   lineheight = lab$lineheight,
    ##                   alpha = lab$alpha,
    ##                   cex = lab$cex))
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




panel.error <- function(e)
{
    print(gettextf("Error using packet %g\n%s",
                   panel.number(),
                   conditionMessage(e)))
}


## S3 print method for "trellis" objects

print.trellis <- function(x, ...)
{
    printFunction <- lattice.getOption("print.function")
    if (is.null(printFunction)) printFunction <- tplot
    printFunction(x, ...)
    invisible(x)
}

