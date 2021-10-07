

### Copyright (C) 2001-2006 Deepayan Sarkar
### <Deepayan.Sarkar@R-project.org>
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



## convenient shortcut to create custom strip functions from
## strip.default. Looks a bit dicey, may not always work :-/

strip.custom <-
    function(...)
{
    args <- list(...)
    function(...)
    {
        dots <- list(...)
        do.call("strip.default",
                updateList(dots, args))
    }
}






paste.and.draw <-
    function(left, right, sep = " : ",
             horizontal = TRUE,
             center = TRUE,
             showl = TRUE,
             showr = TRUE,
             gp = gpar(),
             vp)
{
    ## We are already in a viewport.  Essentially want to draw
    ## paste(left, right, sep = sep) in the middle.  The catch is,
    ## left and right (maybe even sep) may be expressions.  The easy
    ## solution is to draw sep in the middle and left and right on
    ## either side.  The better solution is to combine and then
    ## center.

    ## if (is.expression(left) || is.expression(right)) warning("Expressions not yet supported")
    if (showl || showr)
    {
        myvp <- tviewport(vp, xlim = c(0, vp$w), ylim = c(0, vp$h))
        shows <- showl && showr
        tw <- showl * tstrwidth(left) + showr * tstrwidth(right) + shows * tstrwidth(sep)
        offset <- if (center) (showl * tstrwidth(left) - showr * tstrwidth(right)) / 2 else 0
        if (horizontal)
        {
            if (shows) ttext(x = vp$w/2 + offset, y = vp$h/2, labels = sep, adj = 0.5, vp = myvp) # gp$col etc
            if (showl) ttext(x = vp$w/2 + offset, y = vp$h/2, labels = left, adj = c(1, 0.5), vp = myvp)
            if (showr) ttext(x = vp$w/2 + offset, y = vp$h/2, labels = right, adj = c(0, 0.5), vp = myvp)
        }
        else
        {
            if (shows) ttext(x = vp$w/2, y = vp$h/2 + offset, labels = sep, adj = 0.5, rot = 90, vp = myvp) # gp$col etc
            if (showl) ttext(x = vp$w/2, y = vp$h/2 + offset, labels = left, adj = c(1, 0.5), rot = 90, vp = myvp)
            if (showr) ttext(x = vp$w/2, y = vp$h/2 + offset, labels = right, adj = c(0, 0.5), rot = 90, vp = myvp)
        }
    }
}




strip.default <-
    function(which.given,
             which.panel,
             var.name,
             factor.levels,
             shingle.intervals = NULL,
             strip.names = c(FALSE, TRUE),
             strip.levels = c(TRUE, FALSE),
             sep = " : ",
             style = 1,
             horizontal = TRUE,
             ## FIXME: not sure how to incorporate alpha in strip colors
             bg = trellis.par.get("strip.background")$col[which.given],
             fg = trellis.par.get("strip.shingle")$col[which.given],
             par.strip.text = trellis.par.get("add.text"),
             vp = lattice.getStatus("current.viewport"))
{
    do_clip <- (trellis.par.get("clip")$strip == "on")
    svp <- 
        if (horizontal)
            tviewport(vp,
                      x = vp$xlim[1], w = diff(vp$xlim),
                      y = (which.given-1)/length(which.panel),
                      h = 1/length(which.panel))
        else 
            tviewport(vp,
                      x = (which.given-1)/length(which.panel),
                      w = 1/length(which.panel),
                      y = vp$ylim[1], h = diff(vp$ylim))
    if (do_clip)
    {
        tclip(svp)
    }

    gp.text <- 
        list(col = par.strip.text$col,
             alpha = par.strip.text$alpha,
             lineheight = par.strip.text$lineheight,
             fontfamily = par.strip.text$fontfamily,
             fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
             cex = par.strip.text$cex)

    name <- var.name[which.given]
    level <- which.panel[which.given]
    strip.names <- rep(strip.names, length.out = 2)
    strip.levels <- rep(strip.levels, length.out = 2)
    ## str(shingle.intervals)

    formatLabel <-
        function(s,
                 abbreviate = par.strip.text$abbr,
                 minlength = par.strip.text$minl,
                 dot = par.strip.text$dot)
    {
        if (is.null(abbreviate)) abbreviate <- FALSE
        if (is.null(minlength)) minlength <- 4
        if (is.null(dot)) dot <- FALSE
        if (abbreviate) abbreviate(s, minlength = minlength, dot = dot)
        else s
    }
    factor.levels <- formatLabel(factor.levels)

    if (!is.null(shingle.intervals))
    {
        ## This usually indicates shingles, as opposed to factors.
        ## 'style' will be completely ignored, and shingle.intervals
        ## encoded using bg and fg.  Names and levels are both game.

        type <- if (horizontal) "strip" else "strip.left"
        panel.fill(col = bg, border = bg, vp = svp)

        t <- range(shingle.intervals)
        r <- if (diff(t) == 0) 0.5 + c(-0.005, 0.005)
             else (range(shingle.intervals[level,]) - t[1]) / diff(t)

        ## make sure r is not too thin when singleton - at least 1% of range
        if (r[1] == r[2]) 
        {
            if (r[1] == 0) r[2] <- 0.01
            else if (r[2] == 1) r[1] <- 0.99
            else r <- r + c(-0.005, 0.005)
        }
        if (horizontal)
            trect(xleft = r[1], xright = r[2],
                  ybottom = 0, ytop = 1,
                  col = fg, fill = fg, vp = svp)
        else 
            trect(xleft = 0, xright = 1,
                  ybottom = r[1], ytop = r[2],
                  col = fg, fill = fg, vp = svp)
        paste.and.draw(name, factor.levels[level],
                       sep = sep,
                       horizontal = horizontal,
                       showl = strip.names[2],
                       showr = strip.levels[2],
                       gp = gp.text, vp = svp)
    }
    else
    {
        ## Behaviour depends on 'style'.  Will separate out coloring
        ## and text based on 'style'.

        num <- length(factor.levels)

        ## coloring:

        ## background: all except style = 2
        if (style != 2) {
            panel.fill(col = bg, border = bg, vp = svp)
        }

        ## foreground: needed only for style = 2, 3 and 4
        ## FIXME
        if (FALSE && num > 0 && style %in% c(2, 3, 4))
        {
            if (horizontal)
            {
                grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                          width = unit(1/num, "npc"),
                          name = trellis.grobname("fg", type = "strip"),
                          gp = gpar(fill = fg, col = fg))
            }
            else
            {
                grid.rect(y = unit((2*level-1)/(2*num), "npc"),
                          height = unit(1/num, "npc"),
                          name = trellis.grobname("fg", type = "strip.left"),
                          gp = gpar(fill = fg, col = fg))
            }
        }

        ## text: [names|levels] centered only if style = 1 or 3

        if (style %in% c(1, 3))
        {
            paste.and.draw(name, factor.levels[level],
                           sep = sep,
                           horizontal = horizontal,
                           showl = strip.names[1],
                           showr = strip.levels[1],
                           gp = gp.text, vp = svp)
        }
        ## remaining cases
        else if (num > 0)
        {
            ## either all levels or only one
            lid <- if (style %in% c(2, 4)) 1:num else level
            if (horizontal)
            {
                ttext(x = (2 * lid - 1) / (2 * num), y = 0.5, labels = factor.levels[lid],
                      col = gp.text$col, vp = svp) # FIXME more pars
            }
            else
            {
                ttext(x = 0.5,
                      y = (2 * lid - 1) / (2 * num),
                      labels = factor.levels[lid],
                      rot = 90, col = gp.text$col, vp = svp) # FIXME more pars
            }
        }
    }

    ## border is drawn with clipping off
    if (do_clip) tunclip(svp)
    strip.border <- trellis.par.get("strip.border")
    panel.fill(col = "transparent",
               border =  rep(strip.border$col, length.out = which.given)[which.given],
               lty = rep(strip.border$lty, length.out = which.given)[which.given],
               lwd = rep(strip.border$lwd, length.out = which.given)[which.given],
               alpha = rep(strip.border$alpha, length.out = which.given)[which.given],
               vp = svp)
}





