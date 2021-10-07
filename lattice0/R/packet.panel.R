
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



## Note: the following is only meant to be used in print.trellis.  It
## supercedes the use of cupdate there.  However, cupdate will
## continue to be used by high level functions.


packet.panel.default <-
    function(layout, condlevels,
             page, row, column,
             skip, all.pages.skip = TRUE)
{
    ## assume layout has been standardized

    ## condlevels is a list, as long as the number of conditioning
    ## variables, each component being a vector of integer indices
    ## representing levels.  This must already have considered any
    ## effects of index.cond and perm.cond.

    panels.per.page <- layout[1] * layout[2]
    panels.per.row <- layout[1]
    panels.per.column <- layout[2]

    packet.order <- do.call(expand.grid, condlevels)

    ## Don't try to force the default behaviour of skip elsewhere,
    ## since this gives users the freedom to make it behave as they
    ## want.

    if (all.pages.skip)
    {
        skip <- rep(skip, length.out = panels.per.page * page)
    }
    else
    {
        skip <- rep(skip, length.out = panels.per.page)
        skip <- rep(skip, page)
    }
    ## This covers current page, which is all we care about. 
    
    panel.number <- 1 + 
        (page - 1) * panels.per.page +
            (row - 1) * panels.per.row +
                (column - 1)

    if (skip[panel.number]) return(NULL)
    panel.number <- panel.number - sum(head(skip, panel.number))
    if (panel.number > nrow(packet.order)) return(NULL)

    as.numeric(packet.order[panel.number, ])
}

