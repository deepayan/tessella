
### Copyright (C) 2000-2006 Deepayan Sarkar <Deepayan.Sarkar@R-project.org>,
###
### This file is part of the lattice package for R.  It is made
### available under the terms of the GNU General Public License,
### version 2, or at your option, any later version, incorporated
### herein by reference.
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


.LatticeEnv <- new.env()
assign("lattice.status",  list(), envir = .LatticeEnv)
assign("lattice.theme",   list(), envir = .LatticeEnv)
assign("lattice.options", list(), envir = .LatticeEnv)
## assign("last.object",     NULL,   envir = .LatticeEnv)


.onLoad <- function(libname, pkgname) 
{
    ## library.dynam("lattice", pkgname, libname )
    lattice.options(.defaultLatticeOptions())
    lattice.setStatus(.defaultLatticeStatus())
}

.noGenerics <- TRUE

.onUnload <- function(libpath)
    library.dynam.unload("lattice0", libpath)



