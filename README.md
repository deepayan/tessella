tessella
========

A framework for R graphics

Tessella is supposed to be a grid-like package for R providing
viewports, layouts, etc.  It is backend-agnostic, with multiple
backends providing actual plotting functionality.  Code written using
the tessella API should work on all supported backends.

The tessella package defines the basic API and provides the default
backend using the graphics package.

The quilt package provides Qt-based backends based on the qtbase and
qtpaint packages (see https://github.com/ggobi/qtbase and
https://github.com/ggobi/qtpaint).


