
library(tessella.libuv)

uvapp <- initApp(template.file = system.file("resources/canvas.html.template", 
                                             package = "tessella.libuv"))

server <- httpuv::startDaemonizedServer("0.0.0.0", 9555, uvapp)

browseURL("http://localhost:9555/")

## call this after done, or to reset
## httpuv::stopDaemonizedServer(server)

## source("pixi_primitives.R")


## using lattice0

lattice.options(backend = canvas_primitives(uvapp, guessTextDims = TRUE))

xyplot(sunspot.year)

## using imp

library(imp)
.impenv$backend <- canvas_primitives(uvapp, guessTextDims = TRUE)

p <- yplot(data = mtcars, # aspect = "xy",
           margin.vars = elist(gear = factor(gear)),
           panel.vars = elist(x = disp, y = mpg, size = wt),
           panel = ypanel.xyplot())

p

yplot(data = mtcars,
      panel.vars = elist(x = factor(gear), y = mpg),
      panel = ypanel.grid(v = 0, h = -1) + ypanel.boxplot(),
      ylab = "Miles per gallon",
      switch.axes = FALSE)


yplot(data = mtcars, # aspect = "xy",
      margin.vars = elist(gear = factor(gear)),
      panel.vars = elist(x = disp, y = mpg, size = wt),
      panel = ypanel.lines(col = "red"))

yplot(data = mtcars, ##aspect = "xy",
      margin.vars = elist(gear = factor(gear)),
      panel.vars = elist(x = disp, y = mpg, size = wt),
      panel = ypanel.xyplot(fill = TRUE))

