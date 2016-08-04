
library(tessella.libuv)

app <- initApp()

server <- httpuv::startDaemonizedServer("0.0.0.0", 9555, app)

browseURL("http://localhost:9555/")

## call this after done, or to reset
## stopDaemonizedServer(server)

if (FALSE) ## direct test of send()
{
    ## app$wsock$send('initPage();')
    app$wsock$send('renderer.view.style.border = "1px solid blue";')
    app$wsock$send('myrect();')
    app$wsock$send('renderer.render(stage);')
    app$wsock$send('gprim.clear(stage);')
    app$wsock$send('renderer.render(stage);')
    N <- 5000
    x <- round(runif(N)*400)
    y <- round(runif(N)*400)
    for (i in 1:N)
        app$wsock$send(sprintf('gprim.circle(stages[0], %d, %d, 5, 0x005500, 0xAAAAAA22);', x[i], y[i]))
    app$wsock$send('renderer.render(stage);')
    app$wsock$send('gprim.clear(stages[0]);')
}

## source("pixi_primitives.R")

library(imp)
.impenv$backend <- pixi_primitives(app, 400, 400)

p <- yplot(data = mtcars, # aspect = "xy",
           margin.vars = elist(gear = factor(gear)),
           panel.vars = elist(x = disp, y = mpg, size = wt),
           panel = ypanel.xyplot())

p

yplot(data = mtcars,
      panel.vars = elist(x = factor(gear), y = mpg),
      panel = ypanel.grid(v = -1, h = 0) + ypanel.boxplot(),
      switch.axes = TRUE)


