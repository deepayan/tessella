
library(tessella.libuv)

uvapp <- initApp()

server <- httpuv::startDaemonizedServer("0.0.0.0", 9555, uvapp)

browseURL("http://localhost:9555/")

## call this after done, or to reset
## httpuv::stopDaemonizedServer(server)

if (FALSE) ## direct test of send()
{
    ## uvapp$wsock$send('initPage();')
    uvapp$wsock$send('renderer.view.style.border = "1px solid blue";')
    uvapp$wsock$send('myrect();')
    uvapp$wsock$send('renderer.render(stage);')
    uvapp$wsock$send('gprim.clear(stage);')
    uvapp$wsock$send('renderer.render(stage);')
    N <- 5000
    x <- round(runif(N)*400)
    y <- round(runif(N)*400)
    for (i in 1:N)
        uvapp$wsock$send(sprintf('gprim.circle(stages[0], %d, %d, 5, 0x005500, 0xAAAAAA22);', x[i], y[i]))
    uvapp$wsock$send('renderer.render(stage);')
    uvapp$wsock$send('gprim.clear(stages[0]);')
}

## source("pixi_primitives.R")

library(imp)
.impenv$backend <- pixi_primitives(uvapp, 1000, 800)

p <- yplot(data = mtcars, # aspect = "xy",
           margin.vars = elist(gear = factor(gear)),
           panel.vars = elist(x = disp, y = mpg, size = wt),
           panel = ypanel.xyplot())

p

yplot(data = mtcars,
      panel.vars = elist(x = factor(gear), y = mpg),
      panel = ypanel.grid(v = 0, h = -1) + ypanel.boxplot(),
      switch.axes = FALSE)


