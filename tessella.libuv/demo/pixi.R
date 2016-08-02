
library(httpuv)

app <- initApp()


server <- startDaemonizedServer("0.0.0.0", 9555, app)

stopDaemonizedServer(server)


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


# check the value of .lastMessage after echoing to check it is being updated

# call this after done
#stopDaemonizedServer(server)


vector2json <- function(x)
{
    sprintf("[%s]", paste(as.character(x), collapse=","))
}

color2json <- function(col)
{
    sprintf("0x%s", toupper(paste(as.hexmode(col2rgb(col, alpha = FALSE)), collapse="")))
}

ssend <- function(s, ...) app$wsock$send(sprintf(s, ...))

## while(T)
## {

N <- 50
x <- round(runif(N)*400)
y <- round(runif(N)*400)

ssend("newpage();")
ssend("setPar('stroke', %s);", color2json(sample(colors(), 1)))
ssend("setPar('fill', %s);", color2json("#888888"))
ssend("setPar('salpha', 1);")
ssend("setPar('falpha', 0.5);")

ssend("points(%s, %s);", vector2json(x), vector2json(y))
## ssend("lines(%s, %s);", vector2json(x), vector2json(y))
ssend("update();")

N <- 10
x0 <- round(runif(N)*400)
y0 <- round(runif(N)*400)
x1 <- round(runif(N)*400)
y1 <- round(runif(N)*400)

ssend("segments(%s, %s, %s, %s);", vector2json(x0), vector2json(y0), vector2json(x1), vector2json(y1))
ssend("rect(%s, %s, %s, %s);", vector2json(x0), vector2json(y0), vector2json(x1), vector2json(y1))
ssend("update();")

ssend("text(200, 200, 'Test message', 1, 45);")
## ssend("text(200, 200, 'বাংলা');") ## doesn't work
ssend("update();")

## }

source("pixi_primitives.R")


library(imp)
.impenv$backend <- pixi_primitives(app, 400, 400)

