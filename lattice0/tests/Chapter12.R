library("lattice0")

state <- data.frame(state.x77, state.region)
trellis.vpname("xlab", prefix = "plot1")
trellis.vpname("strip", column = 2, row = 2, prefix = "plot2")


data(Chem97, package = "mlmRev")
qqmath(~ gcsescore | factor(score), Chem97, groups = gender,
       f.value = function(n) ppoints(100),
       aspect = "xy", 
       page = function(n) {
           cat("Click on plot to place legend", fill = TRUE)
           ll <- grid.locator(unit = "npc")
           if (!is.null(ll))
               draw.key(simpleKey(levels(factor(Chem97$gender))),
                        vp = viewport(x = ll$x, y = ll$y),
                        draw = TRUE)
       })


state <- data.frame(state.x77, state.region)
xyplot(Murder ~ Life.Exp | state.region, data = state, 
       layout = c(2, 2), type = c("p", "g"), subscripts = TRUE)
while (!is.null(fp <- trellis.focus())) {
    if (fp$col > 0 & fp$row > 0)
        panel.identify(labels = rownames(state))
}


qqmath(~ (1000 * Population / Area), state, 
       ylab = "Population Density (per square mile)",
       xlab = "Standard Normal Quantiles",
       scales = list(y = list(log = TRUE, at = 10^(0:3))))
trellis.focus()
do.call(panel.qqmathline, trellis.panelArgs())
panel.identify.qqmath(labels = row.names(state))
trellis.unfocus()


env <- environmental
env$ozone <- env$ozone^(1/3)
splom(env, pscales = 0, col = "grey")
trellis.focus("panel", 1, 1, highlight = FALSE)
panel.link.splom(pch = 16, col = "black")
trellis.unfocus()


state$name <- with(state, 
                   reorder(reorder(factor(rownames(state)), Frost), 
                           as.numeric(state.region)))
dotplot(name ~ Frost | reorder(state.region, Frost), data = state, 
        layout = c(1, 4), scales = list(y = list(relation="free")))
trellis.currentLayout()
heights <- 
    sapply(seq_len(nrow(trellis.currentLayout())),
           function(i) {
               trellis.focus("panel", column = 1, row = i, 
                             highlight = FALSE)
               h <- diff(current.panel.limits()$ylim)
               trellis.unfocus()
               h
           })
heights
update(trellis.last.object(), 
       par.settings = list(layout.heights = list(panel = heights)))


