library("lattice0")

methods(class = "trellis")
methods(class = "shingle")
methods(generic.function = "barchart")
dp.uspe <- 
    dotplot(t(USPersonalExpenditure), 
            groups = FALSE, 
            index.cond = function(x, y) median(x),
            layout = c(1, 5), 
            type = c("p", "h"),
            xlab = "Expenditure (billion dollars)")
dp.uspe.log <- 
    dotplot(t(USPersonalExpenditure), 
            groups = FALSE, 
            index.cond = function(x, y) median(x),
            layout = c(1, 5), 
            scales = list(x = list(log = 2)),
            xlab = "Expenditure (billion dollars)")
plot(dp.uspe,     split = c(1, 1, 2, 1), more = TRUE)
plot(dp.uspe.log, split = c(2, 1, 2, 1), more = FALSE)
state <- data.frame(state.x77, state.region, state.name)
state$state.name <- 
    with(state, reorder(reorder(state.name, Frost), 
                        as.numeric(state.region)))
dpfrost <- 
    dotplot(state.name ~ Frost | reorder(state.region, Frost),
            data = state, layout = c(1, 4),
            scales = list(y = list(relation = "free")))
summary(dpfrost)
plot(dpfrost, 
     panel.height = list(x = c(16, 13, 9, 12), unit = "null"))
update(trellis.last.object(), layout = c(1, 1))[2]
npanel <- 12
rot <- list(z = seq(0, 30, length = npanel), 
            x = seq(0, -80, length = npanel))
quakeLocs <-
    cloud(depth ~ long + lat, quakes, pch = ".", cex = 1.5,
          panel = function(..., screen) {
              pn <- panel.number()
              panel.cloud(..., screen = list(z = rot$z[pn], 
                                             x = rot$x[pn]))
          },
          xlab = NULL, ylab = NULL, zlab = NULL, 
          scales = list(draw = FALSE), zlim = c(690, 30), 
          par.settings = list(axis.line = list(col="transparent")))
quakeLocs[rep(1, npanel)]
data(Chem97, package="mlmRev")
ChemQQ <- 
    qq(gender ~ gcsescore | factor(score), Chem97, 
       f.value = ppoints(100), strip = strip.custom(style = 5))
tmd(ChemQQ)
library("latticeExtra")
data(biocAccess)
baxy <- xyplot(log10(counts) ~ hour | month + weekday, biocAccess,
               type = c("p", "a"), as.table = TRUE,
               pch = ".", cex = 2, col.line = "black")
dimnames(baxy)$month
dimnames(baxy)$month <- month.name[1:5]
dimnames(baxy)
useOuterStrips(baxy)


