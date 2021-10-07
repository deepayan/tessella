library("lattice0")

data(Oats, package = "MEMSS")
tp1.oats <- 
    xyplot(yield ~ nitro | Variety + Block, data = Oats, type = 'o')
print(tp1.oats)
dim(tp1.oats)
dimnames(tp1.oats)
xtabs(~Variety + Block, data = Oats)
summary(tp1.oats)
summary(tp1.oats[, 1])
print(tp1.oats[, 1])
update(tp1.oats, 
       aspect="xy")
update(tp1.oats, aspect = "xy",
       layout = c(0, 18))
update(tp1.oats, aspect = "xy", layout = c(0, 18), 
       between = list(x = c(0, 0, 0.5), y = 0.5))
dotplot(variety ~ yield | site, barley, 
        layout = c(1, 6), aspect = c(0.7),
        groups = year, auto.key = list(space = 'right'))
key.variety <- 
    list(space = "right", text = list(levels(Oats$Variety)),
         points = list(pch = 1:3, col = "black"))
xyplot(yield ~ nitro | Block, Oats, aspect = "xy", type = "o", 
       groups = Variety, key = key.variety, lty = 1, pch = 1:3, 
       col.line = "darkgrey", col.symbol = "black",
       xlab = "Nitrogen concentration (cwt/acre)",
       ylab = "Yield (bushels/acre)", 
       main = "Yield of three varieties of oats",
       sub = "A 3 x 4 split plot experiment with 6 blocks")
barchart(Class ~ Freq | Sex + Age, data = as.data.frame(Titanic), 
         groups = Survived, stack = TRUE, layout = c(4, 1),
         auto.key = list(title = "Survived", columns = 2))
barchart(Class ~ Freq | Sex + Age, data = as.data.frame(Titanic), 
         groups = Survived, stack = TRUE, layout = c(4, 1), 
         auto.key = list(title = "Survived", columns = 2),
         scales = list(x = "free"))
bc.titanic <- 
    barchart(Class ~ Freq | Sex + Age, as.data.frame(Titanic), 
             groups = Survived, stack = TRUE, layout = c(4, 1),
             auto.key = list(title = "Survived", columns = 2),
             scales = list(x = "free"))
update(bc.titanic, 
       panel = function(...) {
           panel.grid(h = 0, v = -1)
           panel.barchart(...)
       })
update(bc.titanic, 
       panel = function(..., border) {
           panel.barchart(..., border = "transparent")
       })


