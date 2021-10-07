library("lattice0")

densityplot(~ eruptions, data = faithful)

densityplot(~ eruptions, data = faithful, 
            kernel = "rect", bw = 0.2, plot.points = "rug", n = 200)

## library("latticeExtra")
## data(gvhd10)
data(gvhd10, package = "latticeExtra")

densityplot(~log(FSC.H) | Days, data = gvhd10, 
            plot.points = FALSE, ref = TRUE, layout = c(2, 4))

histogram(~log2(FSC.H) | Days, gvhd10, xlab = "log Forward Scatter",
          type = "density", nint = 50, layout = c(2, 4))

data(Chem97, package = "mlmRev")

qqmath(~ gcsescore | factor(score), data = Chem97, 
       f.value = ppoints(100))

qqmath(~ gcsescore | gender, Chem97, groups = score, aspect = "xy", 
       f.value = ppoints(100), auto.key = list(space = "right"),
       xlab = "Standard Normal Quantiles", 
       ylab = "Average GCSE Score")

Chem97.mod <- transform(Chem97, gcsescore.trans = gcsescore^2.34)

qqmath(~ gcsescore.trans | gender, Chem97.mod, groups = score,
       f.value = ppoints(100), aspect = "xy",
       auto.key = list(space = "right", title = "score"), 
       xlab = "Standard Normal Quantiles", 
       ylab = "Transformed GCSE Score")

## library("latticeExtra")
## ecdfplot(~ gcsescore | factor(score), data = Chem97, 
##          groups = gender, auto.key = list(columns = 2),
##          subset = gcsescore > 0, xlab = "Average GCSE Score")

qqmath(~ gcsescore | factor(score), data = Chem97, groups = gender, 
       auto.key = list(points = FALSE, lines = TRUE, columns = 2),
       subset = gcsescore > 0, type = "l", distribution = qunif, 
       prepanel = prepanel.qqmathline, aspect = "xy",
       xlab = "Standard Normal Quantiles", 
       ylab = "Average GCSE Score")

qq(gender ~ gcsescore | factor(score), Chem97, 
   f.value = ppoints(100), aspect = 1)

bwplot(factor(score) ~ gcsescore | gender, data = Chem97, 
       xlab = "Average GCSE Score")

bwplot(gcsescore^2.34 ~ gender | factor(score), Chem97, 
       varwidth = TRUE, layout = c(6, 1),
       ylab = "Transformed GCSE score")

bwplot(Days ~ log(FSC.H), data = gvhd10, 
       xlab = "log(Forward Scatter)", ylab = "Days Past Transplant")

## bwplot(Days ~ log(FSC.H), gvhd10, 
##        panel = panel.violin, box.ratio = 3,
##        xlab = "log(Forward Scatter)", 
##        ylab = "Days Past Transplant")

stripplot(factor(mag) ~ depth, quakes)

stripplot(depth ~ factor(mag), quakes, 
          jitter.data = TRUE, alpha = 0.6,
          xlab = "Magnitude (Richter)", ylab = "Depth (km)")

stripplot(sqrt(abs(residuals(lm(yield~variety+year+site)))) ~ site, 
          data = barley, groups = year, jitter.data = TRUE,
          auto.key = list(points = TRUE, lines = TRUE, columns = 2),
          type = c("p", "a"), fun = median,
          ylab = expression(abs("Residual Barley Yield")^{1 / 2}))

