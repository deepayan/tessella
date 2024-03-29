library("lattice0")

## pkg <- getOption("lattice")
## if (is.null(pkg)) pkg <- "lattice"
## library(pkg, character.only = TRUE)


data(Chem97, package = "mlmRev")
xtabs( ~ score, data = Chem97)
histogram(~ gcsescore | factor(score), data = Chem97)
densityplot(~ gcsescore | factor(score), data = Chem97, 
            plot.points = FALSE, ref = TRUE)
densityplot(~ gcsescore, data = Chem97, groups = score,
            plot.points = FALSE, ref = TRUE,
            auto.key = list(columns = 3))
tp1 <- histogram(~ gcsescore | factor(score), data = Chem97)
tp2 <- 
    densityplot(~ gcsescore, data = Chem97, groups = score,
                plot.points = FALSE,
                auto.key = list(space = "right", title = "score"))
class(tp2)
summary(tp1)
plot(tp1, split = c(1, 1, 1, 2))
plot(tp2, split = c(1, 2, 1, 2), newpage = FALSE)

