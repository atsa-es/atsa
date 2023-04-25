## ----setup, include=FALSE---------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
library(MARSS)
set.seed(123)
data("Nile")

mod_list <- list(B = "identity", U = "zero", Q = matrix("q"),
                 Z = "identity", A = matrix("a"), R = matrix("r"))

fit <- MARSS(matrix(Nile, nrow = 1), mod_list)


## ----ex_Nile_flow-----------------------------------------------------------------------------
data("Nile")

par(mai = c(0.9, 0.9, 0.1, 0.1), omi = c(0, 0, 0, 0))

plot.ts(Nile, las = 1,
        xlab = "Year", ylab = "Flow of the River Nile")


## ----ex_Nile_DLM------------------------------------------------------------------------------
par(mai = c(0.9, 0.9, 0.1, 0.1), omi = c(0, 0, 0, 0))

plot.ts(Nile, las = 1, lwd = 2,
        xlab = "Year", ylab = "Flow of the River Nile")
lines(seq(start(Nile)[1], end(Nile)[1]),
       lwd = 2, t(fit$states), col = "blue")
lines(seq(start(Nile)[1], end(Nile)[1]), t(fit$states + 2*fit$states.se),
       lwd = 2, lty = "dashed", col = "blue")
lines(seq(start(Nile)[1], end(Nile)[1]), t(fit$states - 2*fit$states.se),
       lwd = 2, lty = "dashed", col = "blue")

