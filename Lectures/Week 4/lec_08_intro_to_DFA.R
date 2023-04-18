## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
set.seed(123)


## ----ts_with_AR_errors--------------------------------------------------------------------------------------------------------
set.seed(123)
tt <- 30
ee <- rnorm(tt)
ww <- rnorm(tt)
phi <- 0.7
for(t in 2:tt) {
  ee[t] <- phi * ee[t-1] + ww[t]
}

aa <- 5
bb <- 1
xx <- rnorm(tt) + 2

yy <- aa + bb * xx + ee

par(mai = c(0.9, 0.9, 0.3, 0.1), omi = c(0, 0, 0, 0))

plot.ts(yy, ylim = c(min(xx,yy), max(xx, yy)), las = 1,
        col = "dodgerblue", lwd = 2,
        ylab = expression(italic(x[t])~~or~~italic(y[t])))
lines(xx, col = "blue", lwd = 2)
text(xx[1], expression(italic(x[t])), pos = 3, col = "blue")
text(yy[1], expression(italic(y[t])), pos = 3, col = "dodgerblue")
mtext(side = 3, expression(italic(y[t]) == 1~+~5~italic(x[t])~+~italic(e[t])),
      line = 0.4, adj = 0)


## ----model_residuals----------------------------------------------------------------------------------------------------------

par(mai = c(0.9, 0.9, 0.3, 0.1), omi = c(0, 0, 0, 0))

plot.ts(ee, col = "darkred", lwd = 2,
        ylab = expression(italic(e[t])))


## ----ACF_model_residuals------------------------------------------------------------------------------------------------------

par(mai = c(0.9, 0.9, 0.3, 0.1), omi = c(0, 0, 0, 0))

acf(ee)


## ----MARSS_DFA_setup, echo = TRUE, eval = FALSE-------------------------------------------------------------------------------
## y = data         ## [1 x T] matrix of data
## a = matrix("a")  ## intercept
## D = matrix("D")  ## slope
## d = covariate    ## [1 x T] matrix of measured covariate
## Z = matrix(1)    ## no multiplier on x
## R = matrix(0)    ## v_t ~ N(0,R); want v_t = 0 for all t


## ----MARSS_AR1_setup, echo = TRUE, eval = FALSE-------------------------------------------------------------------------------
## B = matrix("b")  ## AR(1) coefficient for model errors
## Q = matrix("q")  ## w_t ~ N(0,Q); var for model errors
## u = matrix(0)    ## u = 0
## C = matrix(0)    ## C = 0
## c = matrix(0)    ## c_t = 0 for all t


## ----plot_many_ts, echo = FALSE, fig.align = 'center'-------------------------------------------------------------------------
NN <- 25
TT <- 30
MM <- 3
 
## MM x TT matrix of innovations
ww <- matrix(rnorm(MM * TT, 0, 1), MM, TT)
ww[,1] <- rnorm(MM, 0, sqrt(5))
## MM x TT matrix of scaled latent trends
xx <- t(scale(apply(ww, 1, cumsum)))

## loadings matrix
ZZ <- matrix(runif(NN * MM, -1, 1), NN, MM)
diag(ZZ) <- rev(sort(abs(diag(ZZ))))
ZZ[upper.tri(ZZ)] <- 0
ZZ <- round(ZZ, 2)

## obs var
obs_var <- 0.2^2
## obs errors
ee <- t(MASS::mvrnorm(TT, matrix(0, NN, 1), diag(obs_var, NN, NN)))
## NN x TT matrix of observed data
yy <- ZZ %*% xx + ee

clr <- viridis::plasma(NN, alpha = 0.7, end = 0.8)

vv <- sample(seq(NN), NN)

par(mfrow = c(5, 5), mai = c(0.1, 0.1, 0, 0), omi = c(0, 0, 0, 0)) 

for(i in 1:NN) {
	plot.ts(yy[vv[i],], lwd = 2,
	        xlab = "", xaxt = "n", ylab = "", yaxt = "n",
	        col = clr[i], bty = "n")
}


## ----plot_many_ts_2, echo = FALSE, fig.align='center'-------------------------------------------------------------------------
par(mfrow = c(5,5), mai = c(0.1,0.1,0,0), omi = c(0, 0, 0, 0)) 

for(i in 1:NN) {
	plot.ts(yy[vv[i],], lwd = 2,
	        xlab = "", xaxt = "n", ylab = "", yaxt = "n",
	        col = clr[i], bty = "n")
}


## ----plot_dfa_trends, fig.align="center"--------------------------------------------------------------------------------------
## plot the trends
par(mfrow = c(1,3), mai = c(1.2, 0, 0, 0), omi = rep(0.1, 4))
clr <- viridis::plasma(MM, end = 0.8)
for(i in 1:3) {
	plot.ts(xx[i,], lwd = 3,
	        xlab = "", xaxt = "n", ylab = "", yaxt = "n",
	        col = clr[i], bty = "n")
}


## ----ex_corr_ts---------------------------------------------------------------------------------------------------------------
nn <- 100
xx <- rnorm(nn, 10, 1)
yy <- xx + rnorm(nn, 5)
xx <- xx - mean(xx)
yy <- yy - mean(yy)

par(mai = c(0.9, 0.9, 0.1, 0.1), omi = c(0, 0, 0, 0))

plot(xx, yy, pch = 16, las = 1,
     ylab = "Y", xlab = "X")

text(-2, 2,
     substitute(rho == x, list(x = round(cor(cbind(xx, yy))[2,1], 2))))


## ----ex_PCA-------------------------------------------------------------------------------------------------------------------
pca <- prcomp(cbind(xx, yy))

pc1 <- pca$rotation[,1]
pc2 <- matrix(c(0, -1, 1, 0), 2, 2) %*% matrix(pc1, 2, 1)

par(mai = c(0.9, 0.9, 0.1, 0.1), omi = c(0, 0, 0, 0))

plot(xx, yy, pch = 16, las = 1,
     ylab ="Y", xlab = "X")
arrows(0, 0, pc1[1], pc1[2],
       col = "blue", lwd = 3, length = 0.1)
text(pc1[1], pc1[2], "PC1",
     pos = 3, col = "blue")
arrows(0, 0, pc2[1], pc2[2],
       col = "darkred", lwd = 3, length = 0.1)
text(pc2[1], pc2[2], "PC2",
     pos = 1, col = "darkred")


## ----ex_PCA_rotated-----------------------------------------------------------------------------------------------------------
par(mai = c(0.9, 0.9, 0.1, 0.1), omi = c(0, 0, 0, 0))

plot(pca$x[,1], pca$x[,2], pch = 16, las = 1,
     ylab = "PC2", xlab = "PC1")

text(-4, 1.2,
     substitute(rho == x, list(x = round(cor(pca$x)[2,1], 2))))


## ----ex_PCA_wines-------------------------------------------------------------------------------------------------------------
dat <- read.csv("wine_data_for_PCA.csv")
colnames(dat)[2:3] <- c("For meat", "For dessert")

pca <- prcomp(dat, scale. = TRUE)

loads <- pca$rotation
if(loads[1,2] < 0) { loads[,2] <- -1 * loads[,2] }

load1 <- loads[,1]
load2 <- loads[,2]

par(mai = c(0.9, 0.9, 0.1, 0.1), omi = c(0, 1.5, 0, 1.5))

poz <- c(3, 1, 3, 3, 3, 4, 2)
  
plot(load1, load2, pch = 16,
     xlim = c(min(load1), max(load1)),
     ylim = c(min(load2), max(load2)),
     xaxt = "n", yaxt = "n", bty = "n",
     xlab = "", ylab = "")
abline(h = 0, col = "darkgray")
abline(v = 0, col = "darkgray")
for(i in 1:ncol(dat)) {
  text(load1[i], load2[i], colnames(dat)[i],
       pos = poz[i], xpd = NA)
}
text(0, min(load2) * 1.05, "X2", pos = 1, xpd = NA, col = "darkgray")
text(min(load1) * 1.05, 0, "X1", pos = 2, xpd = NA, col = "darkgray")


## ----ex_PCA_wines_rotated-----------------------------------------------------------------------------------------------------
theta <- 165 * pi / 180
basis_rot <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
  
new_x <- t(basis_rot %*% matrix(c(1, 0, -1, 0), 2, 2))
new_y <- t(basis_rot %*% matrix(c(0, 1, 0, -1), 2, 2))

par(mai = c(0.9, 0.9, 0.1, 0.1), omi = c(0, 1.5, 0, 1.5))

plot(load1, load2, pch = 16,
     xlim = c(min(load1), max(load1)),
     ylim = c(min(load2), max(load2)),
     xaxt = "n", yaxt = "n", bty = "n",
     xlab = "", ylab = "")
abline(h = 0, col = "darkgray")
abline(v = 0, col = "darkgray")
## new axes
lines(new_x[,1], new_x[,2], col = "blue")
lines(new_y[,1], new_y[,2], col = "blue")
## labels
for(i in 1:ncol(dat)) {
  text(load1[i], load2[i], colnames(dat)[i],
       pos = poz[i], xpd = NA)
}
text(0, min(load2) * 1.05, "X2", pos = 1, xpd = NA, col = "darkgray")
text(min(load1) * 1.05, 0, "X1", pos = 2, xpd = NA, col = "darkgray")

theta <- 345 * pi / 180
basis_rot <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
text((basis_rot %*% matrix(c(0,par()$usr[3]),2,1))[1], min(load2) * 1.05,
     "R2", pos = 1, xpd = NA, col = "blue")
text(min(load1) * 1.05, (basis_rot %*% matrix(c(par()$usr[1],0),2,1))[2],
     "R1", pos = 2, xpd = NA, col = "blue")



## ----ex_PCA_loadings----------------------------------------------------------------------------------------------------------
theta <- 15 * pi / 180
basis_rot <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
  
rot_loads <- t(basis_rot %*% t(loads[,1:2]))
rload1 <- rot_loads[,1]
rload2 <- rot_loads[,2]

  
par(mai = c(0.9, 0.9, 0.1, 0.1), omi = c(0, 1.5, 0, 1.5))

plot(load1, load2, pch = 16, col = "darkgray",
     xlim = c(min(load1,rload1), max(load1,rload1)),
     ylim = c(min(load2,rload2), max(load2,rload2)),
     xaxt = "n", yaxt = "n", bty = "n",
     xlab = "", ylab = "")
abline(h = 0, col = "darkgray")
abline(v = 0, col = "darkgray")
for(i in 1:ncol(dat)) {
  text(rload1[i], rload2[i], colnames(dat)[i],
       pos = poz[i], xpd = NA)
}
text(0, min(load2) * 1.05, "R2", pos = 1, xpd = NA, col = "darkgray")
text(min(load1) * 1.05, 0, "R1", pos = 2, xpd = NA, col = "darkgray")

points(rload1, rload2, pch = 16)

