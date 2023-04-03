#### setup, include = FALSE ####
knitr::opts_chunk$set(echo = FALSE)
set.seed(123)


#### example of Bernoulli white noise (WN) ####
par(mfrow = c(1, 2), mai = c(1.5, 0.9, 0.1, 0.1), omi = c(0, 0, 0, 0))
tt <- rbinom(100, 1, 0.5) * 2 - 1
plot.ts(tt, ylab = expression(italic(w[t])))
acf(tt)


#### example of Gaussian WN ####
par(mfrow = c(1, 2), mai = c(1.5, 0.9, 0.1, 0.1), omi = c(0, 0, 0, 0))
tt <- rnorm(100)
plot.ts(tt, ylab = expression(italic(w[t])))
acf(tt)


#### example of a random walk (RW) ####
par(mfrow = c(1, 2), mai = c(1.5, 0.9, 0.1, 0.1), omi = c(0, 0, 0, 0))
tt <- cumsum(rnorm(100))
plot.ts(tt, ylab = expression(italic(x[t])))
acf(tt)


#### example of a biased RW ####
par(mfrow = c(1, 2), mai = c(1.5, 0.9, 0.1, 0.1), omi = c(0, 0, 0, 0))
xx <- ww <- rnorm(100, 0, 4)
uu <- 1
for(t in 2:100) {
  xx[t] <- xx[t-1] + uu + ww[t]
}
plot.ts(xx, ylab = expression(italic(x[t])))
acf(xx)


#### example of a first-differenced biased RW ####

par(mfrow = c(1, 2), mai = c(1.5, 0.9, 0.1, 0.1), omi = c(0, 0, 0, 0))
plot.ts(diff(xx), ylab = expression(nabla~italic(x[t])))
abline(h = 1, lty = "dashed", col = "blue", lwd = 2)
acf(diff(xx))


#### examples of AR(p) models ####

## the 4 AR coefficients
ARp <- c(0.7, 0.2, -0.1, -0.3)
## empty list for storing models
AR_mods <- vector("list", 4L)

par(mfrow = c(2, 2), mai = c(0.7, 0.7, 0.3, 0.1), omi = c(0, 0, 0, 0))
## loop over orders of p
for(p in 1:4) {
  ## assume SD=1, so not specified
  AR_mods[[p]] <- arima.sim(n=50, list(ar=ARp[1:p]))
  plot.ts(AR_mods[[p]], las = 1,
          ylab = expression(italic(x[t])))
  mtext(side = 3, paste0("AR(",p,")"),
        line = 0.5, adj = 0)
}


#### AR(1) models with different signs ####

## list description for AR(1) model with small coef
AR_pos <- list(order = c(1, 0, 0), ar = 0.7, sd = 0.1)
## list description for AR(1) model with large coef
AR_neg <- list(order = c(1, 0, 0), ar = -0.7, sd = 0.1)
## simulate AR(1)
AR1_pos <- arima.sim(n = 500, model = AR_pos)
AR1_neg <- arima.sim(n = 500, model = AR_neg)

## get y-limits for common plots
ylm1 <- c(min(AR1_pos[1:50],AR1_neg[1:50]), max(AR1_pos[1:50],AR1_neg[1:50]))

## set the margins & text size
par(mfrow = c(1, 2), mai = c(0.8,0.8,0.3,0.2), oma = c(0, 0, 0, 0))
## plot the ts
plot.ts(AR1_pos[1:50], ylim = ylm1, las = 1,
        ylab = expression(italic(x)[italic(t)]),
        main = "")
mtext(side = 3, expression(paste(phi[1]," = 0.7")),
      line = 0.4, adj = 0)
plot.ts(AR1_neg[1:50], ylim = ylm1, las = 1,
        ylab = expression(italic(x)[italic(t)]),
        main = "")
mtext(side = 3, expression(paste(phi[1]," = -0.7")),
      line = 0.4, adj = 0)


#### AR(1) models with large & small coefs ####

## list description for AR(1) model with small coef
AR_bg <- list(order = c(1, 0, 0), ar = 0.9, sd = 0.1)
## list description for AR(1) model with large coef
AR_sm <- list(order = c(1, 0, 0), ar = 0.1, sd = 0.1)
## simulate AR(1)
AR1_bg <- arima.sim(n = 500, model = AR_bg)
AR1_sm <- arima.sim(n = 500, model = AR_sm)

## get y-limits for common plots
ylm2 <- c(min(AR1_bg[1:50],AR1_sm[1:50]), max(AR1_bg[1:50],AR1_sm[1:50]))

## set the margins & text size
par(mfrow = c(1, 2), mai = c(0.8,0.8,0.3,0.2), oma = c(0, 0, 0, 0))
## plot the ts
plot.ts(AR1_bg[1:50], ylim = ylm2, las = 1,
        ylab = expression(italic(x)[italic(t)]),
        main = "")
mtext(side = 3, expression(paste(phi[1]," = 0.9")),
      line = 0.4, adj = 0)
plot.ts(AR1_sm[1:50], ylim = ylm2, las = 1,
        ylab = expression(italic(x)[italic(t)]),
        main = "")
mtext(side = 3, expression(paste(phi[1]," = 0.1")),
      line = 0.4, adj = 0)


#### examples of an ACF for AR(p) models ####

## set the margins & text size
par(mfrow = c(2, 2), mai = c(0.8, 0.8, 0.3, 0.2), oma = c(0, 0, 0, 0))
## plot the ts
plot.ts(AR1_pos[1:50], ylim = ylm1, las = 1,
        ylab = expression(italic(x)[italic(t)]),
        main = "")
mtext(side = 3, expression(paste(phi[1]," = 0.7")),
      line = 0.4, adj = 0)
acf(AR1_pos, lag.max = 20, las = 1)
plot.ts(AR1_neg[1:50], ylim = ylm1, las = 1,
        ylab = expression(italic(x)[italic(t)]),
        main = "")
mtext(side = 3, expression(paste(phi[1]," = -0.7")),
      line = 0.4, adj = 0)
acf(AR1_neg, lag.max = 20, las = 1)


#### examples of an ACF for theoretical AR(p) models ####

## set the margins & text size
par(mfrow = c(2, 2), mai = c(0.8,0.8,0.3,0.2), oma = c(0, 0, 0, 0))
## plot the ts
plot.ts(AR1_bg[1:50], ylim = ylm2, las = 1,
        ylab = expression(italic(x)[italic(t)]),
        main = "")
mtext(side = 3, expression(paste(phi[1]," = 0.9")),
      line = 0.4, adj = 0)
acf(AR1_bg, lag.max = 20, las = 1)
plot.ts(AR1_sm[1:50], ylim = ylm2, las = 1,
        ylab = expression(italic(x)[italic(t)]),
        main = "")
mtext(side = 3, expression(paste(phi[1]," = 0.1")),
      line = 0.4, adj = 0)
acf(AR1_sm, lag.max = 20, las = 1)


#### example ACF's for AR(3) models ####

## set 3 AR coefficients
ARp3 <- list(c(0.7, 0.2, -0.1), c(-0.7, 0.2, 0.1))

expr <- list(expression(paste("AR(3) with ", phi[1], " = 0.7, ",
                              phi[2], " = 0.2, ", phi[3], " = -0.1")),
             expression(paste("AR(3) with ", phi[1], " = -0.7, ",
                              phi[2], " = 0.2, ", phi[3], " = 0.1")))

## empty list for storing models
AR3_mods <- vector("list", 2L)

par(mfrow = c(2,3), mai = c(0.7, 0.7, 0.3, 0.1), omi = c(0, 0, 0, 0))
## loop over orders of p
for(p in 1:2) {
  ## assume SD = 1, so not specified
  AR3_mods[[p]] <- arima.sim(n = 5000, list(ar = ARp3[[p]]))
  plot.ts(AR3_mods[[p]][1:50], las = 1,
          ylab = expression(italic(x[t])))
  acf(AR3_mods[[p]], lag.max = 20,
      las = 1, main = "")
  mtext(side = 3, expr[[p]],
        line = 0.5, adj = 0.5)
  pacf(AR3_mods[[p]], lag.max = 20,
       las = 1, main = "")
}


#### example PACF's for AR(3) models ####

## empty list for storing models
pacf_mods <- vector("list", 4L)

par(mfrow = c(2, 2), mai = c(0.7, 0.7, 0.3, 0.1), omi = c(0, 0, 0, 0))
## loop over orders of p
for(p in 1:4) {
  pacf_mods[[p]] <- arima.sim(n = 5000, list(ar = ARp[1:p]))
  pacf(pacf_mods[[p]], lag.max = 15,
       las = 1, main = "")
  mtext(side = 3, paste0("AR(",p,")"),
        line = 0.5, adj = 0)
}



#### example MA(q) models ####

## compare MA(1) & MA(2) with similar structure
MA1 <- arima.sim(n = 50, list(ma = c(0.7)))
MA2 <- arima.sim(n = 50, list(ma = c(-1, 0.7)))

par(mfrow = c(1, 2), mai = c(1,1,0.3,0.1), omi = c(0, 0, 0, 0))
## loop over orders of p
plot.ts(MA1, las = 1,
        ylab = expression(italic(x[t])))
mtext(side = 3,
      expression(MA(1):~italic(x[t])==~italic(w[t])+0.7~italic(w[t-1])),
      line = 0.5, adj = 0)
plot.ts(MA2, las = 1,
        ylab = expression(italic(x[t])))
mtext(side = 3,
      expression(MA(2):~italic(x[t])==~italic(w[t])-~italic(w[t-1])+0.7~italic(w[t-2])),
      line = 0.5, adj = 0)



#### example PACF's for MA(3) models ####

## set 3 AR coefficients
MAp3 <- list(c(0.7), c(-0.7, 0.2, 0.1))

expr <- list(expression(paste("MA(1) with ", theta[1], " = 0.7, ")),
             expression(paste("MA(3) with ", theta[1], " = -0.7, ",
                              theta[2], " = 0.2, ", theta[3], " = 0.1")))

## empty list for storing models
MA3_mods <- vector("list", 2L)

par(mfrow = c(2,3), mai = c(0.7, 0.7, 0.3, 0.1), omi = c(0, 0, 0, 0))
## loop over orders of p
for(p in 1:2) {
  ## assume SD = 1, so not specified
  MA3_mods[[p]] <- arima.sim(n = 50, list(ma = MAp3[[p]]))
  plot.ts(MA3_mods[[p]][1:50], las = 1,
          ylab = expression(italic(x[t])))
  acf(MA3_mods[[p]], lag.max = 20,
      las = 1, main = "")
  mtext(side = 3, expr[[p]],
        line = 0.5, adj = 0.5)
  pacf(MA3_mods[[p]], lag.max = 20,
       las = 1, main = "")
}


#### example PACF's for MA(q) models ####

## the 4 MA coefficients
MAq <- c(0.7, 0.2, -0.1, -0.3)

## empty list for storing models
acf_mods <- vector("list", 4L)

par(mfrow = c(2, 2), mai = c(0.7, 0.7, 0.3, 0.1), omi = c(0, 0, 0, 0))
## loop over orders of p
for(q in 1:4) {
  acf_mods[[p]] <- arima.sim(n = 5000, list(ma = MAq[1:q]))
  acf(acf_mods[[p]], lag.max = 15,
      las = 1, main = "")
  mtext(side = 3, paste0("MA(",q,")"),
        line = 0.5, adj = 0)
}


#### examples of ARMA(p,q) models ####

arma_mods <- vector("list", 4L)

## ARMA(3,1): phi[1] = 0.7, phi[2] = 0.2, phi[3] = -0.1, theta[1] = 0.5
arma_mods[[1]] <- arima.sim(list(ar = c(0.7, 0.2, -0.1), ma = c(0.5)), n = 5000)
## ARMA(2,2): phi[1] = -0.7, phi[2] = 0.2, theta[1] = 0.7, theta[2]= 0.2
arma_mods[[2]] <- arima.sim(list(ar = c(-0.7, 0.2), ma = c(0.7, 0.2)), n = 5000)
## ARMA(1,3): phi[1] = 0.7, theta[1] = 0.7, theta[2] = 0.2, theta[3] = 0.5
arma_mods[[3]] <- arima.sim(list(ar = c(0.7), ma = c(0.7, 0.2, 0.5)), n = 5000)
## ARMA(2,2): phi[1] = 0.7, phi[2] = 0.2, theta[1] = 0.7, theta[2] = 0.2
arma_mods[[4]] <- arima.sim(list(ar = c(0.7, 0.2), ma = c(0.7, 0.2)), n = 5000)

titles <- list(
  expression("ARMA(3,1): "*phi[1]*" = 0.7, "*phi[2]*" = 0.2, "*phi[3]*" = -0.1, "*theta[1]*" = 0.5"),
  expression("ARMA(2,2): "*phi[1]*" = -0.7, "*phi[2]*" = 0.2, "*theta[1]*" = 0.7, "*theta[2]*" = 0.2"),
  expression("ARMA(1,3): "*phi[1]*" = 0.7, "*theta[1]*" = 0.7, "*theta[2]*" = 0.2, "*theta[3]*" = 0.5"),
  expression("ARMA(2,2): "*phi[1]*" = 0.7, "*phi[2]*" = 0.2, "*theta[1]*" = 0.7, "*theta[2]*" = 0.2")
)

par(mfrow = c(2, 2), mai = c(0.7, 0.7, 0.3, 0.1), omi = c(0, 0, 0, 0))
## loop over orders of p
for(i in 1:4) {
  plot.ts(arma_mods[[i]][1:50], las = 1,
          main = "", ylab = expression(italic(x[t])))
  mtext(side = 3, titles[[i]],
        line = 0.5, adj = 0, cex = 0.8)
  
}


#### examples of ACF's for ARMA(p,q) models ####

par(mfrow = c(2, 2), mai = c(0.7, 0.7, 0.3, 0.1), omi = c(0, 0, 0, 0))
## loop over orders of p
for(i in 1:4) {
  acf(arma_mods[[i]][1:1000], las = 1,
      main = "")
  mtext(side = 3, titles[[i]],
        line = 0.5, adj = 0, cex = 0.8)
  
}


#### examples of PACF's for ARMA(p,q) models ####

par(mfrow = c(2, 2), mai = c(0.7, 0.7, 0.3, 0.1), omi = c(0, 0, 0, 0))
## loop over orders of p
for(i in 1:4) {
  pacf(arma_mods[[i]][1:1000], las = 1,
       main = "")
  mtext(side = 3, titles[[i]],
        line = 0.5, adj = 0, cex = 0.8)
  
}


#### example of an ARIMA(p,d,q) model ####

xx <- arima.sim(model = list(ar = 0.5, sd = 0.1), n = 100)

yy <- cumsum(xx)

par(mfrow = c(2, 2), mai = c(0.7, 0.7, 0.3, 0.1), omi = c(0, 0, 0, 0))

plot.ts(yy, las = 1,
        ylab = expression(italic(x[t])))
mtext(side = 3, "ARIMA(1,1,0)", line = 0.5, adj = 0, cex = 0.8)
acf(yy)


#### example of a first-differenced ARIMA(p,d,q) model  ####

par(mfrow = c(2, 2), mai = c(0.7, 0.7, 0.3, 0.1), omi = c(0, 0, 0, 0))

plot.ts(yy, las = 1,
        ylab = expression(italic(x[t])))
mtext(side = 3, "ARIMA(1,1,0)", line = 0.5, adj = 0, cex = 0.8)
acf(yy)

plot.ts(diff(yy), las = 1,
        ylab = expression(paste(symbol("\xd1"), italic(x[t]))))
mtext(side = 3, "ARMA(1,0)", line = 0.5, adj = 0, cex = 0.8)
acf(diff(yy))

