## set random seed
set.seed(123)


#### covariance example ####

## create dummy x set
xx <- runif(25, 0, 10)
yy <- 1 + 0.3*xx + rnorm(n = 25, mean = 0, sd = 1.5)

## create x-y plot
par(mai=c(1,1,0,0), omi=c(0,0,0.5,1))
plot(xx, yy, pch=16, asp=1, las = 1,
     cex=1.2, cex.lab=1.2, col="black",
     xlab=expression(italic(x)),
     ylab=expression(italic(y)),
     main="")

## add mean lines	
abline(h=mean(yy), lty="dashed")
abline(v=mean(xx), lty="dashed")

## add labels for means
mtext(side=3, line=0.4, at=mean(xx), expression(italic(m[x])))
mtext(side=4, line=0.5, at=mean(yy), expression(italic(m[y])), las=1)

## add quadrant labels
mtext(side=3, line=0.4, at=(mean(xx)+par()$usr[1])/2, expression((italic(x[i])-italic(m[x])) < 0))
mtext(side=3, line=0.4, at=(mean(xx)+par()$usr[2])/2, expression((italic(x[i])-italic(m[x])) > 0))
mtext(side=4, line=0.5, at=(mean(yy)+par()$usr[3])/2, expression((italic(y[i])-italic(m[y])) < 0), las=1)
mtext(side=4, line=0.5, at=(mean(yy)+par()$usr[4])/2, expression((italic(y[i])-italic(m[y])) > 0), las=1)

## get indices for data pairs with neg cov
negC <- (xx<mean(xx) & yy>mean(yy)) | (xx>mean(xx) & yy<mean(yy))

## overlay pos & neg cov values
points(xx[negC], yy[negC], pch="-", cex=2, col="darkred")
points(xx[!negC], yy[!negC], pch="+", cex=1.5, col="blue")


#### stationarity in the mean ####

## number of ts
nn <- 200

## length of ts
tt <- 40

## generate realizations (white noise)
ww <- matrix(rnorm(n = nn*tt), mean = tt, sd = nn)

## calculate the mean
mm <- apply(ww, 1, mean)

## plot the realizations & mean
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))

matplot(ww, type="l", lty="solid",  las = 1,
        ylab = expression(italic(x[t])), xlab = "Time",
        col = gray(0.5, 0.2))
points(rep(0,tt), pch = "-", col = "blue", cex=1.5)


#### single realization of a process ####

## plot it
par(mai = c(0.9,0.9,0.1,0.1), omi = c(0,0,0,0))

matplot(ww, type="l", lty="solid",  las = 1,
        ylab = expression(italic(x[t])), xlab = "Time",
        col = gray(0.5, 0.2))
lines(ww[,1], col = "blue", lwd = 2)


#### stationary AR(1) processes ####

par(mfrow = c(2,2), mai = c(0.7,0.4,0.2,0.1))

plot.ts(arima.sim(model = list(ar = 0.3, sd = 0.1), n = 100),
        las = 1, ylab = "")
mtext("A", 3, line = 0, adj = 0, xpd = NA)
plot.ts(arima.sim(model = list(ar = 0.9, sd = 0.1), n = 100),
        las = 1, ylab = "")
mtext("B", 3, line = 0, adj = 0, xpd = NA)
plot.ts(arima.sim(model = list(ar = -0.9, sd = 0.1), n = 100),
        las = 1, ylab = "")
mtext("C", 3, line = 0, adj = 0, xpd = NA)
plot.ts(arima.sim(model = list(ma = 0.1, sd = 0.1), n = 100),
        las = 1, ylab = "")
mtext("D", 3, line = 0, adj = 0, xpd = NA)


#### ACF schematic ####

## empty plot space
par(mai=c(1,1,0,0), omi=c(0.1,0.1,0.1,0.1))
plot(NA, NA, type="n", xlim=c(0,15), ylim=c(-1,1),
     xlab="", xaxt="n", ylab="", las = 1)
abline(h=0)
axis(side=1, at=seq(15), labels=FALSE)
axis(side=1, at=seq(0,15,5))
mtext(expression(paste("Lag ", (italic(k)))), side=1, line=3, cex=1.2)
mtext(expression(paste("ACF ", (italic(r[k])))), side=2, line=3, cex=1.2)

## ACF at lag = 0
lines(c(0,0), c(0,1), lwd=2, col="darkred")
text(x=1, y =1, expression(italic(r)[0] == 1), col="darkred")

## 95% confidence intervals
nn <- 30
alpha <- 0.05
ts.SD <- qnorm(1-alpha/2, 0, 1)/sqrt(nn)
abline(h=-ts.SD, lty="dashed", col="blue")
text(x=14, y=-0.55, expression(-frac(italic(z)[1-frac(alpha,2)], sqrt(italic(n)))), col="blue")
abline(h=ts.SD, lty="dashed", col="blue")
text(x=14, y=0.55, expression(+frac(italic(z)[1-frac(alpha,2)], sqrt(italic(n)))), col="blue")


#### example ACF's ####

## length of ts
nn <- 100

## linear trend (straight line)
par(mfrow=c(1,2), mai=c(1,1,0,0), omi=c(0.1,0.1,0.6,0.1))
tt <- seq(nn)
plot.ts(tt, ylab=expression(italic(x[t])))
acf(tt)
mtext("Linear trend {1,2,3,...,100}", outer=TRUE, line=1, cex=1.5)


## sine wave
par(mfrow=c(1,2), mai=c(1,1,0,0), omi=c(0.1,0.1,0.6,0.1))
## compute the 2 predictor variables
tt <- sin(2*pi*seq(nn)/12)
plot.ts(tt, ylab=expression(italic(x[t])))
acf(tt)
mtext("Discrete (monthly) sine wave", outer=TRUE, line=1, cex=1.5)


## linear trend + sine wave
par(mfrow=c(1,2), mai=c(1,1,0,0), omi=c(0.1,0.1,0.6,0.1))
## compute the 2 predictor variables
tt <- sin(2*pi*seq(nn)/12) - seq(nn)/50
plot.ts(tt, ylab=expression(italic(x[t])))
acf(tt, lag.max=30)
mtext("Linear trend + seasonal effect", outer=TRUE, line=1, cex=1.5)


## random sequence of numbers
par(mfrow=c(1,2), mai=c(1,1,0,0), omi=c(0.1,0.1,0.6,0.1))
# compute the 2 predictor variables
tt <- rep(floor(runif(nn/10,1,101)), times=10)
plot.ts(tt, ylab=expression(italic(x[t])))
acf(tt)
mtext("Sequence of 10 random numbers repeated 10 times", outer=TRUE, line=1, cex=1.5)


#### Lake Washington phytoplankton ####

## load MARSS & get data file
## install.packages("MARSS")
library(MARSS)
data(lakeWAplankton)
lwa <- lakeWAplanktonTrans
lwa <- lwa[lwa[,"Year"] >= 1975,]
lwa <- ts(lwa, start = c(1975,1), freq = 12)

## plot the ts
par(mai=c(1,1,0,0), omi=c(0.1,0.1,0.1,0.1))
plot.ts(lwa[,"Cryptomonas"], ylab=expression(log(italic(Cryptomonus))), las = 1)


## ACF
par(mai=c(1,1,0,0), omi=c(0.1,0.1,0.1,0.1))
acf(lwa[,"Cryptomonas"], na.action = na.pass, las = 1)


## PACF
par(mai=c(1,1,0,0), omi=c(0.1,0.1,0.1,0.1))
pacf(lwa[,"Cryptomonas"], na.action = na.pass, las = 1)


#### example of cross-correlation function (CCF)

## get the matching years of sunspot data
suns <- ts.intersect(lynx,sunspot.year)[,"sunspot.year"]
## get the matching lynx data
lynx <- ts.intersect(lynx,sunspot.year)[,"lynx"]

## plot data and CCF
layout(mat = matrix(c(1,1,2,2,0,3,3,0), 4, 2))

par(mai=c(0.1,0.6,0.1,0.3), omi=c(0.5,0,0,0))
plot.ts(suns, main="", ylab="Sunspot activity",
        xaxt="n", xlab="", cex.lab = 2)

## par(mai=c(0.6,0.5,0.1,0.1), omi=c(0,0,0,0))
plot.ts(lynx, ylab="Number of trapped lynx")

## par(mai=c(0.6,0.5,0,0.1))
ccf(log(lynx), log(suns), ylab="Cross-correlation", main="")


#### example ACF's for simple processes ####

## white noise; w_t ~ N(0, 1)
par(mfrow = c(1,2), mai = c(1.5,0.9,0.1,0.1), omi = c(0,0,0,0))
tt <- rnorm(n = 100)
plot.ts(tt, ylab = expression(italic(w[t])))
acf(tt)


## random walk; x_t = x_{t-1} + w_t with w_t ~ N(0, 1)

## option 1: via for() loop
wt <- xt <- rnorm(n = 100)
for(t in 2:100) {
  xt[t] <- xt[t-1] + wt[t]
}
par(mfrow = c(1,2), mai = c(1.5,0.9,0.1,0.1), omi = c(0,0,0,0))
plot.ts(xt, ylab = expression(italic(x[t])))
acf(xt)

## option 2: via cumulative sum
xt <- cumsum(wt)
par(mfrow = c(1,2), mai = c(1.5,0.9,0.1,0.1), omi = c(0,0,0,0))
plot.ts(xt, ylab = expression(italic(x[t])))
acf(xt)


#### first difference of linear trend ####

## create biased RW

## option 1: via for() loop
xt <- wt <- rnorm(n = 100)
bias <- 0.3
for(t in 2:100) {
  xt[t] <- bias + xt[t-1] + wt[t]
}

## first difference
dx <- diff(xt, lag = 1)
mean(dx)

par(mfrow = c(2,1), mai = c(0.5,0.8,0.1,0), omi=c(0,0,0,0))
## raw data
plot.ts(xt)
## first difference
plot.ts(dx, las = 1,
        ylab = expression(nabla~italic(x[t])))

## option 2: via cumsum()
wt <- rnorm(n = 100, mean = bias)
xt <- cumsum(wt)

## first difference
dx <- diff(xt, lag = 1)
mean(dx)

par(mfrow = c(2,1), mai = c(0.5,0.8,0.1,0), omi=c(0,0,0,0))
## raw data
plot.ts(xt, las = 1,
        ylab = expression(italic(x[t])))
## first difference
plot.ts(dx, las = 1,
        ylab = expression(nabla~italic(x[t])))


#### first difference of a seasonal ts ####

## L Washington temperature
xt <- lwa[,"Temp"]

## first difference
dx <- diff(xt, lag = 12)

par(mfrow = c(2,1), mai = c(0.5,0.8,0.1,0), omi=c(0,0,0,0))

## raw data
plot.ts(xt, las = 1,
        ylab = "Temperature")
## first difference
plot.ts(dx, las = 1,
        ylab = expression(nabla~Temperature))

