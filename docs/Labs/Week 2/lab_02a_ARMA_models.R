## ----ts-load-datasets, eval=FALSE----------------------------------------
load("CO2_data.RData")
load("NHemiTemp_data.RData")
load("hourly_phyto.RData")

## ----ts-loadpackages, warning=FALSE, message=FALSE, results='hide'-------
library(stats)
library(MARSS)
library(forecast)
library(datasets)

## ----ts-CO2data, eval=FALSE----------------------------------------------
## library(RCurl)
## ## get CO2 data from Mauna Loa observatory
## ww1 <- "ftp://aftp.cmdl.noaa.gov/products/"
## ww2 <- "trends/co2/co2_mm_mlo.txt"
## CO2fulltext <- getURL(paste0(ww1,ww2))
## CO2 <- read.table(text=CO2fulltext)[,c(1,2,5)]
## ## assign better column names
## colnames(CO2) <- c("year","month","ppm")
## save(CO2, CO2fulltext, file="CO2_data.RData")

## ----ts-temp-data, eval=FALSE--------------------------------------------
## library(RCurl)
## ww1 <- "https://www.ncdc.noaa.gov/cag/time-series/"
## ww2 <- "global/nhem/land_ocean/p12/12/1880-2014.csv"
## Temp <- read.csv(text=getURL(paste0(ww1,ww2)), skip=4)
## save(Temp, file="NHemiTemp_data.RData")

## CO2 time series
co2 <- ts(data=CO2$ppm, frequency=12,
          start=c(CO2[1,"year"],CO2[1,"month"]))

## plot the ts
plot.ts(co2, ylab=expression(paste("CO"[2]," (ppm)")))


## temperature time serires
temp.ts <- ts(data=Temp$Value, frequency=12, start=c(1880,1))

## intersection (only overlapping times)
datI <- ts.intersect(co2,temp.ts)
## dimensions of common-time data
dim(datI)

## union (all times)
datU <- ts.union(co2,temp.ts)
## dimensions of all-time data
dim(datU)

## plot both ts
plot(datI, main="", yax.flip=TRUE)


## weights for moving avg
fltr <- c(1/2,rep(1,times=11),1/2)/12


## estimate of trend
co2.trend <- filter(co2, filter=fltr, method="convo", sides=2)

## plot trend in CO2
plot.ts(co2.trend, ylab="Trend", cex=1)

## seasonal effect over time
co2.1T <- co2 - co2.trend

## plot seasonal effect plus error
plot.ts(co2.1T, ylab="Seasonal effect plus errors", xlab="Month", cex=1)


## get mean seasonal effect by month
## length of ts
ll <- length(co2.1T)
## frequency (ie, 12)
ff <- frequency(co2.1T)
## number of periods (years); %/% is integer division
periods <- ll %/% ff
## index of cumulative month
index <- seq(1,ll,by=ff) - 1
## get mean by month
mm <- numeric(ff)
for(i in 1:ff) {
  mm[i] <- mean(co2.1T[index+i], na.rm=TRUE)
}
## subtract mean to make overall mean=0
mm <- mm - mean(mm)

## plot the ts
plot.ts(mm, ylab="Seasonal effect", xlab="Month", cex=1)

## create ts object for season
co2.seas <- ts(rep(mm, periods+1)[seq(ll)],
               start=start(co2.1T), 
               frequency=ff)

## random errors over time
co2.err <- co2 - co2.trend - co2.seas

## plot the ts
plot(cbind(co2,co2.trend,co2.seas,co2.err), main="", yax.flip=TRUE)


## decomposition of CO2 data
co2.decomp <- decompose(co2)

## plot decomposed elements
plot(co2.decomp, yax.flip=TRUE)


## twice-difference the CO2 data
co2.D2 <- diff(co2, differences=2)

## plot the differenced data
plot(co2.D2, ylab=expression(paste(nabla^2,"CO"[2])))

## difference the differenced CO2 data
co2.D2D12 <- diff(co2.D2, lag=12)

## plot the newly differenced data
plot(co2.D2D12, ylab=expression(paste(nabla,"(",nabla^2,"CO"[2],")")))


## correlogram of the CO2 data
acf(co2, lag.max=36)

## better ACF plot
plot.acf <- function(ACFobj) {
  rr <- ACFobj$acf[-1]
  kk <- length(rr)
  nn <- ACFobj$n.used
  plot(seq(kk),rr,type="h",lwd=2,yaxs="i",xaxs="i",
       ylim=c(floor(min(rr)),1),xlim=c(0,kk+1),
       xlab="Lag",ylab="Correlation",las=1)
  abline(h=-1/nn+c(-2,2)/sqrt(nn),lty="dashed",col="blue")
  abline(h=0)
}

## acf of the CO2 data
co2.acf <- acf(co2, lag.max=36)

## new correlogram of the CO2 data
plot.acf(co2.acf)



## deterministic ts and ACF's

## length of ts
nn <- 100
## create straight line
tt <- seq(nn)

## set the margins & text size
par(mfrow=c(1,2), mar=c(4,4,1,1), oma=c(0,0,0,0), cex=1)
## plot line
plot.ts(tt, ylab=expression(italic(x[t])))
## plot ACF
plot.acf(line.acf)


## create sine wave
tt <- sin(2*pi*seq(nn)/12)
## get ACF
sine.acf <- acf(tt, plot=FALSE)

## set the margins & text size
par(mfrow=c(1,2), mar=c(4,4,1,1), oma=c(0,0,0,0), cex=1)
## plot line
plot.ts(tt, ylab=expression(italic(x[t])))
## plot ACF
plot.acf(sine.acf)


## create sine wave with trend
tt <- sin(2*pi*seq(nn)/12) - seq(nn)/50
## get ACF
sili.acf <- acf(tt, plot=FALSE)

## set the margins & text size
par(mfrow=c(1,2), mar=c(4,4,1,1), oma=c(0,0,0,0), cex=1)
## plot line
plot.ts(tt, ylab=expression(italic(x[t])))
## plot ACF
plot.acf(sili.acf)


## better PACF plot
plot.pacf <- function(PACFobj) {
  rr <- PACFobj$acf
  kk <- length(rr)
  nn <- PACFobj$n.used
  plot(seq(kk),rr,type="h",lwd=2,yaxs="i",xaxs="i",
       ylim=c(floor(min(rr)),1),xlim=c(0,kk+1),
       xlab="Lag",ylab="PACF",las=1)
  abline(h=-1/nn+c(-2,2)/sqrt(nn),lty="dashed",col="blue")
  abline(h=0)
}


## PACF of the CO2 data
co2.pacf <- pacf(co2, plot = FALSE)
## correlogram of the CO2 data
plot.pacf(co2.pacf)



## get the matching years of sunspot data
suns <- ts.intersect(lynx,sunspot.year)[,"sunspot.year"]
## get the matching lynx data
lynx <- ts.intersect(lynx,sunspot.year)[,"lynx"]

## plot both ts
plot(cbind(suns,lynx), main="", yax.flip=TRUE)

## NOTE: for ccf(x, y), lags are evaluated at x_{t+k} vs y_t
##       so it's often easier to use cor(x = y, y = x)

## CCF of sunspots and lynx
ccf(lynx, suns, ylab="Cross-correlation")



## WHITE NOISE

set.seed(123)
## random normal variates
GWN <- rnorm(n=100, mean=5, sd=0.2)
## random Poisson variates
PWN <- rpois(n=50, lambda=20)


## set the margins & text size
par(mar=c(4,4,1,1), oma=c(0,0,0,0), cex=1, mfrow=c(1,2))
## plot normal variates with mean
plot.ts(GWN)
abline(h=5, col="blue", lty="dashed")
## plot Poisson variates with mean
plot.ts(PWN)
abline(h=20, col="blue", lty="dashed")


## set the margins & text size
par(mar=c(4,4,1,1), oma=c(0,0,0,0), cex=1, mfrow=c(1,2))
## plot normal variates with mean
acf(GWN, main="", lag.max=20)
## plot Poisson variates with mean
acf(PWN, main="", lag.max=20)



## RW MODELS

## set random number seed
set.seed(123)
## length of time series
TT <- 100
## initialize {x_t} and {w_t}
xx <- ww <- rnorm(n=TT, mean=0, sd=1)
## compute values 2 thru TT
for(t in 2:TT) { xx[t] <- xx[t-1] + ww[t] }

## ACF of RW
xx.acf <- acf(xx, plot=FALSE)

## setup plot area
par(mfrow=c(1,2), mar=c(4,4,1,1), oma=c(0,0,0,0), cex=1)
## plot line
plot.ts(xx, ylab=expression(italic(x[t])))
## plot ACF
plot.acf(xx.acf)

## another method
## simulate RW
x2 <- cumsum(ww)

## setup plot area
par(mfrow=c(1,2), mar=c(4,4,1,1), oma=c(0,0,0,0), cex=1)
## plot 1st RW
plot.ts(xx, ylab=expression(italic(x[t])))
## plot 2nd RW
plot.ts(x2, ylab=expression(italic(x[t])))



set.seed(456)
## list description for AR(1) model with small coef
AR.sm <- list(order=c(1,0,0), ar=0.1, sd=0.1)
## list description for AR(1) model with large coef
AR.lg <- list(order=c(1,0,0), ar=0.9, sd=0.1)
## simulate AR(1)
AR1.sm <- arima.sim(n=50, model=AR.sm)
AR1.lg <- arima.sim(n=50, model=AR.lg)

## ----ts-plotAR1sims, eval=FALSE, echo=TRUE-------------------------------
## ## setup plot region
## par(mfrow=c(1,2))
## ## get y-limits for common plots
## ylm <- c(min(AR1.sm,AR1.lg), max(AR1.sm,AR1.lg))
## ## plot the ts
## plot.ts(AR1.sm, ylim=ylm,
##         ylab=expression(italic(x)[italic(t)]),
##         main=expression(paste(phi," = 0.1")))
## plot.ts(AR1.lg, ylim=ylm,
##         ylab=expression(italic(x)[italic(t)]),
##         main=expression(paste(phi," = 0.9")))

## ----ts-getPlotLims, eval=TRUE, echo=FALSE-------------------------------
## get y-limits for common plots
ylm <- c(min(AR1.sm,AR1.lg), max(AR1.sm,AR1.lg))

## ----ts-plotAR1contrast, eval=TRUE, echo=FALSE, fig=TRUE, fig.height=3, fig.cap='(ref:ts-plotAR1contrast)'----
## set the margins & text size
par(mfrow=c(1,2), mar=c(4,4,1.5,1), oma=c(0,0,0,0), cex=1)
## plot the ts
plot.ts(AR1.sm, ylim=ylm,
        ylab=expression(italic(x)[italic(t)]),
        main=expression(paste(phi," = 0.1")))
plot.ts(AR1.lg, ylim=ylm,
        ylab=expression(italic(x)[italic(t)]),
        main=expression(paste(phi," = 0.9")))

## ----ts-simAR1opps, echo=TRUE, eval=TRUE---------------------------------
set.seed(123)
## list description for AR(1) model with small coef
AR.pos <- list(order=c(1,0,0), ar=0.5, sd=0.1)
## list description for AR(1) model with large coef
AR.neg <- list(order=c(1,0,0), ar=-0.5, sd=0.1)
## simulate AR(1)
AR1.pos <- arima.sim(n=50, model=AR.pos)
AR1.neg <- arima.sim(n=50, model=AR.neg)

## ----ts-plotAR1oppsEcho, eval=FALSE, echo=TRUE---------------------------
## ## setup plot region
## par(mfrow=c(1,2))
## ## get y-limits for common plots
## ylm <- c(min(AR1.pos,AR1.neg), max(AR1.pos,AR1.neg))
## ## plot the ts
## plot.ts(AR1.pos, ylim=ylm,
##         ylab=expression(italic(x)[italic(t)]),
##         main=expression(paste(phi[1]," = 0.5")))
## plot.ts(AR1.neg,
##         ylab=expression(italic(x)[italic(t)]),
##         main=expression(paste(phi[1]," = -0.5")))

## ----ts-getPlotLimsOpps, eval=TRUE, echo=FALSE---------------------------
## get y-limits for common plots
ylm <- c(min(AR1.pos,AR1.neg), max(AR1.pos,AR1.neg))

## ----ts-plotAR1opps, eval=TRUE, echo=FALSE, fig=TRUE, fig.height=3, fig.cap='(ref:ts-plotAR1opps)'----
## set the margins & text size
par(mfrow=c(1,2), mar=c(4,4,1.5,1), oma=c(0,0,0,0), cex=1)
## plot the ts
plot.ts(AR1.pos, ylim=ylm,
        ylab=expression(italic(x)[italic(t)]),
        main=expression(paste(phi[1]," = 0.5")))
plot.ts(AR1.neg, ylim=ylm,
        ylab=expression(italic(x)[italic(t)]),
        main=expression(paste(phi[1]," = -0.5")))

## ----ts-ARpFail, eval=FALSE, echo=TRUE-----------------------------------
## arima.sim(n=100, model=list(order(2,0,0), ar=c(0.5,0.5)))

## ----ts-ARpSims, eval=TRUE, echo=TRUE------------------------------------
set.seed(123)
## the 4 AR coefficients
ARp <- c(0.7, 0.2, -0.1, -0.3)
## empty list for storing models
AR.mods <- list()
## loop over orders of p
for(p in 1:4) {
  ## assume SD=1, so not specified
  AR.mods[[p]] <- arima.sim(n=10000, list(ar=ARp[1:p]))
}

## ----ts-plotARpCompsEcho, eval=FALSE, echo=TRUE--------------------------
## ## set up plot region
## par(mfrow=c(4,3))
## ## loop over orders of p
## for(p in 1:4) {
##   plot.ts(AR.mods[[p]][1:50],
##           ylab=paste("AR(",p,")",sep=""))
##   acf(AR.mods[[p]], lag.max=12)
##   pacf(AR.mods[[p]], lag.max=12, ylab="PACF")
## }

## ----ts-plotARpComps, eval=TRUE, echo=FALSE, fig=TRUE, fig.height=8, fig.cap='(ref:ts-plotARpComps)'----
## set the margins & text size
par(mfrow=c(4,3), mar=c(4,4,0.5,0.5), oma=c(0,0,0,0), cex=1)
## loop over orders of p
for(p in 1:4) {
  plot.ts(AR.mods[[p]][1:50],ylab=paste("AR(",p,")",sep=""))
  acf(AR.mods[[p]], lag.max=12)
  pacf(AR.mods[[p]], lag.max=12, ylab="PACF")
}

## ----ts-simMA1opps, echo=TRUE, eval=TRUE---------------------------------
set.seed(123)
## list description for MA(1) model with small coef
MA.sm <- list(order=c(0,0,1), ma=0.2, sd=0.1)
## list description for MA(1) model with large coef
MA.lg <- list(order=c(0,0,1), ma=0.8, sd=0.1)
## list description for MA(1) model with large coef
MA.neg <- list(order=c(0,0,1), ma=-0.5, sd=0.1)
## simulate MA(1)
MA1.sm <- arima.sim(n=50, model=MA.sm)
MA1.lg <- arima.sim(n=50, model=MA.lg)
MA1.neg <- arima.sim(n=50, model=MA.neg)

## ----ts-plotMA1oppsEcho, eval=FALSE, echo=TRUE---------------------------
## ## setup plot region
## par(mfrow=c(1,3))
## ## plot the ts
## plot.ts(MA1.sm,
##         ylab=expression(italic(x)[italic(t)]),
##         main=expression(paste(theta," = 0.2")))
## plot.ts(MA1.lg,
##         ylab=expression(italic(x)[italic(t)]),
##         main=expression(paste(theta," = 0.8")))
## plot.ts(MA1.neg,
##         ylab=expression(italic(x)[italic(t)]),
##         main=expression(paste(theta," = -0.5")))

## ----ts-plotMA1opps, eval=TRUE, echo=FALSE, fig=TRUE, fig.height=3, fig.cap='(ref:ts-plotMA1opps)'----
## set the margins & text size
par(mfrow=c(1,3), mar=c(4,4,1.5,0.5), oma=c(0,0,0,0), cex=1)
## plot the ts
plot.ts(MA1.sm,
        ylab=expression(italic(x)[italic(t)]),
        main=expression(paste(theta," = 0.2")))
plot.ts(MA1.lg,
        ylab=expression(italic(x)[italic(t)]),
        main=expression(paste(theta," = 0.8")))
plot.ts(MA1.neg,
        ylab=expression(italic(x)[italic(t)]),
        main=expression(paste(theta," = -0.5")))

## ----ts-MAqSims, eval=TRUE, echo=TRUE------------------------------------
set.seed(123)
## the 4 MA coefficients
MAq <- c(0.7, 0.2, -0.1, -0.3)
## empty list for storing models
MA.mods <- list()
## loop over orders of q
for(q in 1:4) {
  ## assume SD=1, so not specified
  MA.mods[[q]] <- arima.sim(n=500, list(ma=MAq[1:q]))
}

## ----ts-plotMApCompsEcho, eval=FALSE, echo=TRUE--------------------------
## ## set up plot region
## par(mfrow=c(4,3))
## ## loop over orders of q
## for(q in 1:4) {
##   plot.ts(MA.mods[[q]][1:50],
##           ylab=paste("MA(",q,")",sep=""))
##   acf(MA.mods[[q]], lag.max=12)
##   pacf(MA.mods[[q]], lag.max=12, ylab="PACF")
## }

## ----ts-plotMApComps, eval=TRUE, echo=FALSE, fig=TRUE, fig.height=8, fig.cap='(ref:ts-plotMApComps)'----
## set the margins & text size
par(mfrow=c(4,3), mar=c(4,4,0.5,0.5), oma=c(0,0,0,0), cex=1)
## loop over orders of q
for(q in 1:4) {
  plot.ts(MA.mods[[q]][1:50],ylab=paste("MA(",q,")",sep=""))
  acf(MA.mods[[q]], lag.max=12)
  pacf(MA.mods[[q]], lag.max=12, ylab="PACF")
}

## ----ts-ARMAest, eval=TRUE, echo=TRUE------------------------------------
set.seed(123)
## ARMA(2,2) description for arim.sim()
ARMA22 <- list(order=c(2,0,2), ar=c(-0.7,0.2), ma=c(0.7,0.2))
## mean of process
mu <- 5
## simulated process (+ mean)
ARMA.sim <- arima.sim(n=10000, model=ARMA22) + mu
## estimate parameters
arima(x=ARMA.sim, order=c(2,0,2))

## ----ts-ARMAsearch1, eval=TRUE, echo=TRUE--------------------------------
## empty list to store model fits
ARMA.res <- list()
## set counter
cc <- 1
## loop over AR
for(p in 0:3) {
  ## loop over MA
  for(q in 0:3) {
    ARMA.res[[cc]] <- arima(x=ARMA.sim,order=c(p,0,q))
    cc <- cc + 1
  }
}
## get AIC values for model evaluation
ARMA.AIC <- sapply(ARMA.res,function(x) x$aic)
## model with lowest AIC is the best
ARMA.res[[which(ARMA.AIC==min(ARMA.AIC))]]

## ----ts-autoARIMA, eval=TRUE, echo=TRUE----------------------------------
## find best ARMA(p,q) model
auto.arima(ARMA.sim, start.p=0, max.p=3, start.q=0, max.q=3)

## ----ts-HW1_1, eval=FALSE, echo=TRUE-------------------------------------
## ## what day of 2014 is Dec 1st?
## dBegin <- as.Date("2014-12-01")
## dayOfYear <- (dBegin - as.Date("2014-01-01") + 1)

