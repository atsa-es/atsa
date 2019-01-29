library(here)
labdir <- here("Labs/Week 1")

## library(datasets)

## load datasets
load(file.path(labdir,"CO2_data.RData"))
load(file.path(labdir,"NHemiTemp_data.RData"))
load(file.path(labdir,"hourly_phyto.RData"))

## CO2 time series
co2 <- ts(data=CO2$ppm, frequency=12,
          start=c(CO2[1,"year"],CO2[1,"month"]))

## plot the ts
plot.ts(co2, ylab=expression(paste("CO"[2]," (ppm)")))


## temperature time serires
temp.ts <- ts(data=Temp$Value, frequency=12, start=c(1880,1))


## union (all times)
datU <- ts.union(co2,temp.ts)
## dimensions of all-time data
dim(datU)
## plot both ts
plot(datU, main="", yax.flip=TRUE)

## intersection (only overlapping times)
datI <- ts.intersect(co2,temp.ts)
## dimensions of common-time data
dim(datI)
## plot both ts
plot(datI, main="", yax.flip=TRUE)



## DECOMPOSITION

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




## ACF & PACF

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



## CCF

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

