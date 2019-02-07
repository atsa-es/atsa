

# get N Hemisphere land & ocean temperature anomalies from NOAA
ww1 <- "http://www.ncdc.noaa.gov/cag/time-series/"
ww2 <- "global/nhem/land_ocean/p12/12/1880-2014.csv"
Temp <- read.csv(paste(ww1,ww2,sep=""), skip=2)

Temp <- Temp[Temp$Year>196000,]


# create ts object
tmp <- ts(data=Temp$Value, frequency=12, start=c(1880,1))


dev.new(height=5,width=7)
par(mai=c(0.8,0.8,0.3,0.1), omi=c(0,0,0,0))
plot.ts(tmp, main="N Hemisphere temperature anomalies (C)", ylab="Temperature (C)")

dd <- decompose(tmp)

lines(dd$trend, col="red", lwd=2)


tmp.seas <- tmp - dd$trend

# length of ts
ll <- length(tmp.seas)
# frequency (ie, 12)
ff <- frequency(tmp.seas)
# number of periods (years); %/% is integer division
periods <- ll %/% ff
# index of cumulative month
index <- seq(1,ll,by=ff) - 1
# get mean by month
mm <- numeric(ff)
for(i in 1:ff) {
  mm[i] <- mean(tmp.seas[index+i], na.rm=TRUE)
}
# subtract mean to make overall mean=0
mm <- mm - mean(mm)


dev.new(height=6,width=7)
par(mai=c(0.8,0.8,0.3,0.1), omi=c(0,0,0,0))
plot(dd)


dev.new(height=5,width=7)
par(mai=c(0.8,0.8,0.3,0.1), omi=c(0,0,0,0))
plot(seq(12), mm, pch=16, type="o", xlab="Month", col="blue",
	 main="Seasonal effect on N Hemis temp anomalies", ylab="Temperature (C)")

tmp.ss <- ts(rep(mm, periods+1)[seq(ll)],
               start=start(tmp.seas), 
               frequency=ff)
lines(tmp.seas)





plot.acf <- function(ACFobj) {rr <- ACFobj$acf[-1]kk <- length(rr)nn <- ACFobj$n.usedplot(seq(kk),rr,type="h",lwd=2,yaxs="i",xaxs="i",ylim=c(floor(min(rr)),1),xlim=c(0,kk+1),xlab="Lag",ylab="Correlation",las=1)abline(h=-1/nn+c(-2,2)/sqrt(nn),lty="dashed",col="blue")abline(h=0)}

acf(tmp, lag.max=24, xaxt="n")
axis(1,at=(seq(0,24,2))/12, labels=seq(0,24,2))

pacf(tmp, lag.max=24, xaxt="n")
axis(1,at=(seq(2,24,2))/12, labels=seq(2,24,2))


dev.new(height=5.5,width=7)

par(mfrow=c(2,1), mai=c(0,0.8,0.3,0.1), omi=c(0.8,0,0,0))

plot.ts(tmp, main="N Hemisphere temperature anomalies (C)", ylab="Temperature (C)", xaxt="n", xlab="")

plot.ts(diff(tmp), ylab=expression(paste(symbol("\xd1"),"Temperature (C)")))
mtext(side=1, "Time", line=3, cex=1.2)





# L WA temperature

ll <- lakeWAplanktonTrans[,"Temp"]

mm <- mean(lakeWAplanktonRaw[,"Temp"],na.rm=TRUE)
sd <- sqrt(var(lakeWAplanktonRaw[,"Temp"],na.rm=TRUE))

lwa <- ts(ll*sd+mm,start=c(1962,1),freq=12)

dev.new(height=5.5,width=7)

par(mfrow=c(2,1), mai=c(0,0.8,0.3,0.1), omi=c(0.8,0,0,0))

plot.ts(lwa, main="L Washington Surface Temperature (C)", ylab="Temperature (C)", xaxt="n", xlab="")

plot.ts(diff(lwa,lag=12), ylab=expression(paste(symbol("\xd1"),"Temperature (C)")))
mtext(side=1, "Time", line=3, cex=1.2)






# get the matching years of sunspot data
suns <- ts.intersect(lynx,sunspot.year)[,"sunspot.year"]
# get the matching lynx data
lynx <- ts.intersect(lynx,sunspot.year)[,"lynx"]

dev.new(height=5,width=6)

par(mfrow=c(2,1), mai=c(0,0.8,0.3,0.1), omi=c(0.8,0,0,0))
plot.ts(suns, main="", ylab="Sunspot activity", xaxt="n", xlab="")

plot.ts(lynx, ylab="Number of trapped lynx")
mtext(side=1, "Time", line=3, cex=1.2)

dev.new(height=4,width=6)
par(mai=c(0.8,0.8,0.3,0.1))
ccf(log(suns), log(lynx), ylab="Cross-correlation", main="")
mtext(side=3, "Cross-correlation of log of sunspots (X) & lynx (Y)", line=0.5, font=2)



xx <- arima.sim(model=list(ar=0.5, sd=0.1), n=100)

yy <- cumsum(xx)

acf(yy)

arima(diff(yy), order=c(1,0,0), include.mean=FALSE)

acf(diff(yy))

par(mfrow=c(1,2))

# length of ts
nn <- 100

# trend only
dev.new(height=2.5, width=7)
par(mfrow=c(1,2), mai=c(0.8,0.9,0.1,0.1), omi=c(0,0,0,0))
plot.ts(yy, ylab=expression(italic(x[t])))
acf(yy)

dev.new(height=2.5, width=7)
par(mfrow=c(1,2), mai=c(0.8,0.9,0.1,0.1), omi=c(0,0,0,0))
plot.ts(diff(yy), ylab=expression(paste(symbol("\xd1"), italic(x[t]))))
acf(diff(yy))






