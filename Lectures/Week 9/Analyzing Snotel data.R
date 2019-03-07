meta = read.csv("SNOTEL_metadata.csv")
y = read.csv("SNOTEL_Washington_1981_2013.csv")

y$matches = match(y$Station.Name,meta$Station.Name)
y = y[-which(is.na(y$matches)),]
y$lat = meta$Latitude[y$matches]
y$lon = meta$Longitude[y$matches]
y$elev = meta$Elevation[y$matches]

# Let's only use Feb SWE
unique(y$Station.Id)
y$Feb

y = y[which(y$lon < -120),]
y = y[which(y$lon > -122.5),]
#y = y[which(is.na(y$Feb)==F)]

subcoords = matrix(0, length(unique(y$Station.Id)),2)
for(i in 1:length(unique(y$Station.Id))) {
  indx = which(y$Station.Id == unique(y$Station.Id)[i])[1]
  subcoords[i,1:2] = cbind(y$lon[indx],y$lat[indx])  
}

library(PBSmapping)
data(nepacLL)
plotMap(nepacLL, ylim=c(min(y$lat)-0.2,max(y$lat)+0.2),xlim=c(min(y$lon)-0.2,max(y$lon)+0.2),col="grey70")
points(y$lon,y$lat,col="tomato1",cex=0.7)

# Fit the gls () models to the data
y$lat = jitter(y$lat)
y$lon = jitter(y$lon)
mod.exp = gls(Feb ~ elev, correlation=corExp(form=~lat+lon,nugget=T),data=y[which(is.na(y$Feb)==F & y$Water.Year==2013),])

mod.gaus = gls(Feb ~ elev, correlation=corGaus(form=~lat+lon,nugget=T),data=y[which(is.na(y$Feb)==F & y$Water.Year==2013),])

var.exp <- Variogram(mod.exp, form =~ lat+lon)
plot(var.exp,main="Exponential",ylim=c(0,1))
var.gaus <- Variogram(mod.gaus, form =~ lat+lon)
plot(var.gaus,main="Gaussian",ylim=c(0,1))

library(spBayes)

y = y[which(is.na(y$Feb)==F & y$Water.Year==2013),]
X = cbind(1,y$elev)
cords = cbind(y$lat,y$lon)
y = y$Feb

priors.1 <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
                 "phi.Unif"=c(3/1, 3/0.1), "sigma.sq.IG"=c(2, 2),
                 "tau.sq.IG"=c(2, 0.1))
starting <- list("phi"=3/0.5, "sigma.sq"=50, "tau.sq"=1)
tuning <- list("phi"=0.1, "sigma.sq"=0.1, "tau.sq"=0.1)
m.1 <- spLM(y~X-1, coords=cords,n.samples=10000,cov.model = "exponential",priors=priors.1,tuning=tuning,starting=starting)



m.1 <- spLM(y~X-1, coords=coords,n.samples=1000)


burn.in <- 0.5*n.samples

##recover beta and spatial random effects
burn.in <- 5000
m.1 <- spRecover(m.1, start=burn.in, verbose=FALSE)

par(mfrow = c(1,2),mai=c(0.5,0.5,0.1,0.1))
hist(m.1$p.beta.recover.samples[,1],col="grey",main="Coefficient")
lines(c(mod.exp$coefficients[1],mod.exp$coefficients[1]),c(0,1.0e6),col="red",lwd=2)
hist(m.1$p.beta.recover.samples[,2],col="grey",main="Elevation")
lines(c(mod.exp$coefficients[2],mod.exp$coefficients[2]),c(0,1.0e6),col="red",lwd=2)

par(mfrow = c(2,2),mai=c(0.5,0.5,0.1,0.1))
plot(m.1$p.beta.recover.samples[,1],xlab="",ylab="Intercept",main="")
plot(m.1$p.beta.recover.samples[,2],xlab="",ylab="Elevation",main="")

library(INLA)

mesh1 = inla.mesh.2d(subcoords,max.edge = 0.2)
plot(mesh1)
points(subcoords,col="red",lwd=3)

mesh1 = inla.mesh.2d(subcoords,max.edge = c(0.2,0.2),cutoff=0.1)
plot(mesh1)
points(subcoords,col="red",lwd=3)

mesh1 = inla.mesh.2d(subcoords,max.edge = c(0.5,1),cutoff=0.1)
plot(mesh1)
points(subcoords,col="red",lwd=3)
spde=inla.spde2.matern(mesh1, alpha=3/2)
y$stationNum = as.numeric(as.factor(as.character(y$Station.Name)))
n= max(y$stationNum)
k = 33
z = matrix(NA, n,k)
yrmat = matrix(NA,n,k)
yrmat01 = matrix(0,n,k)
elev = matrix(NA,n,k)
for(i in 1:dim(y)[1]) {
  z[y$stationNum[i],y$Water.Year[i]-1980]=y$Feb[i]
  yrmat[y$stationNum[i],y$Water.Year[i]-1980] = y$Water.Year[i]
  yrmat01[y$stationNum[i],y$Water.Year[i]-1980] = 1
  elev[y$stationNum[i],y$Water.Year[i]-1980] = y$elev[i]
}

dat <- data.frame(y=as.vector((z)), time=rep(1:k, each=n), xcoo=rep(subcoords[,1], k),ycoo=rep(subcoords[,2], k), elev = as.vector(elev), year=as.vector(yrmat))#[which(is.na(as.vector(y))==F),]




iset = inla.spde.make.index("i", n.spde=mesh1$n)

# Make the covariates
X.1 = dat[,5:6]
Covar.names <- colnames(X.1)
XX.list <- as.list(X.1)
effect.list <- list()  					
effect.list[[1]] <- c(iset, list(Intercept=1))
for (i in 1:ncol(X.1)) effect.list[[i+1]] <- XX.list[[i]]
names(effect.list) <- c("1", Covar.names)

A <- inla.spde.make.A(mesh=mesh1, loc=cbind(dat$xcoo, dat$ycoo))
A.list = list()
A.list[[1]] = A
for (i in 1:ncol(X.1)) A.list[[i+1]] <- 1
sdat <- inla.stack(tag='stdata', data=list(y=dat$y), A=A.list, effects=effect.list)

h.spec <- list(theta=list(initial=0.7, param=c(0, 5)))
#formulae <- y ~ 0+f(i, model=spde, group=i.group,control.group=list(model='ar1', hyper=h.spec))

formula2 = as.formula(paste0("y ~ -1 + Intercept +",  paste(Covar.names, collapse="+"), "+ f(i, model=spde)"))		# field evolves with AR1 by year
res2pos <- inla(formula2, family = "gaussian", data=inla.stack.data(sdat),control.predictor=list(compute=TRUE, A=inla.stack.A(sdat)), verbose = TRUE, debug=TRUE, keep=FALSE,control.compute = list(dic=TRUE, cpo=TRUE), control.fixed = list(correlation.matrix=TRUE))

stepsize <- 4*1/111
nxy <- round(c(diff(range(subcoords[,1])), diff(range(subcoords[,2])))/stepsize)
projgrid <- inla.mesh.projector(mesh1, xlim=range(subcoords[,1]),ylim=range(subcoords[,2]), dims=nxy)

xmean <- list()
for (j in 1:1)
  xmean[[j]] <- inla.mesh.project(
    projgrid, res2pos$summary.random$i$mean[iset$i.group==j])
do.call(function(...) grid.arrange(..., nrow=1),
        lapply(xmean, levelplot, xlab='', ylab='',
               col.regions=topo.colors(16), scale=list(draw=FALSE)))



dat <- data.frame(y=as.vector((z)), time=rep(1:k, each=n), xcoo=rep(subcoords[,1], k),ycoo=rep(subcoords[,2], k), elev = as.vector(elev))#[which(is.na(as.vector(y))==F),]
daty = matrix(0, dim(dat)[1],k)
for(i in 1:k) {
  indx = which(dat$time==i)
  daty[indx,i] = 1
}
dat[paste("Y",seq(1,k),sep="")]=NA
dat[,-c(1:5)]=daty


iset <- inla.spde.make.index('i', n.spde=spde$n.spde, n.group=k)

# Make the covariates
X.1 = dat[,5:38]
Covar.names <- colnames(X.1)
XX.list <- as.list(X.1)
effect.list <- list()  					
effect.list[[1]] <- c(iset, list(Intercept=1))
for (i in 1:ncol(X.1)) effect.list[[i+1]] <- XX.list[[i]]
names(effect.list) <- c("1", Covar.names)

A <- inla.spde.make.A(mesh=mesh1, loc=cbind(dat$xcoo, dat$ycoo),group = dat$time)
A.list = list()
A.list[[1]] = A
for (i in 1:ncol(X.1)) A.list[[i+1]] <- 1
sdat <- inla.stack(tag='stdata', data=list(y=dat$y), A=A.list, effects=effect.list)

h.spec <- list(theta=list(initial=0.7, param=c(0, 5)))
#formulae <- y ~ 0+f(i, model=spde, group=i.group,control.group=list(model='ar1', hyper=h.spec))

formula1 = as.formula(paste0("y ~ -1 + Intercept +",  paste(Covar.names, collapse="+"), "+ f(i, model=spde, group=i.group, control.group=list(model='ar1'))"))		# field evolves with AR1 by year

sdat <- inla.stack(tag='stdata', data=list(y=dat$y), A=A.list, effects=effect.list)
res1pos <- inla(formula1, family = "gaussian", data=inla.stack.data(sdat),control.predictor=list(compute=TRUE, A=inla.stack.A(sdat)), verbose = TRUE, debug=TRUE, keep=FALSE,control.compute = list(dic=TRUE, cpo=TRUE,config=TRUE), control.fixed = list(correlation.matrix=TRUE))

stepsize <- 4*1/111
nxy <- round(c(diff(range(subcoords[,1])), diff(range(subcoords[,2])))/stepsize)
projgrid <- inla.mesh.projector(mesh1, xlim=range(subcoords[,1]),ylim=range(subcoords[,2]), dims=nxy)

xmean <- list()
for (j in 1:16)
  xmean[[j]] <- inla.mesh.project(
    projgrid, res1pos$summary.random$i$mean[iset$i.group==j])


require(gridExtra)
require(splancs)
require(lattice)
do.call(function(...) grid.arrange(..., nrow=4),
        lapply(xmean, levelplot, xlab='', ylab='',
               col.regions=topo.colors(16), scale=list(draw=FALSE)))