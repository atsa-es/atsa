## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(lubridate)
library(mgcv)


## ----tickdata---------------------------------------------------------------------------------------------------------------------------
# this data is from the neon tick github repo, 
# https://github.com/eco4cast/neon4cast-ticks
dat.raw <- readRDS("tck_longform.Rdata")
dat = dplyr::filter(dat.raw, Species_LifeStage == "IXOSP2_Larva") 
# only use sites 
dat = dplyr::filter(dat, decimalLongitude > -100, decimalLongitude < -80, 
                    decimalLatitude > 30, decimalLatitude < 40)

# sum up spp g
dat_summary = dplyr::group_by(dat, plotID, collectDate) %>% 
  dplyr::summarize(ticks = sum(IndividualCount),
                   lon = decimalLongitude[1],
                   lat = decimalLatitude[1])

# add in date
dat_summary$month = lubridate::month(dat_summary$collectDate)
dat_summary$year = lubridate::year(dat_summary$collectDate)
dat_summary$week = ceiling(lubridate::yday(dat_summary$collectDate)/7)

# filter out years 2016-2018, with most data
dat_summary = dplyr::filter(dat_summary, year %in% 2016:2018)
# create a presence-absence variable
dat_summary$presence <- ifelse(dat_summary$ticks > 0, 1, 0)


## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------
## acf(residuals(fit))
## pacf(residuals(fit))
## qqnorm(residuals(fit))


## ---------------------------------------------------------------------------------------------------------------------------------------
null_fit <- glm(presence ~ as.factor(year), 
                data = dat_summary, 
                family = "binomial")


## ---------------------------------------------------------------------------------------------------------------------------------------
fit <- gam(presence ~ as.factor(year) + s(week), 
           data = dat_summary, 
           family = "binomial")


## ---------------------------------------------------------------------------------------------------------------------------------------
spatial_fit <- gam(presence ~ as.factor(year) + s(week) + s(lon,lat), 
           data = dat_summary, 
           family = "binomial")


## ---------------------------------------------------------------------------------------------------------------------------------------
spatial_fit2 <- gam(presence ~ as.factor(year) + s(week) + 
                     s(lon,lat,by=as.factor(year)), 
           data = dat_summary, 
           family = "binomial", 
           control=list(maxit=1000))


## ---------------------------------------------------------------------------------------------------------------------------------------
spatial_fit3 <- gam(presence ~ as.factor(year) + s(week) + 
                     s(lon,lat) + 
                      s(lon,lat,as.factor(year),bs="fs",m=2), 
           data = dat_summary, 
           family = "binomial",
           control=list(maxit=1000))


## ---------------------------------------------------------------------------------------------------------------------------------------
d = read.csv("SNOTEL_Washington_1981_2013.csv")
d = d[,c("Station.Id","Station.Name","Water.Year","Feb")]
meta = read.csv("SNOTEL_metadata.csv")
d = dplyr::left_join(d, meta) %>% 
  dplyr::filter(!is.na(Water.Year), !is.na(Feb))


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------
## library(sdmTMB)
## mesh <- make_mesh(d, xy_cols = c("Longitude","Latitude"), n_knots = 50)


## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------
## plot(mesh)


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------
## fit <- sdmTMB(Feb ~ 1, spde = mesh, time = NULL, data = d)


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------
## fit <- sdmTMB(Feb ~ 1, spde = mesh, time = Water.Year, data = d)


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------
## fit <- sdmTMB(Feb ~ 1, spde = mesh, time = Water.Year, data = d, ar1_fields=TRUE)


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------
## install.packages("ape")
## library(ape)
## ?Morans.I


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------
## d$resid = residuals(fit)
## df = dplyr::filter(d,Water.Year==2003)


## ----eval =FALSE------------------------------------------------------------------------------------------------------------------------
## m <- as.matrix(dist(df[,c("Longitude","Latitude")]))
## m <- 1/m
## diag(m) <- 0
## Moran.I(df$resid, weight=m)

