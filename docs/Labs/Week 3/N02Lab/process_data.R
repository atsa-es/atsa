# # https://catalogue.ceh.ac.uk/documents/baf51776-c2d0-4e57-9cd3-30cd6336d9cf
# 
# dat <- read.csv("ECN_AN1.csv", stringsAsFactors = FALSE)
# dat$Date <- as.Date(dat$SDATE, "%d-%B-%y")
# dat$Year <- format(dat$Date, "%Y")
# dat$Mon <- format(dat$Date, "%b")
# 
# library(ggplot2)
# library(stringr)
# library(tidyr)
# library(reshape2)
# 
# a <- subset(dat, TUBEID %in% c("E1","E2","E3"))
# b <- spread(a, FIELDNAME, VALUE)
# b <- b[order(b$Date), ]
# bad <- c(103, 104, 107:120)
# b$NO2[b$Q1 %in% bad | b$Q2 %in% bad | b$Q3 %in% bad] <- NA
# 
# val <- tapply(b$NO2, list(b$Year, b$Mon, b$TUBEID, b$SITECODE), mean, na.rm=TRUE)
# dat.mon <- melt(data=val, value.name="NO2")
# colnames(dat.mon) <- c("Year", "Month", "TubeID", "SiteCode", "Value")
# dat.mon$Date <- as.Date(paste(1,dat.mon$Mon, dat.mon$Year), "%d %b %Y")
# dat.mon <- dat.mon[order(dat.mon$Date),]
# rownames(dat.mon) <- NULL
# ECNNO2 <- dat.mon
# 
# ECNmeta <- read.csv("ECN_meta.csv", stringsAsFactors = FALSE)
# ECNmeta <- ECNmeta[,c(1,2,5,6)]
# library(ggmap)
# ylims=c(min(ECNmeta$Latitude)-1,max(ECNmeta$Latitude)+1)
# xlims=c(min(ECNmeta$Longitude)-2,max(ECNmeta$Longitude)+2.2)
# base = ggmap::get_map(location=c(xlims[1],ylims[1],xlims[2],ylims[2]), zoom=7, maptype="terrain-background")
# map1 = ggmap::ggmap(base)
# map1 + geom_point(data=ECNmeta, aes(x=Longitude, y=Latitude), color="blue", cex=2.5) + 
#   labs(x="Latitude", y="Longitude", title="ECN sites") + 
#   theme_bw()
# 
# ECNNO2 <- ECNNO2[,c(1:5,6)]
# save(ECNNO2, ECNmeta, file="ECNNO2.RData")
