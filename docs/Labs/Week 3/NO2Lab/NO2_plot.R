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
# save(ECNNO2, ECNmeta, file="ECNNO2.RData")

load("ECNNO2.RData")

library(ggmap)
ylims=c(min(ECNmeta$Latitude)-1,max(ECNmeta$Latitude)+1)
xlims=c(min(ECNmeta$Longitude)-2,max(ECNmeta$Longitude)+2.2)
base = ggmap::get_map(location=c(xlims[1],ylims[1],xlims[2],ylims[2]), zoom=7, maptype="terrain-background")
map1 = ggmap::ggmap(base)
map1 + geom_point(data=ECNmeta, aes(x=Longitude, y=Latitude), color="blue", cex=2.5) +
  labs(x="Latitude", y="Longitude", title="ECN sites") +
  geom_text(data=ECNmeta, aes(Longitude, Latitude),
    label=ECNmeta$SiteCode, 
    nudge_x = 0.35, nudge_y = 0.35, size=2.75, angle=45,
    check_overlap = FALSE
  ) +
  theme_bw()


p <- ggplot(subset(ECNNO2, Year>2010), aes(Date, Value)) +
  geom_line(aes(color=TubeID)) +
  facet_wrap(~SiteCode) +
  ggtitle("N02 in UK")
p

p <- ggplot(ECNNO2, aes(Date, Value)) +
  geom_line(aes(color=TubeID)) +
  facet_wrap(~SiteCode) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("") + ylab("NO2 (micrograms/m3)") +
  ggtitle("N02 in UK")
p

p <- ggplot(subset(ECNNO2, TubeID=="E1" & Month%in%c("Jan", "Jul")), aes(Date, Value)) +
  geom_line(aes(color=factor(Month))) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("NO2 is higher in winter") +
  facet_wrap(~SiteCode)
p


p <- ggplot(subset(ECNNO2,  Month%in%c("Jul") ), aes(Date, log(Value))) +
  geom_line(aes(color=TubeID)) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~SiteCode)
p

# Winds https://earth.nullschool.net/#current/wind/surface/level/orthographic=-4.46,54.31,2778/loc=-6.015,54.437

