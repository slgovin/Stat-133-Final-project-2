# =========================================================
# Stat133: Final Project - Visualization
# Names: Hanna Haddad and Swati Govindaraju
# SIDs: 22922603 and 22380774
# Date: 08/14/15
# Description: Storm Data Visualization 
# Data: IBTrACS Storm Data
# =========================================================

# ============================================
# Import Data
# ============================================

# install packages and load
library(maps)
library(mapproj)
library(ggplot2)
library(stringr)
library(RColorBrewer)

# download the files and save copies
urlEP <- 'ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.EP.ibtracs_wmo.v03r06.csv'
rawdfEP <- read.csv(urlEP, stringsAsFactors = FALSE, header = FALSE)
write.csv(rawdfEP, 'rawdata/Basin.EP.ibtracs_wmo.v03r06.csv')

urlNA <- 'ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.NA.ibtracs_wmo.v03r06.csv'
rawdfNA <- read.csv(urlNA, stringsAsFactors = FALSE, header = FALSE)
write.csv(rawdfNA, 'rawdata/Basin.NA.ibtracs_wmo.v03r06.csv')

# ============================================
#  Data Frame Cleaning
# ============================================

# remove first three rows
rawdfEP <- rawdfEP[-c(1:3), ]
rawdfNA <- rawdfNA[-c(1:3), ]

# preliminary analysis to determine necessary information
levels(factor(rawdfEP$V1)) # important - ID
levels(factor(rawdfEP$V2)) # important - years
levels(factor(rawdfEP$V3))  
levels(factor(rawdfEP$V4)) # important - basin
levels(factor(rawdfEP$V5))
levels(factor(rawdfEP$V6))
levels(factor(rawdfEP$V7)) # important - date & time
levels(factor(rawdfEP$V8))
levels(factor(rawdfEP$V9)) # important - latitude
levels(factor(rawdfEP$V10)) # important - longitude
levels(factor(rawdfEP$V11)) # important - wind speed
levels(factor(rawdfEP$V12))
levels(factor(rawdfEP$V13))
levels(factor(rawdfEP$V14))
levels(factor(rawdfEP$V15))
levels(factor(rawdfEP$V16))

# create month names vector
monthnames <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

# export and build necessary vectors 
EPid <- rawdfEP$V1
EPyears <- as.numeric(rawdfEP$V2)
EPmonthraw <- as.numeric(str_extract(str_extract(rawdfEP$V7, '-..-'), '\\d\\d'))
EPmonth <- monthnames[EPmonthraw]
EPdate <- as.Date(str_extract(rawdfEP$V7, '....-..-..'))
EPbasin <- str_trim(rawdfEP$V4)
EPlatitude <- as.numeric(rawdfEP$V10)
EPlongitude <- as.numeric(rawdfEP$V9)
EPwindspd <- as.numeric(rawdfEP$V11)

NAid <- rawdfNA$V1
NAyears <- as.numeric(rawdfNA$V2)
NAmonthraw <- as.numeric(str_extract(str_extract(rawdfNA$V7, '-..-'), '\\d\\d'))
NAmonth <- monthnames[NAmonthraw]
NAdate <- as.Date(str_extract(rawdfNA$V7, '....-..-..'))
NAbasin <- str_trim(rawdfNA$V4)
NAlatitude <- as.numeric(rawdfNA$V10)
NAlongitude <- as.numeric(rawdfNA$V9)
NAwindspd <- as.numeric(rawdfNA$V11)

# build the data frame
EPdf <- data.frame(EPid, EPyears, EPmonth, EPdate, EPbasin, EPlatitude, EPlongitude, EPwindspd)
NAdf <- data.frame(NAid, NAyears, NAmonth, NAdate, NAbasin, NAlatitude, NAlongitude, NAwindspd)

# rename the columns
colnames(EPdf) <- c('id', 'year', 'month', 'date', 'basin', 'latitude', 'longitude', 'wind')
colnames(NAdf) <- c('id', 'year', 'month', 'date', 'basin', 'latitude', 'longitude', 'wind')

# merge the data frames
basindf <- rbind(EPdf, NAdf)

# export the cleaned basin data frame
write.csv(basindf, 'data/basin.csv')

# ============================================
#  Map Creation
# ============================================

# remove extraneous points
basindf2 <- basindf[(basindf$latitude < -10 & basindf$latitude > -180), ]

# create map image
map.dat <- map_data(map(database= "world", ylim=c(15,90), xlim=c(-180, -40), fill = TRUE))

#  image number 1 - plot showing the trajectory of all the storms (1980 - 2010)
pdf('images/basinplot1.pdf')
ggplot(map.dat) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=NULL)) +
  geom_path(data = subset(basindf2, year >= 1980 & 2010 >= year), aes(x = latitude, y = longitude, col = wind, group = factor(id))) +
  scale_colour_gradientn(colours = brewer.pal(9, 'Reds')[c(2,7,8,9)]) +
  xlab('') + scale_x_continuous(breaks = NULL) +
  ylab('') + scale_y_continuous(breaks = NULL) +
  ggtitle('Storm Trajectory (1980-2010)') +
  theme_bw() + theme(plot.title = element_text(size = 20, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off()
  
png('images/basinplot1.png')
ggplot(map.dat) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=NULL)) +
  geom_path(data = subset(basindf2, year >= 1980 & 2010 >= year), aes(x = latitude, y = longitude, col = wind, group = factor(id))) +
  scale_colour_gradientn(colours = brewer.pal(9, 'Reds')[c(2,7,8,9)]) +
  xlab('') + scale_x_continuous(breaks = NULL) +
  ylab('') + scale_y_continuous(breaks = NULL) +
  ggtitle('Storm Trajectory (1980-2010)') +
  theme_bw() + theme(plot.title = element_text(size = 20, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
dev.off()

# image number 2 - plot showing the trajectory of storms per month [all years 1980-2010] (one facet per month)
pdf('images/basinplot2.pdf')
ggplot(map.dat) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=NULL)) +
  geom_path(data = subset(basindf2, year >= 1980 & 2010 >= year), aes(x = latitude, y = longitude, col = wind, group = factor(id))) +
  scale_colour_gradientn(colours = brewer.pal(9, 'Reds')[c(2,7,8,9)]) +
  xlab('') + scale_x_continuous(breaks = NULL) +
  ylab('') + scale_y_continuous(breaks = NULL) +
  ggtitle('Storm Trajectory (1980-2010)') +
  theme_bw() + theme(plot.title = element_text(size = 20, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~month)
dev.off()

png('images/basinplot2.png')
ggplot(map.dat) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=NULL)) +
  geom_path(data = subset(basindf2, year >= 1980 & 2010 >= year), aes(x = latitude, y = longitude, col = wind, group = factor(id))) +
  scale_colour_gradientn(colours = brewer.pal(9, 'Reds')[c(2,7,8,9)]) +
  xlab('') + scale_x_continuous(breaks = NULL) +
  ylab('') + scale_y_continuous(breaks = NULL) +
  ggtitle('Storm Trajectory (1980-2010)') +
  theme_bw() + theme(plot.title = element_text(size = 20, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~month)
dev.off()

# image number 3 - One plot showing the trajectory of storms in decade 1980s (one facet per year)
pdf('images/basinplot3.pdf')
ggplot(map.dat) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=NULL)) +
  geom_path(data = subset(basindf2, year >= 1980 & 1989 >= year), aes(x = latitude, y = longitude, col = wind, group = factor(id))) +
  scale_colour_gradientn(colours = brewer.pal(9, 'Reds')[c(2,7,8,9)]) +
  xlab('') + scale_x_continuous(breaks = NULL) +
  ylab('') + scale_y_continuous(breaks = NULL) +
  ggtitle('Storm Trajectory (1980s)') +
  theme_bw() + theme(plot.title = element_text(size = 20, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~year)
dev.off()

png('images/basinplot4.png')
ggplot(map.dat) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=NULL)) +
  geom_path(data = subset(basindf2, year >= 1980 & 1989 >= year), aes(x = latitude, y = longitude, col = wind, group = factor(id))) +
  scale_colour_gradientn(colours = brewer.pal(9, 'Reds')[c(2,7,8,9)]) +
  xlab('') + scale_x_continuous(breaks = NULL) +
  ylab('') + scale_y_continuous(breaks = NULL) +
  ggtitle('Storm Trajectory (1980s)') +
  theme_bw() + theme(plot.title = element_text(size = 20, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~year)
dev.off()

# image number 4 - One plot showing the trajectory of storms in decade 1990s (one facet per year)
pdf('images/basinplot4.pdf')
ggplot(map.dat) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=NULL)) +
  geom_path(data = subset(basindf2, year >= 1990 & 1999 >= year), aes(x = latitude, y = longitude, col = wind, group = factor(id))) +
  scale_colour_gradientn(colours = brewer.pal(9, 'Reds')[c(2,7,8,9)]) +
  xlab('') + scale_x_continuous(breaks = NULL) +
  ylab('') + scale_y_continuous(breaks = NULL) +
  ggtitle('Storm Trajectory (1990s)') +
  theme_bw() + theme(plot.title = element_text(size = 20, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~year)
dev.off()

png('images/basinplot4.png')
ggplot(map.dat) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=NULL)) +
  geom_path(data = subset(basindf2, year >= 1990 & 1999 >= year), aes(x = latitude, y = longitude, col = wind, group = factor(id))) +
  scale_colour_gradientn(colours = brewer.pal(9, 'Reds')[c(2,7,8,9)]) +
  xlab('') + scale_x_continuous(breaks = NULL) +
  ylab('') + scale_y_continuous(breaks = NULL) +
  ggtitle('Storm Trajectory (1990s)') +
  theme_bw() + theme(plot.title = element_text(size = 20, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~year)
dev.off()

# image number 5 - One plot showing the trajectory of storms in decade 2000s (one facet per year)
pdf('images/basinplot5.pdf')
ggplot(map.dat) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=NULL)) +
  geom_path(data = subset(basindf2, year >= 2000 & 2009 >= year), aes(x = latitude, y = longitude, col = wind, group = factor(id))) +
  scale_colour_gradientn(colours = brewer.pal(9, 'Reds')[c(2,7,8,9)]) +
  xlab('') + scale_x_continuous(breaks = NULL) +
  ylab('') + scale_y_continuous(breaks = NULL) +
  ggtitle('Storm Trajectory (2000s)') +
  theme_bw() + theme(plot.title = element_text(size = 20, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~year)
dev.off()

png('images/basinplot5.png')
ggplot(map.dat) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=NULL)) +
  geom_path(data = subset(basindf2, year >= 2000 & 2009 >= year), aes(x = latitude, y = longitude, col = wind, group = factor(id))) +
  scale_colour_gradientn(colours = brewer.pal(9, 'Reds')[c(2,7,8,9)]) +
  xlab('') + scale_x_continuous(breaks = NULL) +
  ylab('') + scale_y_continuous(breaks = NULL) +
  ggtitle('Storm Trajectory (2000s)') +
  theme_bw() + theme(plot.title = element_text(size = 20, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~year)
dev.off()
