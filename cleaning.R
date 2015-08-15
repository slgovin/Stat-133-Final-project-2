# =========================================================
# Stat133: Final Project - Cleaning
# Names: Hanna Haddad and Swati Govindaraju
# SIDs: 22922603 and 22380774
# Date: 08/14/15
# Description: Storms and Tracks Data Frame Organization and Cleaning
# Data: IBTrACS Storm Data
# =========================================================

# ============================================
# Import Data
# ============================================

# install packages and load
library(XML)
library(stringr)

# download the file and save a copy
url <- 'ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/hurdat_format/basin/Basin.NA.ibtracs_hurdat.v03r06.hdat'
rawdata <- read.csv(url, stringsAsFactors = FALSE, header = FALSE)
write.csv(rawdata, 'rawdata/Basin.NA.ibtracs_hurdat.v03r06.hdat')

# ============================================
# Build Storms Data Frame
# ============================================

# build storms components
stormsid <- as.numeric(unlist(str_extract_all(str_extract_all(rawdata, 'SNBR=....'), '\\d?\\d?\\d?\\d')))
stormsdate <- as.Date(unlist(str_extract_all(rawdata, '../../....')), format = "%m/%d/%Y")
stormsdays <- as.numeric(unlist(str_extract_all(str_extract_all(rawdata, 'M=..'), '\\d?\\d')))
stormsname <- unlist(str_extract_all(unlist(str_extract_all(str_extract_all(rawdata, '../../...............................................'), '............XING')), '............'))

# build storms
storms <- data.frame(stormsid, stormsdate, stormsdays, stormsname)

# rename storms columns
colnames(storms) <- c('id','date','days','name')

# ============================================
# Build Tracks Data Frame
# ============================================

# isolate raw data
tracksraw <- unlist(str_extract_all(rawdata, '\\d{5}.\\d\\d/\\d\\d.{68}\\*'))

# separate and recombine the tracks raw data
tracksraw1 <- str_sub(tracksraw, start = 13, end = 29) # 0:00
tracksraw2 <- str_sub(tracksraw, start = 30, end = 46) # 6:00
tracksraw3 <- str_sub(tracksraw, start = 47, end = 63) # 12:00
tracksraw4 <- str_sub(tracksraw, start = 64, end = 80) # 18:00
rawdaily <- c(rbind(tracksraw1, tracksraw2, tracksraw3, tracksraw4))

# build tracks ID
tracksid <- rep(rep(stormsid, stormsdays), each = 4)

# build tracks dates
tracksmonthday <- rep(unlist(str_extract_all(unlist(str_extract_all(rawdata, '\\d{5}.\\d\\d/\\d\\d.{68}\\*')), '../..')), , each = 4)
tracksyear <- rep(rep(unlist(str_extract_all(str_extract_all(str_extract_all(rawdata, '\\d{5}.../../....'), '../../....'), '\\d{4}')), stormsdays), each = 4)
tracksdates <- as.Date(paste(tracksmonthday, '/', tracksyear, sep = ''), format = "%m/%d/%Y")

# build tracks period
tracksperiod <- rep(c('00h', '06h', '12h', '18h'), times = 12889)

# build tracks stage
rawstage <- str_sub(rawdaily, 17, 17)
tracksstage <- character(0)
for (i in 1:length(rawdaily)) {
  if(rawstage[i] == '*') {
    tracksstage[i] <- 'cyclone'
  } else if (rawstage[i] == 'E') {
      tracksstage[i] <- 'extratropical'
  } else if (rawstage[i] == 'S') {
        tracksstage[i] <- 'subtropical'
  }
}

# build tracks longitude
trackslongitude <- as.numeric(str_sub(rawdaily, 1, 3))*.1

# build tracks latitude
rawlat <- as.numeric(str_sub(rawdaily, 5, 7))*.1
degree <- str_sub(rawdaily, 4, 4)
trackslatitude <- numeric(0)
for (i in 1:length(rawdaily)) {
  if(degree[i] == '2') {
    trackslatitude[i] <- rawlat[i] - 160
  } else if (degree[i] == '3') {
      trackslatitude[i] <- rawlat[i] - 60
  } else if (degree[i] == '0') {
        trackslatitude[i] <- '0'
  }
}

trackslatitude <- as.numeric(trackslatitude)

# build tracks wind & press
trackswind <- as.numeric(str_sub(rawdaily, 10, 11))
trackspress <- as.numeric(str_sub(rawdaily, 13, 16))

# build tracks
tracks <- data.frame(tracksid, tracksdates, tracksperiod, tracksstage, trackslongitude, trackslatitude, trackswind, trackspress)

# rename tracks columns
colnames(tracks) <- c('id','date','period','stage', 'lat', 'long', 'wind', 'press')

# remove zero values from tracks
tracks <- na.omit(tracks[!(tracks$lat == 0 & tracks$long == 0 & tracks$wind == 0 & tracks$press == 0),])

# ============================================
# Export the Data Frames
# ============================================

write.csv(storms, file = 'data/storms.csv')
write.csv(tracks, file = 'data/tracks.csv')
