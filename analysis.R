# =========================================================
# Stat133: Final Project - Analysis
# Names: Hanna Haddad and Swati Govindaraju
# SIDs: 22922603 and 22380774
# Date: 08/14/15
# Description: Storm Analysis in the East Pacific and North Atlantic
# Data: Cleaned IBTrACS Storm Data
# =========================================================

# ============================================
# Load Packages 
# ============================================
library("dplyr")
library("ggplot2")
library("stringr")

# ============================================
# Read Cleaned Data from csv Files 
# ============================================
storms = read.csv("data/storms.csv", stringsAsFactors = FALSE)
tracks = read.csv("data/tracks.csv", stringsAsFactors = FALSE)

# ============================================
# Modify tracks to be from 1980-2010  
# ============================================
tracks = mutate(tracks, 
                year = factor(str_sub(as.character(tracks$date), 1, 4)))
tracks = mutate(tracks, 
                month = factor(str_sub(as.character(tracks$date), 6,7)))
tracks = filter(tracks, 
                as.numeric(as.character(year))>=1980 & as.numeric(as.character(year))<=2010)

# ============================================
# Analysis Per Year   
# ============================================

stormsPerYear_total = barplot(table(tracks$year)[130:160], 
        main = "Number of Storms per Year from 1980-2010", 
        xlab = "Year", 
        ylab = "Number of Storms",
        )

stormsPerYear_35 = barplot(table(select(filter(tracks, wind>=35), year))[130:160], 
        main = "Number of Storms per Year from 1980-2010 with wind >= 35 knots", 
        xlab = "Year", 
        ylab = "Number of Storms")

stormsPerYear_64 = barplot(table(select(filter(tracks, wind>=64), year))[130:160], 
        main = "Number of Storms per Year from 1980-2010 with wind >= 64 knots", 
        xlab = "Year", 
        ylab = "Number of Storms")

table(select(filter(tracks, wind>=96), year))
stormsPerYear_96 = barplot(table(select(filter(tracks, wind>=96), year))[130:160], 
        main = "Number of Storms per Year from 1980-2010 with wind >= 96 knots", 
        xlab = "Year", 
        ylab = "Number of Storms")

# ============================================
# Analysis Per Month  
# ============================================
table(tracks$month)
stormsPerMonth_total = barplot(table(tracks$month), 
        main = "Number of Storms per Month from 1980-2010", 
        xlab = "Month", 
        ylab = "Number of Storms")

table(select(filter(tracks, wind>=35), month))
stormsPerMonth_35 = barplot(table(select(filter(tracks, wind>=35), month)), 
        main = "Number of Storms per Month with wind >= 35 knots", 
        xlab = "Month", 
        ylab = "Number of Storms")

table(select(filter(tracks, wind>=64), month))
stormsPerMonth_64 = barplot(table(select(filter(tracks, wind>=64), month)), 
        main = "Number of Storms per Year with wind >= 64 knots", 
        xlab = "Month", 
        ylab = "Number of Storms")

table(select(filter(tracks, wind>=96), month))
stormsPerMonth_96 = barplot(table(select(filter(tracks, wind>=96), month)), 
        main = "Number of Storms per Year with wind >= 96 knots", 
        xlab = "Month", 
        ylab = "Number of Storms")

# ============================================
# Create Annual Average Number of Storms Table
# ============================================

table35 <- 
  table(tapply(tracks$wind, tracks$id, max))[3:15]

avg35 <- mean(table35)
std35 <- sd(table)

summary(table35)

# Avg Number of storms >= 35 knots
avg35 = 
  mean(
    table(select(
      
      , year))
    )
std35 = sd(table(select(filter(tracks, wind>=35), year)))

raw35 <- as.numeric(table(select(filter(tracks, wind>=35), year)))
raw35[raw35 == 0] <- NA
quartiles35 = summary(as.numeric(na.omit(raw35)))
q1_35 = as.numeric(quartiles35)[2]
q2_35 = as.numeric(quartiles35)[3]
q3_35 = as.numeric(quartiles35)[5]

# Avg Number of storms >= 64 knots
avg64 = mean(table(select(filter(tracks, wind>=64), year)))
std64 = sd(table(select(filter(tracks, wind>=64), year))) 

raw64 <- as.numeric(table(select(filter(tracks, wind>=64), year)))
raw64[raw64 == 0] <- NA
quartiles64 = summary(as.numeric(na.omit(raw64)))
q1_64 = as.numeric(quartiles64)[2]
q2_64 = as.numeric(quartiles64)[3]
q3_64 = as.numeric(quartiles64)[5]

# Avg Number of storms >= 96 knots
avg96 = mean(table(select(filter(tracks, wind>=96), year)))
std96 = sd(table(select(filter(tracks, wind>=96), year)))

raw96 <- as.numeric(table(select(filter(tracks, wind>=96), year)))
raw96[raw96 == 0] <- NA
quartiles96 = summary(as.numeric(na.omit(raw96)))
q1_96 = as.numeric(quartiles96)[2]
q2_96 = as.numeric(quartiles96)[3]
q3_96 = as.numeric(quartiles96)[5]

# Create data table: Annual Average Number of Storms
AnnualAvgNumberOfStorms = data.frame("Avg" = c(avg35, avg64, avg96), 
                                     "Std Dev"  = c(std35, std64, std96), 
                                     "25th" = c(q1_35, q1_64, q1_96), 
                                     "50th" = c(q2_35, q2_64, q2_96),
                                     "75th" = c(q3_35, q3_64, q3_96), 
                                     row.names = c("35 knots", "64 knots", "96 knots"))

# ============================================
# Regression Analysis
# ============================================

## 1.)
# Obtain mean windspeed and mean pressure for each storm
mean_press1 = tapply(tracks$press, tracks$id, mean)
mean_wind1 = tapply(tracks$wind, tracks$id, mean)

# Add everything to data frame
# Remove all rows where mean pressure = 0
reg1_df = 
  filter(
    data.frame("mean_press" = mean_press1, "mean_wind" = mean_wind1)[mean_press1 == 0] 
    , mean_press>0)

# Regression
reg1 = lm(mean_press1~mean_wind1, data = reg1_df)
reg1
# Plot 
reg1_plot = ggplot(reg1_df, aes(x = mean_press, y = mean_wind)) +
  geom_smooth(method = "lm") +
  geom_point() + 
  ggtitle("Linear Regression 1: Average Pressure and Average Windspeed") 

?lm
reg1_plot

# 2.)
# Obtain median windspeed and median pressure for each storm
med_press1 = tapply(tracks$press, tracks$id, median)
med_wind1 = tapply(tracks$wind, tracks$id, median)


# Add everything to data frame
# Remove all rows where median pressure = 0
reg2_df = data.frame(med_press = med_press1, 
                     med_wind = med_wind1)
reg2_df = filter(reg2_df, med_press>0)

# Regression
reg2 = lm(med_press~med_wind, data = reg2_df)

# Plot
reg2_plot = ggplot(reg2_df, aes(x = med_press, y = med_wind)) +
  geom_smooth(method = "lm") +
  geom_point() + 
  ggtitle("Linear Regression 2: Median Pressure and Median Windspeed") 

# ============================================
# Regression Analysis
# ============================================

# pdf
pdf("../images/stormsPerYear_total.pdf")
stormsPerYear_total
dev.off()

pdf("../images/stormsPerYear_35.pdf")
stormsPerYear_35
dev.off()

pdf("../images/stormsPerYear_64.pdf")
stormsPerYear_64
dev.off()

pdf("../images/stormsPerYear_96.pdf")
stormsPerYear_96
dev.off()

pdf("../images/stormsPerMonth_total.pdf")
stormsPerMonth_total
dev.off()

pdf("../images/stormsPerMonth_35.pdf")
stormsPerMonth_35
dev.off()

pdf("../images/stormsPerMonth_64.pdf")
stormsPerYear_64
dev.off()

pdf("../images/stormsPerMonth_96.pdf")
stormsPerYear_96
dev.off()

pdf("../images/reg1_plot.pdf")
reg1_plot
dev.off()

pdf("../images/reg2_plot.pdf")
reg2_plot
dev.off()

# png
png("../images/stormsPerYear_total.png")
stormsPerYear_total
dev.off()

png("../images/stormsPerYear_35.png")
stormsPerYear_35
dev.off()

png("../images/stormsPerYear_64.png")
stormsPerYear_64
dev.off()

png("../images/stormsPerYear_96.png")
stormsPerYear_96
dev.off()

png("../images/stormsPerMonth_total.png")
stormsPerMonth_total
dev.off()

png("../images/stormsPerMonth_35.png")
stormsPerMonth_35
dev.off()

png("../images/stormsPerMonth_64.png")
stormsPerYear_64
dev.off()

png("../images/stormsPerMonth_96.png")
stormsPerYear_96
dev.off()

png("../images/reg1_plot.png")
reg1_plot
dev.off()

png("../images/reg2_plot.png")
reg2_plot
dev.off()


