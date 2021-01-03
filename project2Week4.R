# Exploratory Data Analysis
# Course Project 2. Week 4
# Author: Gabriel General
# Date: 03-01-2021

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

dim(NEI)
str(NEI)
head(NEI)

dim(SCC)
str(SCC)
head(SCC)

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.

# 1. Sumar emisiones para cada estado

sum(NEI$Emissions, na.rm = TRUE)

emYear <- with(NEI, tapply(Emissions, year,sum, na.rm = T))
emYear <- as.Date(as.character(emYear),"%Y%m%d")
str(emYear)
emYear

names(emYear)

emi <- data.frame(year = names(emYear), pm25 = emYear)
emi$year <- as.numeric(emi$year)
emi$pm25 <- as.numeric(emi$pm25)/10^6

# barplot(emi$pm25)
# 
# par(mfrow = c(1,1), mar = c(4,4,2,1))
# barplot(emi$pm25,main = "Total PM 2.5  emissions all Sources",
#      xlab = "Year", ylab = "PM2.5 tons", ylim = c(20000,7500000))

# plot(emi$year,emi$pm25)

range(emi$pm25, na.rm = T)
rng <- range(emi$pm25, na.rm = T)

plot(emi$year, emi$pm25, main = "Total PM 2.5  emissions all Sources",
     xlab = "Year", ylab = "PM2.5 Million of tons", type = "b", lty = 1, lwd = 1, col = "red",xlim = c(1999,2008), ylim = rng) 
#and lines(x, y, lty = 2, lwd = 1)

dev.copy(png,file = "plot1.png")
dev.off()






