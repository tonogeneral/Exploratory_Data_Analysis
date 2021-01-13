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
str(emYear)
emYear

names(emYear)

emi <- data.frame(year = names(emYear), pm25 = emYear)
emi$year <- as.numeric(emi$year)
emi$pm25 <- as.numeric(emi$pm25/10^6)


range(emi$pm25, na.rm = T)
rng <- range(emi$pm25, na.rm = T)

plot(emi$year, emi$pm25, main = "Total PM 2.5  emissions all sources",
     xlab = "Year", ylab = "PM2.5 Millions of tons", type = "o", lty = 1, lwd = 1, col = "red",xlim = c(1999,2008), ylim = rng) 
#and lines(x, y, lty = 2, lwd = 1)

dev.copy(png,file = "plot1.png")
dev.off()


# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot
# answering this question.


baltimore <- subset(NEI, fips == "24510")
dim(baltimore)
str(baltimore)





sum(baltimore$Emissions, na.rm = TRUE)

baltYear <- with(baltimore, tapply(Emissions, year,sum, na.rm = T))

names(baltYear)
baltYear

emiBalti <- data.frame(year = names(baltYear), pm25 = baltYear)
emiBalti$year <- as.numeric(emiBalti$year)
emiBalti$pm25 <- as.numeric(emiBalti$pm25)



rngBalti <- range(emiBalti$pm25, na.rm = T)

plot(emiBalti$year, emiBalti$pm25, main = "Total PM 2.5  emissions Baltimore",
     xlab = "Year", ylab = "PM2.5 Tons", type = "o", lty = 1, lwd = 1, col = "red",xlim = c(1999,2008), ylim = rngBalti) 


dev.copy(png,file = "plot2.png")
dev.off()


# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases in emissions from 1999–2008 
# for Baltimore City? Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

library(ggplot2)

NEI
baltpoint <- subset(baltimore, baltimore$type == "POINT")
baltpointYear <- with(baltpoint, tapply(Emissions, year,sum, na.rm = T))
emipoint <- data.frame(year = names(baltpointYear), pm25 = baltpointYear)
str(emipoint)
emipoint$year <- as.numeric(emipoint$year)
emipoint$pm25 <- as.numeric(emipoint$pm25)
range(emipoint$pm25, na.rm = T)
rngemipoint <- range(emipoint$pm25, na.rm = T)





baltnopoint <- subset(baltimore, baltimore$type == "NONPOINT")
baltnopointYear <- with(baltnopoint, tapply(Emissions, year,sum, na.rm = T))
eminopoint <- data.frame(year = names(baltnopointYear), pm25 = baltnopointYear)
eminopoint$year <- as.numeric(eminopoint$year)
eminopoint$pm25 <- as.numeric(eminopoint$pm25)


baltonroad <- subset(baltimore, baltimore$type == "ON-ROAD")
baltonroadYear <- with(baltonroad, tapply(Emissions, year,sum, na.rm = T))
emionroad <- data.frame(year = names(baltonroadYear), pm25 = baltonroadYear)
emionroad$year <- as.numeric(emionroad$year)
emionroad$pm25 <- as.numeric(emionroad$pm25)


baltnonroad <- subset(baltimore, baltimore$type == "NON-ROAD")
baltnonroadYear <- with(baltnonroad, tapply(Emissions, year,sum, na.rm = T))
eminonroad <- data.frame(year = names(baltnonroadYear), pm25 = baltnonroadYear)
eminonroad$year <- as.numeric(eminonroad$year)
eminonroad$pm25 <- as.numeric(eminonroad$pm25)


emipoint
eminopoint
emionroad
eminonroad



library(tidyr)


# df <- gather(baltimore, key = baltimore$type, value = baltimore$Emissions)
# ggplot(df, aes(x=year, y = Emissions, group = type, colour = type)) + 
#   geom_line()


library(ggplot2)
colors <- c("POINT" = "blue", "NOPOINT" = "green", "ONROAD" = "orange","NONROAD" = "magenta")
ggplot(emiBalti, aes(x=year), ylim(c(rngemipoint))) +
ggtitle("Baltimore City emissions by types of source") +
geom_line(aes(y = emipoint$pm25, color = "POINT"), size = 1.5)+
geom_line(aes(y = eminopoint$pm25, color = "NOPOINT"), size = 1.5) +
geom_line(aes(y = emionroad$pm25, color = "ONROAD"), size = 1.5)+
geom_line(aes(y = eminonroad$pm25, color = "NONROAD"), size = 1.5)+
  labs(x = "Year",
       y = "PM25",
       color = "Type") +
  scale_color_manual(values = colors)

dev.copy(png,file = "plot3.png")
dev.off()



#4. Across the United States, how have emissions from coal combustion-related
#sources changed from 1999–2008?



combu <- SCC[grep("Coal",SCC$Short.Name),]
nrow(combu)
nrow(distinct(combu,SCC))

dim(combu)


dim(combu)

coalPollution <- combu$SCC


coalNEI <- subset(NEI,SCC %in% coalPollution)
dim(coalNEI)


sum(coalNEI$Emissions, na.rm = TRUE)

coalYear <- with(coalNEI, tapply(Emissions, year,sum, na.rm = T))
str(coalYear)
coalYear

names(coalYear)

emicoal <- data.frame(year = names(coalYear), pm25 = coalYear)
emicoal$year <- as.numeric(emicoal$year)
emicoal$pm25 <- as.numeric(emicoal$pm25)


range(emicoal$pm25, na.rm = T)
rngcoal <- range(emicoal$pm25, na.rm = T)

plot(emicoal$year, emicoal$pm25, main = " US emissions from coal combustion-related sources",
     xlab = "Year", ylab = "PM2.5 coal tons", type = "o", lty = 1, lwd = 1, col = "red",xlim = c(1999,2008), ylim = rngcoal) 
#and lines(x, y, lty = 2, lwd = 1)

dev.copy(png,file = "plot4.png")
dev.off()


#5. How have emissions from motor vehicle sources changed from 1999–2008
#in Baltimore City?

motor <- SCC[grep("Vehicles",SCC$Short.Name),]
nrow(motor)
nrow(distinct(motor,SCC))



vehicleEmi <- motor$SCC

vehiBaltimore <- subset(baltimore,SCC %in% vehicleEmi)
dim(vehiBaltimore)


sum(vehiBaltimore$Emissions, na.rm = TRUE)


vehiBaltimoreYear <- with(vehiBaltimore, tapply(Emissions, year,sum, na.rm = T))
str(vehiBaltimoreYear)
vehiBaltimoreYear

names(vehiBaltimoreYear)

emiVehiBaltimore <- data.frame(year = names(vehiBaltimoreYear), pm25 = vehiBaltimoreYear)
emiVehiBaltimore$year <- as.numeric(emiVehiBaltimore$year)
emiVehiBaltimore$pm25 <- as.numeric(emiVehiBaltimore$pm25)


range(emiVehiBaltimore$pm25, na.rm = T)
rngemiVehiBaltimore <- range(emiVehiBaltimore$pm25, na.rm = T)

plot(emiVehiBaltimore$year, emiVehiBaltimore$pm25, main = " Baltimore emissions from motor vehicles sources",
     xlab = "Year", ylab = "PM2.5 tons", type = "o", lty = 1, lwd = 1, col = "red",xlim = c(1999,2008), ylim = rngemiVehiBaltimore) 
#and lines(x, y, lty = 2, lwd = 1)

dev.copy(png,file = "plot5.png")
dev.off()




#6. Compare emissions from motor vehicle sources in Baltimore City with emissions
#from motor vehicle sources in Los Angeles County, California fips == "06037".
#Which city has seen greater changes over time in motor vehicle emissions?


LA <- subset(NEI, fips == "06037")
dim(LA)
str(LA)


sum(LA$Emissions, na.rm = TRUE)

LAYear <- with(LA, tapply(Emissions, year,sum, na.rm = T))

names(LAYear)
LAYear

emiLA <- data.frame(year = names(LAYear), pm25 = LAYear)
emiLA$year <- as.numeric(emiLA$year)
emiLA$pm25 <- as.numeric(emiLA$pm25)



rngLA <- range(emiLA$pm25, na.rm = T)

# plot(emiLA$year, emiLA$pm25, main = "Total PM 2.5  emissions Los Angeles",
#      xlab = "Year", ylab = "PM2.5 Tons", type = "o", lty = 1, lwd = 1, col = "red",xlim = c(1999,2008), ylim = rngLA) 



baltYear
LAYear



library(ggplot2)
colors <- c("Baltimore" = "magenta", "Los Angeles" = "cyan")
ggplot(emiLA, aes(x=year), ylim(c(rngLA))) + 
  ggtitle("Comparisson by emissions Los Angeles vs Baltimore") +
  geom_line(aes(y = emiBalti$pm25, color = "Baltimore"), size = 1.5)+
  geom_line(aes(y = emiLA$pm25, color = "Los Angeles"), size = 1.5) +
  labs(x = "Year",
       y = "PM25",
       color = "City") +
  scale_color_manual(values = colors)


dev.copy(png,file = "plot6.png")
dev.off()
