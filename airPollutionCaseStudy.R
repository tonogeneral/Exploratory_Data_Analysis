#Caso de estudio Air Pollution
#Gabriel General Soto
# 02-01-2021 21:03

data <- read.table("./data/RD_501_88101_1999-0.txt", header = FALSE, sep = "|",comment.char = "#",na.strings = "")
dim(data)
str(data)
head(data)

cnames <- readLines("./data/RD_501_88101_1999-0.txt",1)
cnames <- strsplit(cnames,"|",fixed = TRUE)
cnames
#agrega puntos a los nombres de columnas que contienen espacios
names(data) <- make.names(cnames[[1]])
head(data)

x0 <- data$Sample.Value
str(x0)
summary(x0)
mean(is.na(x0))

data1 <- read.table("./data/RD_501_88101_2012-0.txt", header = FALSE, sep = "|",comment.char = "#",na.strings = "")
dim(data1)
names(data1) <- make.names(cnames[[1]])

x1 <- data1$Sample.Value

summary(x0)
summary(x1)

mean(is.na(x1))

negative <- x1 <0
str(negative)

dates <- data1$Date
dates <- as.Date(as.character(dates),"%Y%m%d")
str(dates)
hist(dates,"month")
hist(dates[negative],"month")



site0 <- unique(subset(data,State.Code == 36, c(County.Code,Site.ID)))
site1 <- unique(subset(data1,State.Code == 36, c(County.Code,Site.ID)))
head(site0)

site0 <- paste(site0[,1],site0[,2],sep = ".")
site1 <- paste(site1[,1],site1[,2],sep = ".")

both <- intersect(site0,site1)
both

data$county.site <- with(data, paste(County.Code, Site.ID, sep = "."))
data1$county.site <- with(data1, paste(County.Code, Site.ID, sep = "."))

cnt <- subset(data, State.Code == 36 & county.site %in% both)
cnt1 <- subset(data1, State.Code == 36 & county.site %in% both)
head(cnt)

split(cnt, cnt$county.site)
sapply(split(cnt,cnt$county.site), nrow)
sapply(split(cnt1,cnt1$county.site), nrow)

datasub <- subset(data, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
data1sub <- subset(data1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)

dim(datasub)
dim(data1sub)


dates1 <- datasub$Date
x1sub <- datasub$Sample.Value
dates1 <- as.Date(as.character(dates1),"%Y%m%d")

dates2 <- data1sub$Date
x2sub <- data1sub$Sample.Value
dates2 <- as.Date(as.character(dates2),"%Y%m%d") 
plot(dates1,x1sub)

par(mfrow = c(1,2), mar = c(4,4,2,1))
plot(dates1, x1sub, pch = 20)
abline(h = median(x1sub, na.rm = T))
plot(dates2, x2sub, pch =20)
abline(h = median(x2sub, na.rm = T))
range(x1sub,x2sub, na.rm = T)
rng <- range(x1sub,x2sub, na.rm = T)
par(mfrow = c(1,2))
plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm = T))
plot(dates2, x2sub, pch = 20, ylim = rng)
abline(h = median(x2sub, na.rm = T))


mn1 <- with(data, tapply(Sample.Value, State.Code,mean, na.rm = T))
mn2 <- with(data1, tapply(Sample.Value, State.Code,mean, na.rm = T))

summary(mn1)
summary(mn2)

d0 <- data.frame(state = names(mn1), mean = mn1)
d1 <- data.frame(state = names(mn2), mean = mn2)
head(d0)


mrg <- merge(d0, d1, by = "state" )
dim(mrg)
head(mrg)

#Estudiar
par(mfrow = c(1,1))
with(mrg, plot(rep(1999, 52), mrg[,2], xlim = c(1998,2013)))
with(mrg, points(rep(2012, 52), mrg[,3]))
segments(rep(1999,52), mrg[,2], rep(2012,52), mrg[,3])