#archivo de prueba 222

data <- read.table("./data/RD_501_88101_1999-0.txt", header = FALSE, sep = "|",comment.char = "#",na.strings = "")
dim(data)
str(data)
head(data)

cnames <- readLines("./data/RD_501_88101_1999-0.txt",1)
cnames <- strsplit(cnames,"|",fixed = TRUE)





