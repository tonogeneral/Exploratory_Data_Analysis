par(mfrow = c(1,1))

# data2 <- data1 %>%
#   filter(!is.na(data1$Date) & !is.na(data1$Global_active_power))


with(data1,plot(Date,Global_active_power,type = "l"))

dev.copy(png,file = "plot2.png")
dev.off()
