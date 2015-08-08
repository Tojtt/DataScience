##read file
power <- read.csv("~/household_power_consumption.txt", header = TRUE, sep = ";")
##since the current class of Date is factor, we need to convert it into Date format
power$Date = as.Date(as.character(power$Date), format = "%d/%m/%Y")
## now we find rows with the dates we want
powerset <- power[c(grep("2007-02-01",power$Date),grep("2007-02-02",power$Date)),]
## now we group the date and time together as the column Time
powerset$Time <- strptime(paste(powerset$Date,powerset$Time), format= "%Y-%m-%d %H:%M:%S")
## plot the curve with smooth line (type ="l") and labels
with(powerset, plot(powerset$Time, powerset$Global_active_power, type="l",xlab = "", ylab = "Global Active Power(kilowatts)"))
##save file as image
dev.copy(png, file = "plot2.png")
dev.off()