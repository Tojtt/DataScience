##read file
power <- read.csv("~/household_power_consumption.txt", header = TRUE, sep = ";")
##since the current class of Date is factor, we need to convert it into Date format
power$Date = as.Date(as.character(power$Date), format = "%d/%m/%Y")
## now we find rows with the dates we want
powerset <- power[c(grep("2007-02-01",power$Date),grep("2007-02-02",power$Date)),]
## now we group the date and time together as the column Time
powerset$Time <- strptime(paste(powerset$Date,powerset$Time), format= "%Y-%m-%d %H:%M:%S")
## now we convert the data in other columns into numeric
powerset[,3:9] <- apply(powerset[,3:9],2, as.character)
powerset[,3:9] <- apply(powerset[,3:9],2, as.numeric)
##adjust the graph space so we can plot 4 graphs
par(mfrow = c(2,2))
with(powerset, plot(powerset$Time, powerset$Global_active_power, type="l",xlab = "", ylab = "Global Active Power(kilowatts)"))
with(powerset, plot(powerset$Time, powerset$Voltage, type="l",xlab = "datetime", ylab = "Voltage"))
with(powerset, plot(powerset$Time, powerset$Sub_metering_1, type="l",xlab = "", ylab = "Energy sub metering"))
lines(powerset$Time, powerset$Sub_metering_2, col="red")
lines(powerset$Time, powerset$Sub_metering_3, col="blue")
legend("topright","(x,y)", c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"),bty="n", lty = c(1,1,1),lwd = c(2.5,2.5,2.5), col = c("black", "red","blue",box.lwd =0))
with(powerset, plot(powerset$Time, powerset$Global_reactive_power, type="l",xlab = "datetime", ylab = "Global_reactive_power"))
##save file as image
dev.copy(png, file = "plot4.png")
dev.off()
