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
## plot Time with any sub metering is fine
with(powerset, plot(powerset$Time, powerset$Sub_metering_1, type="l",xlab = "", ylab = "Energy sub metering"))
## add lines into plot
lines(powerset$Time, powerset$Sub_metering_2, col="red")
lines(powerset$Time, powerset$Sub_metering_3, col="blue")
## add legend
legend("topright", c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"), lty = c(1,1,1),lwd = c(2.5,2.5,2.5), col = c("black", "red","blue"))
##save file as image
dev.copy(png, file = "plot3.png")
dev.off()