##read file
power <- read.csv("~/household_power_consumption.txt", header = TRUE, sep = ";")
##since the current class of Date is factor, we need to convert it into Date format
power$Date = as.Date(as.character(power$Date), format = "%d/%m/%Y")
## now we find rows with the dates we want
powerset <- power[c(grep("2007-02-01",power$Date),grep("2007-02-02",power$Date)),]
## converting desired column into numeric from factor so we can plot
converteddata <-as.numeric(as.character(powerset$Global_active_power))
## plot the histogram with data, column color red,  title, and labels as desired
hist(converteddata, col="red",main = "Global Active Power", xlab ="Global Active Power(kilowatts)", ylab="frequency")
##save file as image
dev.copy(png, file = "plot1.png")
dev.off()