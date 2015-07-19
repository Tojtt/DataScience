pollutantmean <- function(directory,pollutant, id = 1:332){
    temp <- list.files(path = directory, pattern="*.csv")
    scope <- data.frame()
    for (i in id){
        scope <- rbind(scope, read.csv(paste(directory,temp[i], sep="/")))
    }
    if (pollutant == "sulfate") {
        result <- mean(scope$sulfate, na.rm = TRUE)
    } else  if (pollutant == "nitrate") {
        result <- mean(scope$nitrate, na.rm = TRUE)
    }
    result
}