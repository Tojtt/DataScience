complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    temp <- list.files(path = directory, pattern="*.csv")
    endframe <- data.frame()
    for (i in id){
        endframe <- rbind(endframe, sum(complete.cases(read.csv(paste(directory,temp[i], sep="/")))))
    }
    endframe <- cbind(id,endframe)
    colnames(endframe) <- c("id","nobs")
    endframe
    
}