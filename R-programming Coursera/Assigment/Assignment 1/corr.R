corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    temp <- list.files(path = directory, pattern="*.csv")
    a <- vector()
    for (i in 1:332) {
        if (sum(complete.cases(read.csv(paste(directory,temp[i], sep="/")))) > threshold) {
            dummyfile <- read.csv(paste(directory,temp[i], sep="/"))
            a <- c(a, cor(dummyfile[2],dummyfile[3],use = "pairwise.complete.obs"))
        }
    }
    a
}