
best <- function(state, outcome){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    ## Because R reads " " as "." we need to substitute the input
    subdotin <-sub(" ",".", outcome)
    ## now we find the row with the state input
    rowwithState <- grep(state, data[,7])
    ## if no match found as in it is not valid, we stop the function
    if (length(rowwithState) == 0){
        stop("invalid state")
    }
    ## the same thing goes with the column with outcome input
    columnwithoutcome <- grep(subdotin, colnames(data), ignore.case = TRUE)
    if (length(columnwithoutcome) == 0) {
        stop("invalid outcome")
    }
    ## Return hospital name in that state with lowest 30-day death 
    ## we now create a dataframe with the desire dimensions found from above
    statedata <- data[rowwithState, c(2,columnwithoutcome[1])]
    ## because we import the table as character we need to convert it back
    ## to numeric in able to sort the data
    statedata[,2]<-as.numeric(statedata[,2])
    ## now we sort the row ASCENDINGLY
    newstatedata <- statedata[order(statedata[,2]),]
    ## now we get the result
    newstatedata[1,1]
}
