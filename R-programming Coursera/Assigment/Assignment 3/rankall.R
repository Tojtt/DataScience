rankall <- function(outcome, num="best"){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    ## Because R reads " " as "." we need to substitute the input
    subdotin <-sub(" ",".", outcome)
    ## We find the columns that have the value of outcome
    columnwithoutcome <- grep(subdotin, colnames(data), ignore.case = TRUE)
    if (length(columnwithoutcome) == 0) {
        stop("invalid outcome")
    }
    ## Return hospital name in that state with lowest 30-day death 
    ## we now create a dataframe with the desire dimensions found from above
    statedata <- data[, c(2,7,columnwithoutcome[1])]
    ## because we import the table as character we need to convert it back
    ## to numeric in able to sort the data
    statedata[,3]<-as.numeric(statedata[,3])
    ## now we sort the row ASCENDINGLY
    ## we adjusting the sorting method considering the value of num
    newstatedata <- statedata[order(statedata[,2],statedata[,3], na.last = NA),]
    if (class(num)!= "numeric") { 
        index <-1
    } else {
        index <- num
    }
    finalframe <- data.frame()
    fullstatelist <- sort(c("DC","GU","PR","VI",state.abb))
    for (namestate in fullstatelist){
        rowwithState <- grep(namestate, newstatedata[,2])
        finalcheck <- newstatedata[rowwithState,]
        if (num == "worst"){
            finalcheck <- finalcheck[order(-finalcheck[,3],finalcheck[,1]),]
        } else {
            finalcheck <- finalcheck[order(finalcheck[,3],finalcheck[,1]),]
        }
        finalframe<- rbind(finalframe,finalcheck[index,])
    }
    finalframe <- cbind.data.frame(finalframe[,1], fullstatelist)
    colnames(finalframe) <- c("hospital","state")
    finalframe
}

