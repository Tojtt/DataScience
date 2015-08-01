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
    ## now we sort the row ASCENDINGLY base on State 
    ## and the value of outcome column
    newstatedata <- statedata[order(statedata[,2],statedata[,3], na.last = NA),]
    ## we set the value of the object index based on the input of outcome
    ## that will be use later on to pick out the row
    if (class(num)!= "numeric") { 
        index <-1
    } else {
        index <- num
    }
    ## create an empty data frame called finalframe
    finalframe <- data.frame()
    ## compose a full list of states
    fullstatelist <- sort(c("DC","GU","PR","VI",state.abb))
    ## for every value in the state list
    for (namestate in fullstatelist){
 	## pick out the row number that has the same state value
        rowwithState <- grep(namestate, newstatedata[,2])
	## now pull out those rows in our data frame
        finalcheck <- newstatedata[rowwithState,]
	## we adjusting the sorting method considering the value of num
        if (num == "worst"){
	    ## this will sort the list DESCENDINGLY so the worst value is first
            finalcheck <- finalcheck[order(-finalcheck[,3],finalcheck[,1]),]
        } else {
	    ## this will sort the list DESCENDINGLY so the best value is first
            finalcheck <- finalcheck[order(finalcheck[,3],finalcheck[,1]),]
        }
	## now we get the data base on the index
        finalframe<- rbind(finalframe,finalcheck[index,])
    }
    finalframe <- cbind.data.frame(finalframe[,1], fullstatelist)
    ## changing the column num so it is easier to read
    colnames(finalframe) <- c("hospital","state")
    finalframe
}

