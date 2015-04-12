rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    state <- data$State
    state <- sort(unique(state))

    hospital <- rep("", length(state))
    
    if (outcome == 'heart attack') {
        death <- as.numeric(statedata[,11])
    } else if (outcome == 'heart failure') {
        death <- as.numeric(statedata[,17])
    } else if (outcome == 'pneumonia') {
        death <- as.numeric(statedata[,23])
    } else {
        stop("invalid outcome")
    }

    a <- rank(death, na.last=NA)
    
    if (num=="best") {
        r <- 1
    } else if (num =="worst") {
        r <- length(a)
    } else if (num <= length(a) ) {
        r <- num
    } else {
        r <- NA
    }
  
    for (i in 1:length(state)) {
        statedata<- data[data$State==state[i],]

        if (is.na(r)) {
            hospital[i] <- NA
        } else {
            hospital[i] <- statedata$Hospital.Name[order(death, statedata$Hospital.Name)[r]]
        }

    }

    return(data.frame(hospital=hospital, state=state))
}
