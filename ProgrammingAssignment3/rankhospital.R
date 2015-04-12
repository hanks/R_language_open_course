get_rank_hospital_name <- function(data, state, column, num) {
    data[, 2] <- as.numeric(data[, 2])
    ordered_data <- order(data[column], data$Hospital.Name)
    as.character(data$Hospital.Name[ordered_data[num]])
}

check_input <- function(data) {
    if (nrow(data) == 0) {
        stop("invalid state")
    }         
}

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses="character")

    column <- if (outcome == "heart attack") {
		"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	} else if (outcome == "heart failure") {
		"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	} else if (outcome == "pneumonia") {
		"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	} else {
		stop("invalid outcome")
	}

    ## collect needed data
    data <- data[data$State == state, c("Hospital.Name", column)]

    ## Check that state and outcome are valid
    check_input(data)
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    num <- if (num == "best") {
        1
    } else if (num == "worst") {
        nrow(data)
    } else {
        num
    }
    
    result <- get_rank_hospital_name(data, state, column, num)

    return (result)
}

