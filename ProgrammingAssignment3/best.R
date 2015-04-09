get_best_hospital_name <- function(data, state, column) {
    data[, 2] <- as.numeric(data[, 2])
    ordered_data <- order(data[column], data$Hospital.Name)
    as.character(data$Hospital.Name[ordered_data[1]])
}

check_input <- function(data) {
    if (nrow(data) == 0) {
        stop("invalid state")
    }         
}

best <- function(state, outcome) {
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
    result <- get_best_hospital_name(data, state, column)

    return (result)
}
