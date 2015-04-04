complete <- function(directory, ids = 1:332) {
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
    id <- c()
    nobs <- c()

    ## count completes
    for (i in ids) {
        ## read data from csv file
        file_name = sprintf("%03d", i)
        csv_file_name <- paste(directory, "/", file_name, ".csv", sep="")
        mydata <- read.csv(csv_file_name)

        completes <- 0
        for (j in 1:nrow(mydata)) {
            row <- mydata[j, ]

            if (!is.na(row['sulfate']) && !is.na('nitrate')) {
                completes <- completes + 1
            }
        }

        id <- c(id, i)
        nobs <- c(nobs, completes)
    }

    result <- data.frame(id, nobs)
    result
}
