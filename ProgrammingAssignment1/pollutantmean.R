pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)

    result_list <- c()

    for (i in id) {
        ## read data from csv file
        file_name = sprintf("%03d", i)
        csv_file_name <- paste(directory, "/", file_name, ".csv", sep="")
        mydata <- read.csv(csv_file_name)

        ## remove NA data, count mean, and append to result list
        avector <- mydata[, pollutant]
        result_list <- c(result_list, avector)
    }

    round(mean(result_list, na.rm=T), 3)
}
