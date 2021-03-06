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

    # pad ints with leading zeroes: sprintf('%03d', x)

    l = NA

    for (i in id) {
        l = c(l, read.csv(paste(directory, '/', sprintf('%03d', i), '.csv', sep=''))[[pollutant]])
    }

    sprintf('%.3f', mean(l, na.rm = T))
}