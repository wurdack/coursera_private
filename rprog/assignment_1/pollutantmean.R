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

        ## filenames is a vector of monitor CSV files.
        ids <- sprintf("%03d.csv",id)
        filenames <- file.path(directory, ids)

        ## Read a data frame from each file in filenames.
        frames <- lapply(filenames, read.csv)

        ## Bind all the frames together.
        all_frames <- Reduce(rbind, frames)

        ## Extract requested column.
        col <- all_frames[,pollutant]

        ## Calculate mean, ignoring NA values.
        mean(col, na.rm = TRUE)
}

