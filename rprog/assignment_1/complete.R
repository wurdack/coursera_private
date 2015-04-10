count_complete <- function(frame) {
        ## Pull an array of sulfate readings from the frame
        sulf_present <- !is.na(frame[,"sulfate"])
        nitr_present <- !is.na(frame[,"nitrate"])

        ## Count the number of instances where both are present.
        sum(sulf_present & nitr_present)
}

complete <- function(directory, id = 1:332) {
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

        ## filenames is a vector of monitor CSV files.
        ids <- sprintf("%03d.csv",id)
        filenames <- file.path(directory, ids)

        ## Frames is a list of data frames corresponding to each file
        frames <- lapply(filenames, read.csv)

        ## From each frame, pull an array of readings
        nobs <- sapply(frames, count_complete)

        ## Package the results into a data frame
        data.frame(id, nobs)
}
