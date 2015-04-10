count_complete <- function(frame) {
        ## Pull an array of sulfate readings from the frame
        sulf_present <- !is.na(frame[,"sulfate"])
        nitr_present <- !is.na(frame[,"nitrate"])

        ## Count the number of instances where both are present.
        sum(sulf_present & nitr_present)
}

calc_corr <- function(frame) {
        cor(frame$sulfate, frame$nitrate, use="complete.obs")
}

corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

        ## filenames is a collection of all data files in the directory
        filenames <- dir(directory)
        filenames <- file.path(directory, filenames)

        ## Populate a list of frames for all monitors in the data directory
        frames <- lapply(filenames, read.csv)

        ## From each frame, pull an array of readings
        nobs <- sapply(frames, count_complete)

        ## Extract frames that meet the threshold criteria
        valid_frames <- frames[nobs > threshold]

        ## Calculate correlation within each frame
        sapply(valid_frames, calc_corr)
}
