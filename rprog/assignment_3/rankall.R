rankhospital_2 <- function(state, ot, col, num = "best") {
        ## Assumes ot has already been loaded and converted to numeric and sorted.
        ## col should already be the fully qualified column name from the original data set.
        
        ## Extract records for the supplied state
        state_only <- ot[ot$State == state,]
        
        ## Remove NA values
        valid_only <- state_only[!is.na(state_only[,col]),]
        
        if (num == "best") {
                num <- 1
        } else if (num == "worst") {
                num <- nrow(valid_only)
        }
        
        if (num > nrow(valid_only)) {
                return(NA)
        } else {
                return(toString(valid_only[num, "Hospital.Name"]))
        }
}


rankall <- function(outcome, num = "best") {

        ## Read outcome data
        ot<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Lame string parsing...
        if (outcome == "heart attack") {
                col = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
                col = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"                
        } else if (outcome == "pneumonia") {
                col = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                stop("invalid outcome")
        }
        
        ## Convert numbers from strings to actual numbers.
        ot[,col] <- as.numeric(ot[,col])
        
        ## Extract all valid states?unique
        state <- unique(ot$State)
        state <- state[order(state)]
        
        ## Sort the table
        ot <- ot[order(ot[,col], ot$Hospital.Name),]
 
        hospital = sapply(state, rankhospital_2, ot, col, num)
        
        data.frame(cbind(hospital, state))
}
