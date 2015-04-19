rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ot<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if (!sum(ot$State==state)) {
                stop("invalid state")        
        }
        
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
        
        ## Sort the table
        ot_sorted <- ot[order(ot[,col], ot$Hospital.Name),]
        
        ## Extract records for the supplied state
        state_only <- ot_sorted[ot_sorted$State == state,]
        
        ## Remove NA values
        valid_only <- state_only[!is.na(state_only[,col]),]
        
        if (num == "best") {
                num <- 1
        } else if (num == "worst") {
                num <- nrow(valid_only)
        }

        message(num)
        if (num > nrow(valid_only)) {
                return(NA)
        } else {
                return(toString(valid_only[num, "Hospital.Name"]))
        }
}
