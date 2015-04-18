best <- function(state, outcome) {

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

        ## Return hospital name in that state with lowest 30-day death
        ## rate
        toString(state_only[1, "Hospital.Name"])
}
