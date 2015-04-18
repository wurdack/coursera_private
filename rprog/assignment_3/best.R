best <- function(state, outcome) {
        ## Read outcome data
        ot<-read.csv("outcome-of-care-measures.csv")
        
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
        
        ## Extract the 
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate

        
        message(state)
        message(outcome)
        message(col)


        ot[ot$state == state, col]
}
