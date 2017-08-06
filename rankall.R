#ranking hospitals in all states

rankall <- function(outcome, num = "best") {
    #read data
    data <- read.csv("outcome-of-care-measures.csv")
    #check validity
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    ## Validate the num value
    if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
    
    #data subset
    if (outcome == "heart attack") {
        outcome_column <- 11
    } else if (outcome == "heart failure") {
        outcome_column <- 17
    } else {
        outcome_column <- 23
    }
    #if num is greater than the number of hospitals in that state, return NA
    if (is.numeric(num)==TRUE) {
        if(length(data[ ,2])< num) {
            return(NA)
        }
    }
    #ordering data
    ordered_data <- data[order(data[,2]),]
    ordered_data <- ordered_data[order(ordered_data[,outcome_column], ordered_data[,2]), ]
    
    #Return a data frame with the hospital names and the (abbreviated) state name
    result <- data.frame()
    states <- state.abb
    
    #ranking each state
    for(i in states) {
        state <- subset(ordered_data, ordered_data[,7]==i)
        if (grepl(num,"worst")) {
            hosp <- state[nrow(state),2]
        } else  if (num== "best") {
            num = 1
        } else {
            hosp <- state[num,2]
        }
        final_result <- cbind(hosp, i)
        result <- rbind(result, final_result)
    }
    names(result) <- c("hospital", "state")
    return(result)
}
