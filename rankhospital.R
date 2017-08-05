rankhospital <- function(state, outcome, num = "best") {
    #read data
    data <- read.csv("outcome-of-care-measures.csv")
    #check validity
    valid_states <- data[ ,7]
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    #loop to check validity
    if((state %in% valid_states) == FALSE) {
        stop(print("invalid state"))
    } else if ((outcome %in% valid_outcomes) == FALSE) {
        stop(print("invalid outcome"))
    }
    #get data subset
    chosen_data <- subset(data, state == state)
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
    #get rid of NAs
    chosen_data[ ,outcome_column] <- as.numeric(chosen_data[ ,outcome_column])
    bad <- is.na(chosen_data[ ,outcome_column])
    clean_data <- chosen_data[!bad,]
    #arrange ranking order of outcome values through index
    
    selected_outcome <- names(clean_data)[outcome_column]
    selected_hospital <- names(clean_data)[2]
    index <- with(clean_data, order(clean_data[selected_outcome], clean_data[selected_hospital]))
    ordered_data <- clean_data[index, ]
    #outcome values arranged in ascending order
    #interpret "best" and "worst"
    if(is.character(num) == TRUE) {
        if(num== "best") {
            num = 1
        } else if(num == "worst") {
            num= length(ordered_data[ , outcome_column])
        }
    }
    #return the ranked hospital
    ordered_data[num,2]
}
