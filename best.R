#best function
best <- function(state, outcome) {
    #reading data
    data <- read.csv("outcome-of-care-measures.csv")
    #check validity
    valid_states <- data[ ,7]
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    #create loop to check validity
    if ((state %in% valid_states) == FALSE) {
        stop(print("invalid state"))
    }
    else if ((outcome %in% valid_outcomes) == FALSE) {
        stop(print("invalid outcome"))
    }
    #get the subset data with the desired state and outcome
    state_data <- subset(data, state==state)
    #loop to choose outcome
    if (outcome == "heart attack") {
        outcome_column <- 11
    } else if (outcome == "heart failure") {
        outcome_column <- 17
    } else {
        outcome_column <- 23
    }
    #get rid of NA's
    desired_columns <- as.numeric(state_data[ ,outcome_column])
    bad <- is.na(desired_columns)
    desired_data <- state_data[!bad, ]

    
    #find hospital in rows with minimum outcome value
    clean_columns <- as.numeric(desired_data[ , outcome_column])
    desired_rows <- which(clean_columns == min(clean_columns))
    best_hospital <- desired_data[desired_rows,2]
    #if there's a tie, sort alphabetically
    if(length(best_hospital) > 1) {
        sorted_hospital <- sort(best_hospital)
        sorted_hospital[1]
    } else {
        best_hospital
    }
}
