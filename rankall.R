#ranking hospitals in all states

rankall <- function(outcome, num = "best") {
    #read data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
    #check validity
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if ((outcome %in% valid_outcomes) == FALSE) {
        stop(print("invalid outcome"))
    }
    
    if(outcome == "heart attack") {
        outcome_column <- 11
    } else if(outcome == "heart failure") {
        outcome_column <- 17
    } else { outcome_column <- 23
    }
    
    # validate the num value
    if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
    

    #arrange data 1= hospital name 2= state 3= outcome_column & remove NAs
    data <- na.omit(data[ ,c(2,7,outcome_column)])
    
    #if num is greater than the number of hospitals in that state, return NA
    if (is.numeric(num)==TRUE) {
        if(length(data[ ,1])< num) {
            return(NA)
        }
    }
    
    #coerce outcome column to numeric
    data[ ,3] <- as.numeric(data[,3])
    
    #order data by state/ outcome / hospital name columns
    ordered_data <- data[order(data[,2], data[,3], data[,1]), ]
    
    #ranking hospitals of each state
    split_data <- split(ordered_data, ordered_data$State)
    
    hospital_rank <- function(x) {
        num <- ifelse(num =="best", 1, ifelse(num== "worst", nrow(x), num))
        return(c(x[num,1]))
    }
    
    #Return a data frame with the hospital names and the (abbreviated) state name
    hospitals <- unlist(lapply(split_data, hospital_rank))
    
    return(data.frame(hospital = hospitals, state = names(hospitals)))
    
}
