#` The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
#` of the hospital that has the ranking specified by the num argument. For example, the call
rankhospital <- function(state, outcome, num) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    ## Check that state and outcome are valid
    states <- data[, 7]
    outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    if ((state %in% states) == FALSE) {
        stop("Invalid state")
    }
    if ((outcome %in% names(outcomes)) == FALSE) {
        stop("Invalid outcome")
    }
    
    ## Get the subset of the data with desired state
    new_data <- subset(data, data$State == state)
    
    ## Get rid of NA's in the desired outcome column
    required_columns <- as.numeric(new_data[, outcomes[outcome]])
    bad <- is.na(required_columns)
    desired_data <- new_data[!bad, ]
    

    ## Find the hospitals in the rows with the minimum outcome value
    columns_considered <- as.numeric(desired_data[, outcomes[outcome]])
    desired_hospitals <- desired_data[, c(2, outcomes[outcome])]
    
    ## If there are multiple hospitals which the minimum outcome value, then
    ## return the first hospital name from the alphabetically sorter hospital names list
    sorted_hospitals <- desired_hospitals[order(desired_hospitals[, 2], desired_hospitals[, 1]),]
    Rank <- order(sorted_hospitals[, 2], sorted_hospitals[, 1])
    sorted_hospitals$Rank <- Rank
    if (num == "best"){
        num = 1
    }
    else if (num == "worst"){
        num = nrow(sorted_hospitals)
    }
    else if (num > nrow(sorted_hospitals)){
        return(NA)
    }
    result_hospitals <- subset(sorted_hospitals, sorted_hospitals$Rank <= num)
    result <- subset(result_hospitals, result_hospitals$Rank == num)[, 1]
    return(result)
}
