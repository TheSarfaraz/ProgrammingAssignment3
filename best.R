best <- function(state, outcome) {
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
    desired_rows <- which(columns_considered == min(columns_considered))
    desired_hospitals <- desired_data[desired_rows, 2]
    
    ## If there are multiple hospitals which the minimum outcome value, then
    ## return the first hospital name from the alphabetically sorter hospital names list
    if (length(desired_hospitals) > 1) {
        hospitals_sorted <- sort(desired_hospitals)
        hospitals_sorted[1]
    } else {
        desired_hospitals
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}