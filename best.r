best <- function(state, outcome) {
        ## Check the outcome parameter
        outcomes = c("heart attack", "heart failure", "pneumonia")
        if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Filter and simplify the column names
        data <- data[c(2, 7, 11, 17, 23)]
        names(data)[1] <- "name"
        names(data)[2] <- "state"
        names(data)[3] <- "heart attack"
        names(data)[4] <- "heart failure"
        names(data)[5] <- "pneumonia"
        
        ## Check the state parameter
        states <- data[, 2]
        all_states <- unique(states)
        if( state %in% all_states == FALSE ) stop("State not in file")
        
        ## filter the dataset, remove the non numeric one	
        data <- data[data$state==state & data[outcome] != 'Not Available', ]
        outcome_value <- data[, outcome]
        rowNum <- which.min(outcome_value)
        ## Return hospital name in that state with minimum 30-day death rate
        data[rowNum, ]$name
}

best("SC", "heart attack")


best("NY", "pneumonia")


best("AK", "pneumonia")
