

rankhospital <- function(state, outcome, num = "best") {
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
        data <- data[c("name","state",outcome)]
        names(data)[3] <- "outcome"
        
        ## order the dataset and add ranking on it
        ordered <- data[rev(order(as.numeric(data$outcome))),]
        ordered$rank <- NA
        ordered <- transform(ordered, rank = rank(as.numeric(outcome), ties.method = "first"))
        
        ## produce result
        if( num == "best" ) {
                ordered[ordered$rank==1,]
        } else if( num == "worst" ) {
                tail(ordered,1)
        } else {
                ordered[ordered$rank==num,]
        }

}

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)