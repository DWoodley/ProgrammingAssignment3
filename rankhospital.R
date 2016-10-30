rankhospital <- function(state, outcome, num = "best") { 
    ## Read outcome data
    OutcomeData <- read.csv("outcome-of-care-measures.csv",header = TRUE,
                            colClasses = "character")
    
    ## Create vector of valid outcomes
    ## The outcomes can be one of heart attack, heart failure, or pneumonia.
    ocvect <- c("heart attack", "heart failure","pneumonia")

    if (!is.numeric(as.numeric(num)) && sum(num == c("best","worst")) == 0)
        stop("Invalid outcome ranking.")
    
    ## Check that state is valid
    if(sum(OutcomeData[,7] == state) == 0) stop("invalid state")
    
    ## Check that is valid
    if (outcome != ocvect[1] && outcome != ocvect[2] && outcome != ocvect[3]) 
        stop("Invalid outcome type")
    
    ## Assign appropriate column for requested outcome
    if (outcome == "heart attack") colnum <- 11
    if (outcome == "heart failure") colnum <- 17
    if (outcome == "pneumonia") colnum <- 23
    
    ## Filter on state
    OutcomeData <- OutcomeData[OutcomeData$State == state,]
    
    ## Make appropriate 30 day Mortality column numeric
    OutcomeData[,colnum] <- as.numeric(OutcomeData[,colnum])
    
    ## Trim leading whitespace to get alphabetical order later
    OutcomeData$Hospital.Name <- trimws(OutcomeData$Hospital.Name,"l")
    
    ## Remove NAs in outcome column
    OutcomeData <- OutcomeData[!is.na(OutcomeData[,colnum]),]
    
    ## Order by outcome and hospital name
    OutcomeData <- OutcomeData[order(OutcomeData[,colnum],OutcomeData$Hospital.Name),]

    ## Index of record with the given rank of 30-day death rate
    if (num == "best") {
        x <- 1
    }
    else if (num == "worst") {
        x <- length(OutcomeData[,colnum])
    }
    else {
        x <- as.integer(num)
    }

    ## Return hospital name in state with the given rank of 30-day death rate
    OutcomeData[x,]$Hospital.Name 
}


