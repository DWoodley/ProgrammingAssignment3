rankall <- function(outcome, num = "best") { 
    ## Read outcome data
    ## Check that outcomes are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the (abbreviated) state name
    
    OutcomeData <- read.csv("outcome-of-care-measures.csv",header = TRUE,
                            colClasses = "character")
    
    ## Create vector of valid outcomes
    ## The outcomes can be one of heart attack, heart failure, or pneumonia.
    ocvect <- c("heart attack", "heart failure","pneumonia")
    
    if (!is.numeric(as.numeric(num)) && sum(num == c("best","worst")) == 0)
        stop("Invalid outcome ranking")
    
    ## Check that outcome type is valid
    if (outcome != ocvect[1] && outcome != ocvect[2] && outcome != ocvect[3]) 
        stop("Invalid outcome type")
    
    ## Assign appropriate column for requested outcome
    if (outcome == "heart attack") colnum <- 11
    if (outcome == "heart failure") colnum <- 17
    if (outcome == "pneumonia") colnum <- 23
    
    ## Make appropriate 30 day Mortality column numeric
    OutcomeData[,colnum] <- as.numeric(OutcomeData[,colnum])
    
    ## Remove NAs in outcome column
    OutcomeData <- OutcomeData[!is.na(OutcomeData[,colnum]),]
    
    ## Trim leading whitespace to get alphabetical order later
    OutcomeData$Hospital.Name <- trimws(OutcomeData$Hospital.Name,"l")
    
    ### For each state: Select outcome that matches requested level
    ### Insert into reporting table
    StList <- sort(unique(OutcomeData$State))
    OutList <- data.frame()
    
    for (st in StList) {
        ## Filter on state
        TempData <- OutcomeData[OutcomeData$State == st,]
        
        ## Order by rating and hospital name
        TempData <- TempData[order(TempData[,colnum],TempData$Hospital.Name),]
        
        ## Get index of record with the given rank of 30-day death rate
        if (num == "best") {
            x <- 1
        }
        else if (num == "worst") {
            x <- length(TempData[,colnum])
        }
        else {
            x <- as.integer(num)
        }
        
        ## Return hospital name in state with the given rank of 30-day death rate
        OutList <- rbind(OutList,
                         data.frame(hospital = TempData[x,]$Hospital.Name,
                         state = st))
    } 
    OutList
}