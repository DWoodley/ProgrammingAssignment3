best <- function(state, outcome) { 
    ## Read outcome data
    OutcomeData <- read.csv("outcome-of-care-measures.csv",header = TRUE,
                            colClasses = "character")
    
    ocvect <- c("heart attack", "heart failure","pneumonia")
    
    ## Check that state and outcome are valid
    ## The outcomes can be one of heart attack, heart failure, or pneumonia.
    if(sum(OutcomeData[,7] == state) == 0) stop("invalid state.")
    
    if (outcome != ocvect[1] && outcome != ocvect[2] && outcome != ocvect[3]) 
        stop("Invalid outcome type.")

    if (outcome == "heart attack") colnum <- 11
    if (outcome == "heart failure") colnum <- 17
    if (outcome == "pneumonia") colnum <- 23
    
    ## Filter on state
    OutcomeData <- OutcomeData[OutcomeData$State == state,]
    
    ## Make appropriate 30 day Mortality column numeric
    OutcomeData[,colnum] <- as.numeric(OutcomeData[,colnum])
        
    rating = min(OutcomeData[,colnum], na.rm = TRUE) 
    
    ## Return hospital name in that state with lowest 30-day death ## rate
    OutcomeData$Hospital.Name[(OutcomeData[,colnum] == rating
                               & !is.na(OutcomeData[,colnum]))]        

}



