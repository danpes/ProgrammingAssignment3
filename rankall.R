setwd("C:/Users/daniele.pes/Desktop/coursera/week4")

## will use what follows as constants
cHeartAttack <- 11   # index of "heart attack" column
cHeartFailure <- 17  # index of "heart failure" column
cPneumonia <- 23     # index of "pneumonia" column
cBadOutcomeIdx <- 0  # outcome not in specified set
cState <- 7          # index of "State" column
cInvState <- -2      # invalid state
cInvOutc <- -1       # invalid outcome
cValidParams <- 0    # parameters are valid

outToIdx <- function(outcome){
    ## Return index (in data source) relating to passed outcome name
    ## Return 0 if passed outcome is not a valid one (out of the three specified in requirements)
    voutc <- c("heart attack", "heart failure", "pneumonia")
    if(!outcome %in% voutc){
        return (cBadOutcomeIdx)
    }
    if(outcome == "heart attack") {
        return (cHeartAttack)
    } else if(outcome == "heart failure"){
        return(cHeartFailure)
    } else {
        return (cPneumonia)
    }
}

checkParams <- function(outcome){
    ## validate outcome against the three valid ones
    if(outToIdx(outcome) == cBadOutcomeIdx) {
        return (cInvOutc)
    }
    return (cValidParams)
}

orderedRanked <- function(dInState, outcData, num){
    # Return the ranked row for the selected (state, outcome)
    ordered <- order(outcData, dInState$Hospital.Name) # ascending ordering of outcome/stated data according to Hospital.Name
    rankedOrdered <- ordered[num]                      # take the num-th
    return (dInState$Hospital.Name[rankedOrdered])
}

hByRankedOutc <- function(dInState, outcome, num) {
    ## Return the name of the hospital having the num-ranked mortality, if available (otherwise NA)
    outcData <- dInState[, outToIdx(outcome)]                 # will contain only data (column) relating to the specified outcome
    
    if(num == "best"){
        return (orderedRanked(dInState, outcData, 1))         # get the first in the ordered list (that is, the best performing)
    }
    
    hm <- dim(dInState[!is.na(outcData),])[1]                 # 2 dimensions: [1] is the number of !NA rows and [2] is the number
    # of columns (that is 46) in the original data source, having bean
    # selected by state and outcome
    if(num == "worst"){
        return (orderedRanked(dInState, outcData, hm))        # get the last ordered and !NA
    } else{
        if (num > hm){
            return (NA)                                       # over available range
        } else {
            return (orderedRanked(dInState, outcData, num))   # return the num-th in the ordered range
        }
    }
}

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that outcome is valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    ## get measures and check if params are valid 
    dsrc <- "./outcome-of-care-measures.csv"
    data <- read.csv(dsrc, colClasses="character")
    
    #check parameters
    cp <- checkParams(outcome)
    if (cp == cInvOutc) stop(paste("invalid outcome >", outcome))
    
    ## convert char (just read) measures into relating numbers
    data[, cHeartAttack] <- as.numeric(data[, cHeartAttack])
    data[, cHeartFailure] <- as.numeric(data[, cHeartFailure])
    data[, cPneumonia] <- as.numeric(data[, cPneumonia])
    
    states <- sort(unique(data$State))                       # order by ascending name
    hms <- length(states)
    hs <- rep("", hms)                                       # array of ranked hospitals (one or NA, per each state)
    
    for(i in 1:hms){
        ## select only measures in specified state
        dInState <- data[data[, cState] == states[i], ]
        ## get the name of the hospital having mortality ranked num-th, according ith STATE and OUTCOME
        hs[i] <- hByRankedOutc(dInState, outcome, num)
    }
    
    rhs <- data.frame(hospital=hs, state=states)
    return(rhs)
}

# tests
tests <- function(){
    head(rankall("heart attack", 20), 10)            # expected: "" plus some warnings on data convertion
    tail(rankall("pneumonia", "worst"), 3)           # expected: "" plus some warnings on data convertion
    tail(rankall("heart failure"), 10)               # expected: "" plus some warnings on data convertion
}