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

## Return index (in data source) relating to passed outcome name
## Return 0 if passed outcome is not a valid one (out of the three specified in requirements)
outToIdx <- function(outcome){
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

## validate state against data$State and and outcome against the three valid ones
checkParams <- function(state, outcome){
    if (!state %in% data$State) {
        return (cInvState)
    } else {
        if(outToIdx(outcome) == cBadOutcomeIdx) {
            return (cInvOutc)
        }
    }
    return (cValidParams)
}

## Return the name of the hospital having the lowest mortality
hByOutcome <- function(dInState, outcome) {
    outcData <- dInState[, outToIdx(outcome)] # will contain only data relating to the specified outcome
    m <- min(outcData, na.rm=T)               # get lowest (min) mortality for specified outcome (exclude NAs)
    midx <- which(outcData == m)              # get hospital index with lowest mortality
    return(dInState[midx, 2])                 # get Hospital.Name
}

best <- function(state, outcome) {
    ## get measures and check if params are valid 
    dsrc <- "./outcome-of-care-measures.csv"
    data <- read.csv(dsrc, colClasses="character")
    
    #check parameters
    cp <- checkParams(state, outcome)
    if (cp == cInvState) stop(paste("invalid state >", state))
    if (cp == cInvOutc) stop(paste("invalid outcome >", outcome))
    
    ## convert char (just read) measures into relating numbers
    data[, cHeartAttack] <- as.numeric(data[, cHeartAttack])
    data[, cHeartFailure] <- as.numeric(data[, cHeartFailure])
    data[, cPneumonia] <- as.numeric(data[, cPneumonia])
    
    ## select only measures in specified state
    dInState <- data[data[, cState] == state, ]
    ## get the name of the hospital having the lowest mortality according specified STATE and OUTCOME
    hn <- hByOutcome(dInState, outcome)
    return(hn)
}

# tests
tests <- function(){
    str(best("TX", "heart attack"))     # expected: [1] "CYPRESS FAIRBANKS MEDICAL CENTER" plus some warnings
    str(best("TX", "heart failure"))    # expected: [1] "FORT DUNCAN MEDICAL CENTER"  plus some warnings
    str(best("MD", "heart attack"))     # expected: [1] "JOHNS HOPKINS HOSPITAL, THE" plus some warnings
    str(best("MD", "pneumonia"))        # expected: [1] "GREATER BALTIMORE MEDICAL CENTER"  plus some warnings
    str(best("BB", "heart attack"))     # expected: Error in checkParams(state, outcome) : invalid state
    str(best("NY", "hert attack"))      # expected: Error in checkParams(state, outcome) : invalid outcome 
}

