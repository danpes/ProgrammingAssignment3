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

checkParams <- function(state, outcome){
    ## validate state against data$State and and outcome against the three valid ones
    if (!state %in% data$State) {
        return (cInvState)
    } else {
        if(outToIdx(outcome) == cBadOutcomeIdx) {
            return (cInvOutc)
        }
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

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
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
    ## get the name of the hospital having mortality ranked num-th, according specified STATE and OUTCOME
    hn <- hByRankedOutc(dInState, outcome, num)
    return(hn)
}

# tests
tests <- function(){
    rankhospital("MN", "heart attack", 5000)         # expected: "[1] NA" plus some warnings on data convertion
    rankhospital("MD", "heart attack", "worst")      # expected: "[1] "HARFORD MEMORIAL HOSPITAL" plus some warnings on data convertion
    rankhospital("MD", "heart attack")               # expected: "[1] "JOHNS HOPKINS HOSPITAL, THE" plus some warnings on data convertion
    rankhospital("TX", "heart failure", 4)           # expected: "[1] "DETAR HOSPITAL NAVARRO" plus some warnings on data convertion
}
