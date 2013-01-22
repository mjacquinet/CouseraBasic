best <- function(state, outcome) {
  ## Read outcome data
  outcomedata <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  outcomedata[, 11] <- as.numeric(outcomedata[, 11])
  outcomedata[, 17] <- as.numeric(outcomedata[, 17])
  outcomedata[, 23] <- as.numeric(outcomedata[, 23])
  
  ## Check that state and outcome are valid
  if (outcome=="heart attack"){
    outcome.number<-11  
  }else if(outcome=="heart failure"){
    outcome.number<-17
  }else if(outcome=="pneumonia"){
    outcome.number<-23
  }else{
    stop("invalid outcome")
  }
  
  #Create array of states
  tempState <- data.frame(table(outcomedata$State))
  tempState<- sapply(tempState, as.character)
  
  #Test is state parameter is in table
  if(!(state %in% tempState)){
    stop("invalid state")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  outcomeTemp<-subset(outcomedata, State == state)
  outcomeTemp<-outcomeTemp[order(outcomeTemp[,outcome.number], outcomeTemp[,"Hospital.Name"]),]
  
  return(outcomeTemp[1,"Hospital.Name"])
}