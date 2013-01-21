rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomedata <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
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
  outcomeTemp<-subset(outcomeTemp, outcomeTemp[,11] != "Not Available")
  outcomeTemp<-outcomeTemp[order(outcomeTemp[,outcome.number], outcomeTemp[,"Hospital.Name"], na.last = TRUE),]
  
  #check if best, worst, or an integer and that the num isn't greater than the number of rows
  if (num == "best"){
    return(outcomeTemp[1,"Hospital.Name"])    
  }else if (num == "worst"){
    return(outcomeTemp[nrow(outcomeTemp), "Hospital.Name"])
  }else if (num > nrow(outcomeTemp)){
    return(NA)
  }else{
    return(outcomeTemp[num,"Hospital.Name"])  
  }
}
rankhospital("MD", "heart attack", "worst")
rankhospital ("NY", "heart failure", num = "worst")
rankhospital("MN", "heart attack", 5000)