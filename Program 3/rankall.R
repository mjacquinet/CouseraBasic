source(rankhospital.R)

rankall <- function(outcome, num = "best") {
    outcomedata <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    tempState <- data.frame(table(outcomedata$State))
    tempState<- sapply(tempState, as.character)
    
    
    rankAllDf <- data.frame(hospital=character(), state=character(), stringsAsFactors=FALSE)
    for(i in tempState[,1]){
      tempRank<-rankhospital(i, outcome, num)
      tempfr <- data.frame(hospital = tempRank, state = i)
      rankAllDf <- rbind(rankAllDf, tempfr)
    }
    return(rankAllDf)
}