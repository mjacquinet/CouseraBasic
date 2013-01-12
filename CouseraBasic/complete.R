##Part 2 complete.R

## need for the count function
library(plyr)

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  completeDf <- data.frame(id=numeric(), nobs=numeric(), stringsAsFactors=FALSE)
  
  for(idCounter in id){
    tempData<-getmonitor(idCounter, directory)
    tempAdd<- c(idCounter, count(complete.cases(tempData))[2,2])
    completeDf <- rbind(completeDf, tempAdd)
  }
  names(completeDf)[1]="id"
  names(completeDf)[2]="nobs"
  
  return(completeDf)
}

