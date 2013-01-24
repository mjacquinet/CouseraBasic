##Part 1 - getmonitor.R

getmonitor <- function(id, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  ## Your code here
  
  ## make id a character
  idChar<-as.character(id)
  
  ## add leading zeros if needed
  if(nchar(idChar)==1){
    idChar<-paste("00", idChar, sep="")
  }else if(nchar(idChar)==2){
    idChar<-paste("0", idChar, sep="")
  }
  
  ## add .csv if needed
  if(!grepl(".csv",idChar)){
    idChar<-paste(idChar, ".csv", sep="")
  }
  
  
  #concatonate the directory with the file name
  path<- paste(directory, idChar, sep="/")
  
  data <- read.table(path, header=TRUE, sep=",", stringsAsFactors=TRUE)
  
  if(summarize==TRUE){
    print(summary(data))
  }
   
  return(data)
  
}


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
    tempAdd<- c(idCounter, count(complete.cases(getmonitor(idCounter, directory)))[2,2])
    completeDf <- rbind(completeDf, tempAdd)
  }
  names(completeDf)[1]="id"
  names(completeDf)[2]="nobs"
  
  return(completeDf)
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  corData <- vector(mode="integer")
  
  fileCount<-length(list.files(directory, pattern = "\\.csv$"))
  
  for(idCounter in 1:fileCount){
    tempData<-getmonitor(idCounter, directory)
    complete(directory= directory, id = idCounter)
    if(!is.na(complete(directory= directory, id = idCounter)[,"nobs"]) && complete(directory= directory, id = idCounter)[,"nobs"]> threshold){
      corData<- c(corData,cor(tempData[,"sulfate"], tempData[,"nitrate"], use="pairwise.complete.obs"))
    }
  }
  
  return(corData)
} 
