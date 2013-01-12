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