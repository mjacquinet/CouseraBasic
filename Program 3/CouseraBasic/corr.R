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
  
  if(fileCount>1){
    compDf<-complete(directory= directory, id=1:fileCount)
    
    for(idCounter in 1:fileCount){
      tempData<-getmonitor(idCounter, directory)
      
      if(!is.na(compDf[idCounter,"nobs"]) && compDf[idCounter,"nobs"]> threshold){
        corData<- c(corData,cor(tempData[,"sulfate"], tempData[,"nitrate"], use="pairwise.complete.obs"))
      }
    }
  }
  
  return(corData)
} 