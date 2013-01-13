directory <- "specdata"
corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  fileCount<-length(list.files(directory, pattern = "\\.csv$"))
  
  for(idCounter in 1:fileCount){
    tempData<-getmonitor(idCounter, directory)
    print(cor(tempData[,"sulfate"], tempData[,"nitrate"], use="pairwise.complete.obs"))
  }
  
  return(invisible())
}


