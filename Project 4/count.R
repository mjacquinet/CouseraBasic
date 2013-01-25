count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if(!(cause %in% c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown"))){
    stop("cause needs to be one of the following: “asphyxiation”, “blunt force”, “other”, “shooting”, “stabbing”, “unknown”")
  }else if(cause=="asphyxiation"){
    causeString <- "[cC]ause: [aA]sphyxiation"
  }else if(cause=="blunt force"){
    causeString <- "[cC]ause: [bB]lunt [fF]orce"
  }else if(cause=="other"){
    causeString <- "[cC]ause: [oO]ther"
  }else if(cause=="shooting"){
    causeString <- "[cC]ause: [sS]hooting"
  }else if(cause=="stabbing"){
    causeString <- "[cC]ause: [sS]tabbing"
  }else if(cause=="unknown"){
    causeString <- "[cC]ause: [uU]nknown"
  }
  
  ## Check that specific "cause" is allowed; else throw error
  ## Read "homicides.txt" data file
  
  homicides <- readLines("homicides.txt")
  
  ## Extract causes of death
  countVector <- grep(causeString, homicides)
  ## Return integer containing count of homicides for that cause
  return(length(countVector))
}