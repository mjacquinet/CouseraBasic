agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  ## Extract ages of victims; ignore records where no age is
  ## given
  age <- paste0(" ",age, " ")
  agePosition <- regexpr("[yY]ears [Oo]ld", homicides)
  ageNum <- substr(homicides, agePosition-4, agePosition-1)
  countVector <- grep(age, ageNum)
  
  ## Return integer containing count of homicides for that age
  return(length(countVector))
}

agecount(3)