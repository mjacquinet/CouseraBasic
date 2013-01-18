##Part 1
outcome <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], main = "Heart Attack 30-day Death Rate", xlab = "30-day Death Rate")


#Part 2
