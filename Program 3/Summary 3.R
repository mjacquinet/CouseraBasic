##Part 1
outcome <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], main = "Heart Attack 30-day Death Rate", xlab = "30-day Death Rate")


#Part 2
#Change to numeric
outcome[, 11] <- as.numeric(outcome[, 11])
outcome[, 17] <- as.numeric(outcome[, 17])
outcome[, 23] <- as.numeric(outcome[, 23])

par(mfrow = c(3,1))
median.1 = median(outcome[,11], na.rm=TRUE)
hist(outcome[, 11], main = expression(paste("Heart Attack ", tilde(X), median.1)), xlab="30-day Death Rate", xlim= range(outcome[,11], outcome[,17],outcome[,23], na.rm = TRUE))
abline(v = median.1, untf = FALSE, col = "red")

hist(outcome[, 17], main = "Heart Failure", xlab="30-day Death Rate", xlim= range(outcome[,11], outcome[,17],outcome[,23], na.rm = TRUE))
abline(v = median(outcome[,17], na.rm=TRUE), untf = FALSE, col = "red")
              
hist(outcome[, 23], main = "Pneumonia", xlab="30-day Death Rate", xlim= range(outcome[,11], outcome[,17],outcome[,23], na.rm = TRUE))
abline(v = median(outcome[,23], na.rm=TRUE), untf = FALSE, col = "red")
              
#part 3
table(outcome$State)