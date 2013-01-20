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
tempState <- data.frame(table(outcome$State))
temp<-subset(outcome, table(outcome$State) > 20)
tempState<-subset(tempState, Freq > 20, select = Var1)
tempState<- sapply(tempState, as.character)
#forced the subset of states to be a character
##subsetStates<-c("AL","AR","AZ","CA","CO","CT","FL","GA","IA","ID","IL","IN","KS","KY","LA", "MA"
##                ,"MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH"
##                ,"OK","OR","PA","PR","SC","SD","TN","TX","UT","VA","WA","WI","WV","WY")


outcome2 <- subset(outcome, State %in% tempState)


death<-outcome2[,11]
state<-outcome2$State
par(mfrow = c(1,1))
boxplot(death ~ state, ylab="30-day Death Rate", main="Heart Attack 30-day Death Rate by State")

#Part 4