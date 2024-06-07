#R Programming Assignment # 3
#Author: Isaac Cormier
#Date: 2024/05/24

## Plot the 30 day mortality rates for heart attack
#Load data and review
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors = FALSE)
head(outcome)
ncol(outcome)
nrow(outcome)
str(outcome)
names(outcome)

#run historgram on 30-day death rates from heart attack
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], xlab = "Death Rates", main = "Histogram of 30-Day Death Rates from Heart Attack")

