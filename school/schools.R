SMU = c(34, 200, 23, 50, 60, 50, 0, 0, 30, 89, 0, 300, 400, 20, 10, 0)
Seattle = c(20, 10, 5, 0, 30, 50, 0, 100, 110, 0, 40, 10, 3, 0)
par(mfrow=c(1,2))
hist(SMU)
hist(Seattle)


## Input the Data

schools = read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/school/SMU/DS 6371/wk1/HW1/schools.csv")
schools


library(tidyverse)
# Read in Data

## Calculate the Observed Difference in Means

xbars = schools %>% group_by(school) %>% summarize(mean = mean(cash))
xbarNminusT = xbars[2,2] - xbars[1,2] # observed difference xbarSMU - xbarSeattle = 52.125
xbarNminusT

## Build the Distribution
# set.seed(2) # So we all get the same randomizations and thus the same values

xbarDiffHolder = numeric(1000)

for (i in 1:1000)
{
  scrambledLabels = sample(schools$school, 30, replace = FALSE) # shuffle the Labels
  
  schoolsTemp = schools
  schoolsTemp$school = scrambledLabels
  
  xbars = schoolsTemp %>% group_by(school) %>% summarize(mean = mean(cash))
  xbarNminusT = xbars[2,2] - xbars[1,2] # observed difference xbarSMU - xbarSeattle
  xbarDiffHolder[i] = xbarNminusT$mean
}

## Calculate the p-value
num_more_extreme = sum(abs(xbarDiffHolder) >= abs(52.125))
pvalue = num_more_extreme / 1000
pvalue

num_more_extreme

# Draw the histogram of permuted mean differences
hist(xbarDiffHolder, 
     main = "Histogram of Permuted Mean Differences",
     xlab = "Permuted Mean Difference",
     ylab = "Frequency",
     col = "lightblue", 
     border = "black")

# Add a line indicating the observed difference
abline(v = 52.125, col = "red", lwd = 2, lty = 2)
legend("topright", legend = "Observed Mean Difference", col = "red", lwd = 2, lty = 2)