library(ggplot2)
library(bigvis)
library(plyr)

# load data, combine into one dataframe
siData <- read.csv("data/SI.csv")
mnData <- read.csv("data/MN.csv")
bkData <- read.csv("data/BK.csv")
bxData <- read.csv("data/BX.csv")
qnData <- read.csv("data/QN.csv")
allData <- rbind(siData, mnData, bkData, bxData, qnData)

# clean up data.  Not interested in anything before 1850 (as per the assignment spec) 
# or entries with 0 or negative floor counts (assume erroneous)
# ignore columns (the majority of the columns) we don't need
builtFar <- allData$BuiltFAR[allData$YearBuilt > 1850 & allData$NumFloors > 0]
numFloors <- allData$NumFloors[allData$YearBuilt > 1850 & allData$NumFloors > 0]
yearBuilt <- allData$YearBuilt[allData$YearBuilt > 1850 & allData$NumFloors > 0]
assessTot <- allData$AssessTot[allData$YearBuilt > 1850 & allData$NumFloors > 0]

# Q1

# use big vis to condense on year built
condYear <- condense(bin(yearBuilt, 1))

# a running total of % of total built should allow a decision maker to make a call
# on what the cutoff date should be
tot <- sum(condYear$.count)
condYear$percBuilt <- cumsum(condYear$.count)/tot
ggplot(condYear, aes(x= yearBuilt, y=percBuilt)) + geom_line() +ylab('Cummulative % Built') + ggtitle("% Built By Year")

# Check Data for weirdness
# The following plot shows that the data gathered before ~ 1980 was not done consistently each year
# and/or it was estimated by some unknown method
summary(yearBuilt)
condYear2 <- condense(bin(yearBuilt, 1))
autoplot(condYear2) + ggtitle("Num Built By Year")

# Q2

breaks = c(0, 20, 200, 2000, 20000)

# We'll set our floor bin at 10 as we're interested in cutoffs at 20, 30 and 40 floors
# The default grid lines provide an easy visual for these cutoffs
yrFloorCond <- condense(bin(yearBuilt, 1), bin(numFloors, 10))
plot <- autoplot(yrFloorCond) + theme(panel.background=element_rect(fill='white')) + ylim(0,80)
plot + scale_fill_gradient(limits= c(1,250000),
                        low='grey',
                        high='red',
                        trans="log",
                        breaks = breaks) + 
                        ggtitle("NumFloors vs Year")

# Q3
# per floor value is simple ratio (for our purposes)
perFloorVal <- assessTot/numFloors

# condense on year and floor value. 
# There does seem to be a lull in value per floor during WWII
valPerFloor <- condense(bin(yearBuilt,5), z =perFloorVal)
autoplot(valPerFloor) +xlab('') + ylab('$ per floor') + xlim(1915,1965)

# However the lull seems more likely to be random variation once we zoom out.
# No conclusion drawn strictly from the data visualizations in this exercise.
autoplot(valPerFloor) +xlab('') + ylab('$ per floor') 


