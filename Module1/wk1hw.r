library(ggplot2)
library(plyr)
library(dplyr)

setwd("C:/CUNY/IS608")

##### Q1 #####

rawData = read.csv("inc5000_data.csv", header = TRUE) 

bp = ggplot(rawData, aes(x = State)) + geom_bar() + coord_flip()

flevels = levels(rawData$State)
flevels = rev(flevels)

bp + scale_x_discrete(limits=flevels) + ggtitle("Fastest Growing Companies by State")

##### Q2 #####

# filter out incomplete cases
rawData = rawData[complete.cases(rawData),]

# determine the state with the 3rd largest number of companies in the dataset
byState = rawData %>%
  group_by(State) %>% 
  summarise(total = n()) %>%
  arrange(desc(total))

targetState = sapply(temp[3,1], as.character)

# Note, state with 3rd largest number of fast growing companies is NY. 

#prune outliers
byTargetState = rawData %>%
  filter(State == targetState & Employees < 2000)

bp2 = ggplot(byTargetState, aes(x=Industry, y=Employees)) + geom_boxplot() + coord_flip()

bp2 + ggtitle("Employment by Industry in NY")

# Extra:  The below would give us a similar look in a tabular view.
byTargetState = rawData %>%
  filter(State == targetState) %>%
  group_by(Industry) %>% 
  summarise(NumCompanies = n(), TotalEmployed = sum(Employees), AvgEmployed = mean(Employees), MaxEmployed = max(Employees), MinEmployed = min(Employees)) %>%
  arrange(desc(NumCompanies))

##### Q3 #####

# Assumption: We're still looking specifically at New York companies for this section.  We'll also continue to ignore the outliers as dubious as that
# may be. (They'd be simple enough to add back in to this section)

byTargetState = rawData %>%
  filter(State == targetState) %>%
  group_by(Industry) %>% 
  summarise(NumCompanies = n(), TotalEmployed = sum(Employees), TotalRev = sum(Revenue)) %>%
  arrange(desc(NumCompanies))

byTargetState$revPerEmp = byTargetState$TotalRev / byTargetState$TotalEmployed
byTargetState = byTargetState %>% arrange(desc(revPerEmp))

flevels <- with(byTargetState, reorder(revPerEmp, revPerEmp, function(x) -length(x)))

byTargetState$Ind2 <- reorder(byTargetState$Industry, byTargetState$revPerEmp)
  
bp3 = ggplot(byTargetState, aes(y = revPerEmp))
bp3 + geom_bar(aes(x=Ind2), data=byTargetState, stat="identity") + coord_flip() + scale_x_discrete(name="") + scale_y_continuous(name="Revenue Per Employee ($)") + ggtitle("Revenue per Employee by Industry in NY")


