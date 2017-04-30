# To test whether coming in first place at the box office has a lasting effect on box office revenue


#Part1 - Causal Regression
#Load the data 
boxofficeDB = read.csv("~/Document/boxofficeDB.csv")

#Remove number 0 in the column of subsequentEarnings
#First, build a newDataFrame with columns"subsequentEarnings" and "releaseRank"
newDataFrame = boxofficeDB[,c("subsequentEarnings", "releaseRank")]
newDataFrame = newDataFrame[newDataFrame$subsequentEarnings >0,]

#Run a linear regression that uses releasesRank to predict the log of subsequentEarnings
basicLM = lm(log(newDataFrame$subsequentEarnings)~newDataFrame$releaseRank, data = newDataFrame)
basicLM
summary(basicLM)


#Part2 - Data Cleaning
#Find each week's top release movie in each "releaseDate" 
releaseWeekEarningsBig = setNames(aggregate(boxofficeDB$releaseWeekEarnings ~ boxofficeDB$releaseDate, boxofficeDB, function(x) sort(x, decreasing=T)[1]), c("releaseDate","releaseWeekEarnings"))

#Join homeWork3DB to releaseWeekEarningsBig 
releaseWeekEarningsBig = merge(releaseWeekEarningsBig, boxofficeDB,by=c("releaseWeekEarnings","releaseDate"))

#Find each week's second highest release movie in each "releaseDate" 
releaseWeekEarningsSmall = setNames(aggregate(boxofficeDB$releaseWeekEarnings ~ boxofficeDB$releaseDate, boxofficeDB, function(x) sort(x, decreasing=T)[2]), c("releaseDate","releaseWeekEarnings"))

#Join homeWork3DB to releaseWeekEarningsSmall
releaseWeekEarningsSmall = merge(releaseWeekEarningsSmall, boxofficeDB,by=c("releaseWeekEarnings","releaseDate"))

#Combine releaseWeekEarningsBig and releaseWeekEarningsSmall
releaseWeekEarningsCombine = rbind(releaseWeekEarningsBig, releaseWeekEarningsSmall)

#Sort each "releaseWeekEarnings" in each "releaseDate" by ascending order
sortingDF = releaseWeekEarningsCombine[order(as.Date(releaseWeekEarningsCombine$releaseDate, format ="%m/%d/%Y"), releaseWeekEarningsCombine$releaseWeekEarnings),]

#Calculate "firstWeekBoxOfficeDiff" and "laterLogBoxOfficeDiff"
answer = aggregate(cbind(releaseWeekEarnings= sortingDF$releaseWeekEarnings, subsequentEarnings=sortingDF$subsequentEarnings) ~ sortingDF$releaseDate, sortingDF, function(x) log(x[2]) - log(x[1]))

#Remove rows with infinite
answer = answer[!(is.infinite(answer$subsequentEarnings)),]

#Remove rows with NA
answer = answer[!(is.na(answer$subsequentEarnings)),]

#Build a data frame with columns of "date", "firstWeekOfficeDiff" and "laterLogBoxOfficeDiff"
finalDataFrame = setNames(answer, c("date","firstWeekBoxOfficeDiff","laterLogBoxOfficeDiff"))

#Save R workspace
save.image(file = "hw3.RData")


#Part3 - Regression Discontinuity
#(b)Find the threshold for 'close' in firstWeekBoxOfficeDiff that is < 0.1
thresholdDF = subset(finalDataFrame, firstWeekBoxOfficeDiff<0.1)

#(c)Find the treatment group and controp group (Nim's Island and Leatherheads)
boxofficeDB[boxofficeDB$releaseDate == "4/4/2008",]

#(d)Use the t-test to check if the difference is significant
t.test(thresholdDF$laterLogBoxOfficeDiff)




