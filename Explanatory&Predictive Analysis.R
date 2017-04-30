# Explain and predict how highly a consumer will rate movie Rocky 5 given the ratings of the other Rocky movie series.


#a)Load the rating data into R and save as a data frame
#Load the data and set the working directory
setwd("~/Downloads")
recommendDB = read.csv("recommendDB.csv")


#b)Define a data frame that has a row for each consumer, and 5 columns. Fill it with NA
#Data cleaning
#Remove duplicate consumerIDs by unique function
newConsumerID = unique(recommendDB$consumerID)


#Build a matrix and a data frame
movieMatrix = matrix(data = c(NA), nrow = length(unique(recommendDB$consumerID)), ncol = 6)
movieDataFrame = data.frame(movieMatrix)


#Name each of the column name in movieDataFrame
names(movieDataFrame)[1] = "consumerID"
for(i in 2:6)
{
  names(movieDataFrame)[i] = paste("rocky", i-1, sep = "")
}


#c)Fill the data frame from part (b)
#Fill consumerID, rating and rockyID into movieDataFrame
for(j in 1: length(newConsumerID))
{
  movieDataFrame$consumerID[j] = newConsumerID[j]
  ratingData = subset(recommendDB,recommendDB$consumerID==newConsumerID[j],select = c(rating,rockyID))
  for(k in 1: length(ratingData$rating))
  {
    if(ratingData$rockyID[k] == 1)
    {
      movieDataFrame$rocky1[j] = ratingData$rating[k] 
    }
    if(ratingData$rockyID[k] == 2){
      movieDataFrame$rocky2[j] = ratingData$rating[k] 
    }    
    if(ratingData$rockyID[k] == 3){
      movieDataFrame$rocky3[j] = ratingData$rating[k] 
    } 
    if(ratingData$rockyID[k] == 4){
      movieDataFrame$rocky4[j] = ratingData$rating[k] 
    } 
    if(ratingData$rockyID[k] == 5){
      movieDataFrame$rocky5[j] = ratingData$rating[k] 
    }
  }
}


#d)Save your data frame to a variable called "finalDB". Save your workspace to a file called"submit.Rdata"
finalDB = movieDataFrame 
save.image(file = "submit.RData")


#Part2
#a)Compare the correlations between the ratings of each of the five movies
ratingCor = cor(movieDataFrame[,2:6], use = "complete.obs")
ratingCor


#b)Find the mean rating of each movie
rocky1Mean = mean(movieDataFrame$rocky1, na.rm = TRUE)
rocky2Mean = mean(movieDataFrame$rocky2, na.rm = TRUE)
rocky3Mean = mean(movieDataFrame$rocky3, na.rm = TRUE)
rocky4Mean = mean(movieDataFrame$rocky4, na.rm = TRUE)
rocky5Mean = mean(movieDataFrame$rocky5, na.rm = TRUE)
rockyMeanDataFrame.1 = data.frame(rocky1Mean, rocky2Mean, rocky3Mean, rocky4Mean, rocky5Mean)
rockyMeanDataFrame.1


#Part2(c)
#Create a subset from MovieDataFrame that only contains consumers who rated rocky4
rocky4Rating = subset(movieDataFrame, !is.na(movieDataFrame[,5]), select = c("rocky1", "rocky2", "rocky3", "rocky4", "rocky5"))
rocky1RatingMean = mean(rocky4Rating$rocky1, na.rm = TRUE)
rocky2RatingMean = mean(rocky4Rating$rocky2, na.rm = TRUE)
rocky3RatingMean = mean(rocky4Rating$rocky3, na.rm = TRUE)
rocky4RatingMean = mean(rocky4Rating$rocky4, na.rm = TRUE)
rocky5RatingMean = mean(rocky4Rating$rocky5, na.rm = TRUE)
rockyMeanDataFrame.2 = data.frame(rocky1RatingMean, rocky2RatingMean, rocky3RatingMean, rocky4RatingMean, rocky5RatingMean)
rockyMeanDataFrame.2


#Part3
#a)Figure out what each line does by printing the variables
#Create vectors with two predictors; one with rating and one with squared weight
rocky1Vec = c('rocky1','I(rocky1^2)') 
rocky2Vec = c('rocky2','I(rocky2^2)')
rocky3Vec = c('rocky3','I(rocky3^2)')
rocky4Vec = c('rocky4','I(rocky4^2)')


#Each vector has 2 possible combinations, such as rocky1 with rocky1 or rocky1 with I(rocky1^2)
#There are 16(2^4) possibile combinations
fullSet = expand.grid(rocky1Vec,rocky2Vec,rocky3Vec,rocky4Vec)


#Print out each regression model that we are going to run
formulaSet = paste("rocky5 ~",apply(fullSet,1,paste,collapse='+'))


#Run each of the regression and print out the results
for(i in 1:nrow(fullSet)){ 
  print(lm(as.formula(formulaSet[i]),data=movieDataFrame))
}


#b)Treat rocky1 as a categorical variable
rocky1Vec1 = c('rocky1','I(rocky1^2)','as.factor(rocky1)')
rocky2Vec2 = c('rocky2','I(rocky2^2)')
rocky3Vec3 = c('rocky3','I(rocky3^2)')
rocky4Vec4 = c('rocky4','I(rocky4^2)')


fullSet1 = expand.grid(rocky1Vec1,rocky2Vec2,rocky3Vec3,rocky4Vec4) 
formulaSet1 = paste("rocky5 ~",apply(fullSet1,1,paste,collapse='+'))


for(i in 1:nrow(fullSet1)){ 
  print(lm(as.formula(formulaSet1[i]),data=movieDataFrame))
}


#c)AIC and BIC for each model
AIC = c()
BIC = c()
for(m in 1:nrow(fullSet)){
  basicLM = lm(as.formula(formulaSet[m]), movieDataFrame)
  AIC[m] = AIC(basicLM)
  BIC[m] = BIC(basicLM)
}


AIC1 = c()
BIC1 = c()
for(n in 1:nrow(fullSet1)){
  basicLM = lm(as.formula(formulaSet1[n]), movieDataFrame)
  AIC1[n] = AIC(basicLM)
  BIC1[n] = BIC(basicLM)
}


#d)Creates a random hold out sample with 10% of the data
# Obtain a random order
set.seed(123456)
randOrder = order(runif(nrow(movieDataFrame)))


# Create the hold of sample and the out of sample 
trainingData = subset(movieDataFrame,randOrder < .9 * nrow(movieDataFrame))
validationData = subset(movieDataFrame,randOrder >= .9 * nrow(movieDataFrame))

# Calculate the out of sample MSE of each model
MSE = c()
for(i in 1:nrow(fullSet1)){ 
  linearLM = lm(as.formula(formulaSet1[i]),data = trainingData)
  # calculate MSE 
  MSE[i] = mean((validationData$rocky5 - predict(linearLM,validationData))^2, na.rm = TRUE)
}


#e)Which model performs best on each of these metrics (AIC, BIC, out of sample MSE)?
#AIC:12111.27; Model No.10 performs best on AIC metrics
#BIC:12149.57; Model No.10 performs best on BIC metrics
#MSE:0.8359549; Model No.9 performs best on MSE metrics

