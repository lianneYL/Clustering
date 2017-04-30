library(AlgDesign); #load the library AlgDesign


set.seed(123)
levels.design= c(2,2,3,4)
f.design <- gen.factorial(levels.design,factors="all")
#select the best design given main effects are all that is to be estimated
fract.design <- optFederov(frml=~X1+X2+X3+X4,data=f.design,nTrials=12,approximate=FALSE) 


###############################################################
##Regression Analysis for getting part-worth estimates
##list the objects and make sense of them
ls()


# Regression
summary(lm(ratings~desmat))


# Priori segment
# Age
summary(lm(ratings~desmat*ageD))
# Gender
summary(lm(ratings~desmat*genderD))
# If significant, run separately for two categories
# Age is statistically significant
summary(lm(ratings~desmat,subset=ageD==1))
summary(lm(ratings~desmat,subset=ageD==0))


# Run the regression for each individual
desmatf = cbind(rep(1,nrow(desmat)),desmat) 
partworths = matrix(nrow = sampsize, ncol = ncol(desmatf))
for(i in 1:sampsize){ 
  partworths[i,] = lm(ratings ~ desmat, subset = ID ==i)$coef
}


# segmenting individuals
library(cluster)
library(fpc)
set.seed(123456) # Set random number seed 


# Cluster Analysis
toclust = partworths
pm1 = pamk(toclust,scaling=TRUE)


wss = (nrow(toclust)-1)*sum(apply(toclust,2,var))
for (i in 2:15) wss[i] = sum(kmeans(toclust, 
                                    centers=i)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")


km1 = kmeans(toclust,3,iter.max = 20, nstart=2)
km2 = kmeans(toclust,2,iter.max = 20, nstart=2)
percsize = paste(1:2," = ",format(km2$size/sum(km2$size)*100,digits=2),"%",sep="")
pie(km2$size,labels=percsize)


clusplot(toclust, km2$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0) #plot clusters against principal components


plotcluster(toclust, km2$cluster) #plot against discriminant functions ()


# Predicting missing cells (preparing for market simulation)
# Repeat individual level partworths for multiplication
partworths.full = matrix(rep(partworths,each=16),ncol=5)
pratings = rowSums(desmatf*partworths.full)
finalratings = ifelse(is.na(ratings),pratings,ratings) #combining actual when available and predicted ratings


# Market simulation
# A scenario is a set of products, each with a set of levels. 
scen0 = c(1,9)
scen1 = c(1,9,3)
scen2 = c(2,9,3)


# Market simulations
# Tranform final ratings into matrix
simDecInput = matrix(finalratings,nrow=nprofiles)


simDec = function(inputmat,scen){
  inmkt = inputmat[scen,]
  max = apply(inmkt,2,max)
  firstChoices = (inmkt==rep(max,each=length(scen)))
  shares = firstChoices/rep(colSums(firstChoices),each=length(scen))
  rowMeans(shares)
}
simDec0 = simDec(simDecInput,scen0)
simDec1 = simDec(simDecInput,scen1)
simDec2 = simDec(simDecInput,scen2)


simProfit = function(inputmat,scen, myProds, prices, vcosts,fcosts,mktsize=1){
  mktshr = simDec(inputmat,scen);
  vprofit = mktshr * (prices-vcosts)*mktsize;
  sum(vprofit[myProds])-fcosts
}
simProf0 = simProfit(simDecInput,scen0,c(1),c(139,119),c(99,89),20000,2000)
simProf1 = simProfit(simDecInput,scen1,c(1,3),c(139,119,119),c(99,89,89),40000,2000)
simProf2 = simProfit(simDecInput,scen2,c(1,3),c(139,119,119),c(99,89,89),40000,2000)


