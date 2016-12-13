rm(list=ls())
# To replicate my output file, follow these steps:
# (0) download the files train.csv, sample_submission_2.csv, and test_2.csv from https://www.kaggle.com/c/the-winton-stock-market-challenge/data
# (1) un-comment the line that starts with: setwd("/YourPathHere")
# (2) in the setwd() replace "/YourPathHere" with the path to the archive
# (3) name your training data CSV file "train.csv"
# (4) name your test data CSV file "test_2.csv"
# (5) name your sample submission file "sample_submission_2.csv"
# (6) run all of the code in this file

# setwd("/YourPathHere")

xtrain = read.csv("train.csv")
xtest  = read.csv("test_2.csv")
xsubmit = read.csv("sample_submission_2.csv")

########################################################################
# Data cleaning and a little bit of feature transforming
########################################################################
# For now, just put in a zero for all of the NA values
for (i in 2:211){
  xtrain[,i][is.na(xtrain[,i])] <- 0
  if (i<=147){
    xtest[,i][is.na(xtest[,i])] <- 0
  }
}

MinuteReturns.train = data.frame(xtrain[,29:207])
MinuteReturns.test  = data.frame(xtest[,29:147])

RowVar <- function(x) {
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}

# Construct a few new features and add them to the test and training data sets
# These new features are:
# (1) a scaled proxy for weight (Feature_13 is correlated with weight in training data)
# (2) variance of the one-minute returns, a measure of volatility
# (3) sum of the one-minute returns, a proxy for intraday price movement
w.factor = (xtrain$Feature_13/max(xtrain$Feature_13))
MinuteVar = RowVar(MinuteReturns.train)
MinuteSums = rowSums(MinuteReturns.train)
xtrain = data.frame(xtrain, MinuteSums, MinuteVar, w.factor)

w.factor = (xtest$Feature_13/max(xtest$Feature_13))
MinuteVar = RowVar(MinuteReturns.test)
MinuteSums = rowSums(MinuteReturns.test)
xtest  = data.frame(xtest, MinuteSums, MinuteVar, w.factor)

factorvec = (1:10000)/10
scalefactorvec = rep(0,60)
weightvec = xtrain$w.factor
weightvec.test = xtest$w.factor

#################################################################
# Let's do some training models
#################################################################

attach(xtrain)

daily.model.1 = lm(Ret_PlusOne ~ Ret_MinusOne + Ret_MinusTwo + MinuteSums + MinuteVar, weights=Weight_Daily, data=xtrain)
daily.model.2 = lm(Ret_PlusTwo ~ Ret_MinusOne + Ret_MinusTwo + MinuteSums, weights=Weight_Daily, data=xtrain)

######################################################
# Let's do the predictions now
######################################################

# Create a blank matrix for the predictions, whose values we'll fill in later
predictions = data.frame(matrix(nrow=nrow(xtest), ncol=62))

# Predict the one-minute returns
for (i in 1:length(scalefactorvec)){
  vy = paste("Ret_", i+120, sep="")
  theprediction = median(get(vy))
  predictions[,i] = 1*(1/(1+weightvec.test)^2)*theprediction
}

# Now predict the daily returns
daily.pred.1 = 0.8*predict(daily.model.1, newdata=xtest) + 0.2*median(xtrain$Ret_PlusOne)
daily.pred.2 = 0.15*predict(daily.model.2, newdata=xtest) + 0.85*median(xtrain$Ret_PlusTwo)

# Try winsorizing the return predictions at this point
winsorize = function(x, z){
  extrema = quantile(x, c(z, 1-z))
  x[x<extrema[1]] <- extrema[1]
  x[x>extrema[2]] <- extrema[2]
  x
}
daily.pred.1 = winsorize(daily.pred.1, 0.001)
daily.pred.2 = winsorize(daily.pred.2, 0.001)

predictions[, 61] <- daily.pred.1
predictions[, 62] <- daily.pred.2

# This step takes my matrix and re-shapes it for the CSV file to be submitted
answervec = as.vector(rep(NA, nrow(xtest)*62))
for (m in 1:62){
  answervec[ seq(m, length(answervec), 62)  ] <- predictions[,m]
  print(m)
}

#######################################################
# Let's write the output file
#######################################################

xsubmit$Predicted <- answervec
write.csv(xsubmit, file="xsubmit21_FromFurchRepo.csv", row.names=FALSE)

