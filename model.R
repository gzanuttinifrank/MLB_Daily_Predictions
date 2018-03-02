setwd("~/Documents/School/Yale/Sports Analytics/MLB 2017 Daily Predictions/Historical Data")

library(randomForest)
library(e1071)
library(caret)

odds2prob <- function(moneyline) {
  if (length(moneyline>1)) {
    winprob <- c()
    for (i in 1:length(moneyline)) {
      if (moneyline[i]<0) {
        winprob <- c(winprob, (moneyline[i]*-1) / (moneyline[i]*-1 + 100))
      } else {
        winprob <- c(winprob, 100 / (moneyline[i] + 100))
      }
    }
  } else {
    if (moneyline<0) {
      winprob <- (moneyline*-1) / (moneyline*-1 + 100)
    } else {
      winprob <- 100 / (moneyline + 100)
    }
  }
  return(winprob)
}


######### HomeWin #########

histdata <- read.csv("HistoricalTable.csv", as.is=TRUE)
histdata[,c(5,6,13,79,11)] <- lapply(histdata[,c(5,6,13,79,11)], factor)

nopitcher <- which(is.na(histdata$AP.Age) | is.na(histdata$HP.Age))
histdata <- histdata[-nopitcher,]

for (i in 1:nrow(histdata)) {
  if (is.na(histdata$ATURFpct[i])) {
    histdata$ATURFpct[i] <- histdata$AW.L.[i]
    histdata$ATURFdiff[i] <- 0
  }
  if (is.na(histdata$HTURFpct[i])) {
    histdata$HTURFpct[i] <- histdata$HW.L.[i]
    histdata$HTURFdiff[i] <- 0
  }
  if (is.na(histdata$AP.SO.W[i])) {
    histdata$AP.SO.W[i] <- histdata$AP.SO[i]/histdata$AP.G[i]
  }
  if (is.na(histdata$HP.SO.W[i])) {
    histdata$HP.SO.W[i] <- histdata$HP.SO[i]/histdata$HP.G[i]
  }
}

histdata$AP.W.L.[is.na(histdata$AP.W.L.)] <- 0
histdata$HP.W.L.[is.na(histdata$HP.W.L.)] <- 0
histdata$AP.ERA.[is.na(histdata$AP.ERA.)] <- 100
histdata$HP.ERA.[is.na(histdata$HP.ERA.)] <- 100


for (i in 1:nrow(histdata)) {
  histdata$Away.prob[i] <- odds2prob(histdata$AwayOdds[i])
  histdata$Home.prob[i] <- odds2prob(histdata$HomeOdds[i])
}


sample.ind <- sample(2, nrow(histdata), replace = T, prob = c(0.9,0.1))
dev <- histdata[sample.ind==1,]
val <- histdata[sample.ind==2,]
table(dev$HomeWin)/nrow(dev)
table(val$HomeWin)/nrow(val)


varNames <- names(dev)

# Exclude ID or Response variable
varNames <- varNames[-c(1:4,7:11,24,26:37,39:43,90,92:103,105:109,234:235)]
#varNames <- varNames[!varNames %in% c("HomeWin","AwayOdds","HomeOdds","Winner")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("HomeWin", varNames1, sep = " ~ "))


rf <- randomForest(rf.form, dev, ntree=600, importance=T, mtry=30)
plot(rf)

varImpPlot(rf, sort = T, main="Variable Importance", n.var=5)

var.imp <- data.frame(importance(rf, type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]



dev$predicted.response <- predict(rf, dev, type="prob")
val$predicted.response <- predict(rf, val, type="prob")


mostconfident <- val[(val$predicted.response[,1]>(val$Away.prob+.1) | val$predicted.response[,2]>(val$Home.prob+.1)) & (val$predicted.response[,1]>.6 | val$predicted.response[,1]<.4),]
mostconfident$finalpred <- mostconfident$predicted.response[,1]<.5
confusionMatrix(data=mostconfident$finalpred, reference=mostconfident$HomeWin, positive='TRUE')



library(ranger)
rf.fivefoldcv <- function(train, form, ntrees, minnodesize, nvars) {
  samp <- sample(cut(seq(1, nrow(train)), breaks=5, labels=FALSE))
  error <- c()
  for (i in 1:5) {
    holdout <- train[samp==i,]
    training <- train[samp!=i,]
    #rf1 <- ranger(form, data=training, num.trees=ntrees, min.node.size=minnodesize, mtry=nvars, write.forest=TRUE)
    rf1 <- randomForest(form, training, ntree=ntrees, importance=T, mtry=nvars, nodesize=minnodesize)
    pred <- predict(rf1, holdout)
    #error <- c(error, (1/nrow(holdout) * sum(holdout$HomeWin != pred$predictions)))
    error <- c(error, (1/nrow(holdout) * sum(holdout$HomeWin != pred)))
  }
  err <- mean(error)
  return(err)
}

mnstest <- c()
for (i in 1:20) {
  set.seed(1012)
  mnstest <- c(mnstest, rf.fivefoldcv(histdata, rf.form, 100, i, 13))
  print(i)
  plot(seq(1,i,1), mnstest)
}
plot(seq(1,20,1), mnstest)
optmns <- 13  # best minnodesize, default is 1 for classification

nvarstest <- c()
for (i in 5:25) {
  set.seed(1012)
  nvarstest <- c(nvarstest, rf.fivefoldcv(histdata, rf.form, 100, optmns, i))
  print(i)
  plot(seq(5,i,1), nvarstest)
}
plot(seq(5,25,1), nvarstest)
which(nvarstest==min(nvarstest))+4
optnvars <- 13  # best mtry

ntreetest <- c()
for (i in 1:10) {
  set.seed(1012)
  ntreetest <- c(ntreetest, rf.fivefoldcv(histdata, rf.form, i*100, optmns, optnvars))
  print(i)
  plot(seq(100,100*i,100), ntreetest)
}
plot(seq(100,1000,100), ntreetest)
optntrees <- 100  # optimal ntrees

set.seed(1012)
rf.fivefoldcv(histdata, rf.form, optntrees, optmns, optnvars)
### best model error rate = 0.3977605


rf.fivefoldcv.prob <- function(train, form, ntrees, minnodesize, nvars, margin) {
  samp <- sample(cut(seq(1, nrow(train)), breaks=5, labels=FALSE))
  error <- c()
  for (i in 1:5) {
    holdout <- train[samp==i,]
    training <- train[samp!=i,]
    rf1 <- randomForest(form, training, ntree=ntrees, importance=T, mtry=nvars, nodesize=minnodesize)
    pred <- predict(rf1, holdout, type="prob")
    # error <- c(error, (1/nrow(holdout) * sum(abs((as.numeric(holdout$HomeWin)-1) - pred[,2]))))
    # error <- c(error, (1/nrow(holdout) * sum((holdout$HomeWin==TRUE & pred[,2]>odds2prob(holdout$HomeOdds)) | (holdout$HomeWin==FALSE & pred[,1]>odds2prob(holdout$AwayOdds)))))
    # error <- c(error, (1/sum(pred[,2]>odds2prob(holdout$HomeOdds) | pred[,1]>odds2prob(holdout$AwayOdds)) * sum((holdout$HomeWin==TRUE & pred[,2]>odds2prob(holdout$HomeOdds)) | (holdout$HomeWin==FALSE & pred[,1]>odds2prob(holdout$AwayOdds)))))
    error <- c(error, (1/sum(pred[,2]>(odds2prob(holdout$HomeOdds)+margin) | pred[,1]>(odds2prob(holdout$AwayOdds)+margin)) * sum((holdout$HomeWin==TRUE & pred[,2]>(odds2prob(holdout$HomeOdds)+margin)) | (holdout$HomeWin==FALSE & pred[,1]>(odds2prob(holdout$AwayOdds)+margin)))))
  }
  err <- mean(error)
  return(err)
}

mnstest.prob <- c()
for (i in 1:20) {
  set.seed(1012)
  mnstest.prob <- c(mnstest.prob, rf.fivefoldcv.prob(histdata, rf.form, 100, i, 13))
  print(i)
  plot(seq(1,i,1), mnstest.prob)
}
plot(seq(1,20,1), mnstest.prob)
which(mnstest.prob==max(mnstest.prob))
optmns.prob <- 1  # best minnodesize, default is 10 for probability, 5 for regression

nvarstest.prob <- c()
for (i in 5:25) {
  set.seed(1012)
  nvarstest.prob <- c(nvarstest.prob, rf.fivefoldcv.prob(histdata, rf.form, 100, optmns.prob, i, .2))
  print(i)
  plot(seq(5,i,1), nvarstest.prob)
}
plot(seq(5,25,1), nvarstest.prob)
which(nvarstest.prob==max(nvarstest.prob))+4
optnvars.prob <- 18  # best mtry

ntreetest.prob <- c()
for (i in 1:10) {
  set.seed(1012)
  ntreetest.prob <- c(ntreetest.prob, rf.fivefoldcv.prob(histdata, rf.form, i*100, optmns.prob, optnvars.prob, .2))
  print(i)
  plot(seq(100,100*i,100), ntreetest.prob)
}
plot(seq(100,1000,100), ntreetest.prob)
optntrees.prob <- 100  # optimal ntrees

set.seed(1012)
rf.fivefoldcv.prob(histdata, rf.form, optntrees.prob, optmns.prob, optnvars.prob)
### best model prop beat odds out of all games = 0.4988618  (3, 19, 300)  (100%)
### best model prop beat odds out of theoretical bets made = 0.5351051  (3, 5, 100)  (~91.5%)
### best model prop beat odds out of theoretical bets made (+0.05) = 0.5480772  (18, 13, 100)  (~58.2%)
### best model prop beat odds out of theoretical bets made (+0.1) = 0.5516414  (5, 20, 100)  (~34.5%)
### best model prop beat odds out of theoretical bets made (+0.15) = 0.5863534  (19, 14, 100)  (~17.6%)
### best model prop beat odds out of theoretical bets made (+0.2) = 0.6335666  (1, 18, 100)  (~8.5%)





#### Check today's predictions ####
histdata <- read.csv("HistTable_wToday.csv", as.is=TRUE)
histdata[,c(5,6,13,79,11)] <- lapply(histdata[,c(5,6,13,79,11)], factor)

nopitcher <- which(is.na(histdata$AP.Age) | is.na(histdata$HP.Age))
histdata <- histdata[-nopitcher,]

for (i in 1:nrow(histdata)) {
  if (is.na(histdata$ATURFpct[i])) {
    histdata$ATURFpct[i] <- histdata$AW.L.[i]
    histdata$ATURFdiff[i] <- 0
  }
  if (is.na(histdata$HTURFpct[i])) {
    histdata$HTURFpct[i] <- histdata$HW.L.[i]
    histdata$HTURFdiff[i] <- 0
  }
  if (is.na(histdata$AP.SO.W[i])) {
    histdata$AP.SO.W[i] <- histdata$AP.SO[i]/histdata$AP.G[i]
  }
  if (is.na(histdata$HP.SO.W[i])) {
    histdata$HP.SO.W[i] <- histdata$HP.SO[i]/histdata$HP.G[i]
  }
}

histdata$AP.W.L.[is.na(histdata$AP.W.L.)] <- 0
histdata$HP.W.L.[is.na(histdata$HP.W.L.)] <- 0
histdata$AP.ERA.[is.na(histdata$AP.ERA.)] <- 100
histdata$HP.ERA.[is.na(histdata$HP.ERA.)] <- 100


for (i in 1:nrow(histdata)) {
  histdata$Away.prob[i] <- odds2prob(histdata$AwayOdds[i])
  histdata$Home.prob[i] <- odds2prob(histdata$HomeOdds[i])
}


traindata <- histdata[!is.na(histdata$Winner),]
todaysgames <- histdata[is.na(histdata$Winner),]

sample.ind <- sample(2, nrow(traindata), replace = T, prob = c(0.6,0.4))
dev <- traindata[sample.ind==1,]
val <- traindata[sample.ind==2,]
table(dev$HomeWin)/nrow(dev)
table(val$HomeWin)/nrow(val)


varNames <- names(dev)

# Exclude ID or Response variable
varNames <- varNames[-c(1:4,7:11,24,26:37,39:43,90,92:103,105:109,234:235)]
#varNames <- varNames[!varNames %in% c("HomeWin","AwayOdds","HomeOdds","Winner")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("HomeWin", varNames1, sep = " ~ "))


rf <- randomForest(rf.form, dev, ntree=5000, importance=T, mtry=30)


dev$predicted.response <- predict(rf, dev, type="prob")
val$predicted.response <- predict(rf, val, type="prob")

todaysgames$predicted.response <- predict(rf, todaysgames, type="prob")

mostconfident <- todaysgames[(todaysgames$predicted.response[,1]>(todaysgames$Away.prob+.1) | todaysgames$predicted.response[,2]>(todaysgames$Home.prob+.1)) & (todaysgames$predicted.response[,1]>.6 | todaysgames$predicted.response[,1]<.4),]
mostconfident$finalpred <- mostconfident$predicted.response[,1]<.5
for (i in 1:nrow(mostconfident)) {
  if (mostconfident$finalpred[i]==TRUE) {
    mostconfident$finalpred[i] <- mostconfident$HomeTeam[i]
  } else {
    mostconfident$finalpred[i] <- mostconfident$AwayTeam[i]
  }
}






######### Rundiff #########

histdata <- read.csv("HistoricalTable.csv", as.is=TRUE)
histdata[,c(5,6,13,79,11)] <- lapply(histdata[,c(5,6,13,79,11)], factor)

nopitcher <- which(is.na(histdata$AP.Age) | is.na(histdata$HP.Age))
histdata <- histdata[-nopitcher,]

for (i in 1:nrow(histdata)) {
  if (is.na(histdata$ATURFpct[i])) {
    histdata$ATURFpct[i] <- histdata$AW.L.[i]
    histdata$ATURFdiff[i] <- 0
  }
  if (is.na(histdata$HTURFpct[i])) {
    histdata$HTURFpct[i] <- histdata$HW.L.[i]
    histdata$HTURFdiff[i] <- 0
  }
  if (is.na(histdata$AP.SO.W[i])) {
    histdata$AP.SO.W[i] <- histdata$AP.SO[i]/histdata$AP.G[i]
  }
  if (is.na(histdata$HP.SO.W[i])) {
    histdata$HP.SO.W[i] <- histdata$HP.SO[i]/histdata$HP.G[i]
  }
}

histdata$AP.W.L.[is.na(histdata$AP.W.L.)] <- 0
histdata$HP.W.L.[is.na(histdata$HP.W.L.)] <- 0
histdata$AP.ERA.[is.na(histdata$AP.ERA.)] <- 100
histdata$HP.ERA.[is.na(histdata$HP.ERA.)] <- 100


for (i in 1:nrow(histdata)) {
  histdata$Away.prob[i] <- odds2prob(histdata$AwayOdds[i])
  histdata$Home.prob[i] <- odds2prob(histdata$HomeOdds[i])
}


sample.ind <- sample(2, nrow(histdata), replace = T, prob = c(0.6,0.4))
dev <- histdata[sample.ind==1,]
val <- histdata[sample.ind==2,]
table(dev$HomeWin)/nrow(dev)
table(val$HomeWin)/nrow(val)


varNames <- names(dev)

# Exclude ID or Response variable
varNames <- varNames[-c(1:4,7:11,24,26:37,39:43,90,92:103,105:109,234:235)]
#varNames <- varNames[!varNames %in% c("HomeWin","AwayOdds","HomeOdds","Winner")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("Rundiff", varNames1, sep = " ~ "))


rf <- randomForest(rf.form, dev, ntree=500, importance=T, mtry=30)

plot(rf)

varImpPlot(rf, sort = T, main="Variable Importance", n.var=5)

var.imp <- data.frame(importance(rf, type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]



dev$predicted.response <- predict(rf, dev)
val$predicted.response <- predict(rf, val)


# convert val$predicted.response distribution to probabilites and then subset - not perfect because range of
# probabilities should be smaller (no team should have 90% prob of winning)
sd <- sd(val$predicted.response)
val$pred.prob <- pnorm(val$predicted.response, 0, sd)

mostconfident <- val[val$pred.prob>(val$Home.prob+.3) | val$pred.prob<(val$Away.prob-.3),]
mostconfident$finalpred <- mostconfident$predicted.response>0
confusionMatrix(data=mostconfident$finalpred, reference=mostconfident$HomeWin, positive='TRUE')



#### Check today's predictions ####
histdata <- read.csv("HistTable_wToday.csv", as.is=TRUE)
histdata[,c(5,6,13,79,11)] <- lapply(histdata[,c(5,6,13,79,11)], factor)

nopitcher <- which(is.na(histdata$AP.Age) | is.na(histdata$HP.Age))
histdata <- histdata[-nopitcher,]

for (i in 1:nrow(histdata)) {
  if (is.na(histdata$ATURFpct[i])) {
    histdata$ATURFpct[i] <- histdata$AW.L.[i]
    histdata$ATURFdiff[i] <- 0
  }
  if (is.na(histdata$HTURFpct[i])) {
    histdata$HTURFpct[i] <- histdata$HW.L.[i]
    histdata$HTURFdiff[i] <- 0
  }
  if (is.na(histdata$AP.SO.W[i])) {
    histdata$AP.SO.W[i] <- histdata$AP.SO[i]/histdata$AP.G[i]
  }
  if (is.na(histdata$HP.SO.W[i])) {
    histdata$HP.SO.W[i] <- histdata$HP.SO[i]/histdata$HP.G[i]
  }
}

histdata$AP.W.L.[is.na(histdata$AP.W.L.)] <- 0
histdata$HP.W.L.[is.na(histdata$HP.W.L.)] <- 0
histdata$AP.ERA.[is.na(histdata$AP.ERA.)] <- 100
histdata$HP.ERA.[is.na(histdata$HP.ERA.)] <- 100


for (i in 1:nrow(histdata)) {
  histdata$Away.prob[i] <- odds2prob(histdata$AwayOdds[i])
  histdata$Home.prob[i] <- odds2prob(histdata$HomeOdds[i])
}


traindata <- histdata[!is.na(histdata$Winner),]
todaysgames <- histdata[is.na(histdata$Winner),]

sample.ind <- sample(2, nrow(traindata), replace = T, prob = c(0.6,0.4))
dev <- traindata[sample.ind==1,]
val <- traindata[sample.ind==2,]
table(dev$HomeWin)/nrow(dev)
table(val$HomeWin)/nrow(val)


varNames <- names(dev)

# Exclude ID or Response variable
varNames <- varNames[-c(1:4,7:11,24,26:37,39:43,90,92:103,105:109,234:235)]
#varNames <- varNames[!varNames %in% c("HomeWin","AwayOdds","HomeOdds","Winner")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("Rundiff", varNames1, sep = " ~ "))


rf <- randomForest(rf.form, dev, ntree=500, importance=T, mtry=30)


dev$predicted.response <- predict(rf, dev)
val$predicted.response <- predict(rf, val)

todaysgames$predicted.response <- predict(rf, todaysgames)


# stdev <- sd(val$predicted.response)
stdev <- sd(histdata$Rundiff, na.rm=TRUE)
# val$pred.prob <- pnorm(val$predicted.response, 0, stdev)

for (i in 1:nrow(todaysgames)) {
  # todaysgames$pred.prob[i] <- sum(val$predicted.response < todaysgames$predicted.response[i]) / nrow(val)
  todaysgames$pred.prob[i] <- pnorm(todaysgames$predicted.response[i], mean(histdata$Rundiff, na.rm=TRUE), stdev)
}

todaysgames$finalpred <- todaysgames$predicted.response>0
for (i in 1:nrow(todaysgames)) {
  if (todaysgames$finalpred[i]==TRUE) {
    todaysgames$finalpred[i] <- todaysgames$HomeTeam[i]
  } else {
    todaysgames$finalpred[i] <- todaysgames$AwayTeam[i]
  }
}

mostconfident <- todaysgames[todaysgames$pred.prob>(todaysgames$Home.prob+.1) | todaysgames$pred.prob<(1-todaysgames$Away.prob-.1),]
# mostconfident$finalpred <- mostconfident$predicted.response>0

for (i in 1:nrow(mostconfident)) {
  # if (mostconfident$finalpred[i]==TRUE) {
  #   mostconfident$finalpred[i] <- mostconfident$HomeTeam[i]
  # } else {
  #   mostconfident$finalpred[i] <- mostconfident$AwayTeam[i]
  # }
  if (mostconfident$pred.prob[i]>mostconfident$Home.prob[i]) {
    mostconfident$betTeam[i] <- mostconfident$HomeTeam[i]
  } else {
    mostconfident$betTeam[i] <- mostconfident$AwayTeam[i]
  }
}

View(todaysgames[,c(1:4,7:8,234:238)])
View(mostconfident[,c(1:4,7:8,234:239)])



######### GLM ##########

baseball.glm <- glm(rf.form, family=binomial(link='logit'), data=histdata)




######### Linear Regression #########

