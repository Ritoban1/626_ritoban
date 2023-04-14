traindata<-read.table("training_data.txt",header = TRUE)
testdata<-read.table("test_data.txt",header = TRUE)
testdata=testdata[,c(2:ncol(testdata))]
library(tidyverse)
library(e1071)
library(SuperLearner)


traindata$response=ifelse((traindata$activity %in% c(1,2,3)),1,0)



### model logistic
Xtrain=traindata[,c(3:(ncol(traindata)-1))]
traindata=traindata[,c(2:ncol(traindata))]
Ytrain=traindata$response
modellogisitic=glm(Ytrain~.,family = binomial(),data=Xtrain)

Xtest=data.frame(testdata)

Ypred=ifelse(predict(modellogisitic,Xtest,type="response")>=0.5,1,0)
write.table(Ypred,"binary_5088.txt",row.names=FALSE,col.names=FALSE)
# length(Ypred)
# nrow(Xtest)
# ### cross validation
# 
# 
# 
# V=5
# errorlogistic=rep(0,times=5)
# for(i in 1:V){
#   samplesubtrain=sample(1:nrow(traindata),size=0.7*nrow(traindata))
#   trainsub=traindata[samplesubtrain,]
#   testsub=traindata[-samplesubtrain,]
#   Xtrainsub=data.frame(Xtrain[samplesubtrain,])
#   Xtestsub=data.frame(Xtrain[-samplesubtrain,])
#   Ytrainsub=trainsub$response
#   Ytestsub=testsub$response
#   modellogisitic=glm(Ytrainsub~.,family = binomial(),data=Xtrainsub)
#   responsehatlogistic=ifelse(predict(modellogisitic,Xtestsub,type="response")>0.5,1,0)
#   errorlogistic[i]=mean(Ytestsub!=responsehatlogistic)
# }