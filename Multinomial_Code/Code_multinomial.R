traindata<-read.table("training_data.txt",header = TRUE)
testdata<-read.table("test_data.txt",header = TRUE)
testdata=testdata[,c(2:ncol(testdata))]
library(tidyverse)
library(e1071)
library(SuperLearner)


set.seed(100)
traindata$response=traindata$activity
traindata$binresponse=ifelse((traindata$activity %in% c(1,2,3)),1,0)
traindata$response[which(traindata$activity %in% c(7,8,9,10,11,12))]=7
traindata1=traindata[which(traindata$binresponse==1),]
traindata0=traindata[which(traindata$binresponse==0),]
Xtrain=traindata[,c(3:(ncol(traindata)-2))]
Xtrain1=traindata1[,c(3:(ncol(traindata1)-2))]
Xtrain0=traindata0[,c(3:(ncol(traindata0)-2))]



traindata=traindata[,c(2:ncol(traindata))]
Ytrain=traindata$response
Ytrain1=traindata1$response
Ytrain0=traindata0$response
Xtest=data.frame(testdata)
bintest=read.table("binary_5088.txt")
Xtest1=Xtest[which(bintest==1),]
Xtest0=Xtest[which(bintest==0),]

dim(Xtest1)
dim(Xtrain1)

library(glmnet)



model1=cv.glmnet(x=as.matrix(Xtrain1),y=as.factor(Ytrain1),
                family="multinomial",alpha=0.52,nfolds=20)

Ypredtrain1=as.numeric(predict(model1,newx=as.matrix(Xtrain1),type="class"))
sum(Ypredtrain1!=Ytrain1)
Ypredtest1=as.numeric(predict(model1,newx=as.matrix(Xtest1),type="class"))


model0=cv.glmnet(x=as.matrix(Xtrain0),y=as.factor(Ytrain0),
                 family="multinomial",alpha=0.22,nfolds=20)

Ypredtrain0=as.numeric(predict(model0,newx=as.matrix(Xtrain0),type="class"))
Ypredtest0=as.numeric(predict(model0,newx=as.matrix(Xtest0),type="class"))

Ypredtest=rep(0,nrow(Xtest))
Ypredtest[which(bintest==1)]=Ypredtest1
Ypredtest[which(bintest==0)]=Ypredtest0



write.table(Ypredtest,"multiclass_5088.txt",row.names=FALSE,col.names = FALSE)



