# install.packages("e1071")
# install.packages("kernlab")
# install.packages("pROC")

library(caret)
library(kernlab)
library(e1071)

setwd("/export/home/mraza/IFC_Analysis/IFC_Analysis")

sdate<-'Oct29th-2014'
source('./MMClassifierHelper.R')



set.seed(999)

data<-read.csv('DataNormalMM.csv')
dim(data)

data<-data[sample(nrow(data),100000),]
dim(data)
write.csv(data,"DataNormalMMSample.csv",quote=FALSE)

#data<-read.csv("DataNormalMMSample.csv")
data$UserType<-as.character(data$UserType)

data$UserType[data$UserType=='normal']<-'0'
data$UserType[data$UserType=='mm']<-'1'

data$UserType<-as.factor(data$UserType)

inTraining<-createDataPartition(data$UserType,p=0.75,list=FALSE)
training<-data[inTraining,]
testing<-data[-inTraining,]

dim(training)
dim(testing)

training_labels<-training$UserType
testing_labels<-testing$UserType
testing$UserType<-NULL
training<-training[,!names(training) %in% c('X','X.2','X.1','CallerId','UserType')]

testing_callerid=testing$CallerId

testing<-testing[,!names(testing) %in% c('X','X.2','X.1','CallerId','UserType')]

#data$UserType[data$UserType=='normal']<-'0'
#data$UserType[data$UserType=='mm']<-'1'

glm1<-glm(training_labels ~ ., data = training,family='binomial')

print ('RMSE Training')
print (rmse(glm1$residuals))

######RMSE Testing
res<-predict.glm(glm1,newdata=testing, type="response")
result<-cbind.data.frame(res,testing_labels)

head(result)

colnames(result)<-c('yhat','y')

result$yhatclass<-ifelse(result$yhat>0.5,1,0)


print ('RMSE Testing')
print (rmse(as.numeric(result$yhat)-as.numeric(result$y)))
######### End RMSE Testing


write.csv(file='./regressionGLM_NormalMM.csv',
          x=regressionGLM(training_labels, training))

#result<-cbind.data.frame(res,as.numeric(testing$UserType)-1)

perf = function(cut, mod, y)
{
  yhat = (mod$fit>cut)
  w = which(y==1)
  sensitivity = mean( yhat[w] == 1 ) 
  specificity = mean( yhat[-w] == 0 ) 
  c.rate = mean( y==yhat ) 
  d = cbind(sensitivity,specificity)-c(1,1)
  d = sqrt( d[1]^2 + d[2]^2 ) 
  out = t(as.matrix(c(sensitivity, specificity, c.rate,d)))
  colnames(out) = c("sensitivity", "specificity", "c.rate", "distance")
  return(out)
}

#s = seq(.01,.99,length=1000)
#OUT = matrix(0,1000,4)
#for(i in 1:1000) OUT[i,]=perf(s[i],glm1,testing$UserType)
#plot(s,OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
#axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
#axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
#lines(s,OUT[,2],col="darkgreen",lwd=2)
#lines(s,OUT[,3],col=4,lwd=2)
#lines(s,OUT[,4],col="darkred",lwd=2)
#box()
#legend(0,.25,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Classification Rate","Distance"))

#fitControl <- trainControl(## 2-fold CV
#  method = "repeatedcv",
#  number = 2,
  ## repeated ten times
#  repeats = 2)

#colnames(training)

#set.seed(808)
#gbmFit1 <- train(UserType ~ ., data = training,
#                 method = "gbm",
#                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
#                 verbose = TRUE)
#gbmFit1

#results<-predict(gbmFit1,newdata=testing)
