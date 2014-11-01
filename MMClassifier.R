# install.packages("e1071")
# install.packages("kernlab")
# install.packages("pROC")

library(caret)
library(kernlab)
library(e1071)
library(doParallel)

setwd("/export/home/mraza/IFC_Analysis/IFC_Analysis")

sdate<-'Oct31-2014'
source('./MMClassifierHelper.R')

 cl <- makeCluster(4)
 registerDoParallel(cl)


set.seed(999)

i=0
subdir=''
for (each in c('DataNormalMM-Call1Threshold.csv','DataNormalMMTop-Call1Threshold.csv')){

subdir=ifelse(i==0,'MMResults','MMTopResults')
dir.create(subdir,showWarnings=FALSE)	

data<-read.csv('DataNormalMM-Call1Threshold.csv')
dim(data)

data<-data[sample(nrow(data),100000),]
dim(data)
write.csv(data,paste("sample",each,sep='-'),quote=FALSE)

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

res_training<-predict.glm(glm1,newdata=training, type="response")
result<-cbind.data.frame(res_training,training_labels)

head(result)

colnames(result)<-c('yhat','y')

result$yhatclass<-ifelse(result$yhat>0.5,1,0)

write.csv(result,paste(subdir,"LogisticTrainingResults.csv",sep="/"),quote=FALSE)


print ('RMSE Training')
print (rmse(as.numeric(result$yhat)-as.numeric(result$y)))



######RMSE Testing
res<-predict.glm(glm1,newdata=testing, type="response")
result<-cbind.data.frame(res,testing_labels)

head(result)

colnames(result)<-c('yhat','y')

result$yhatclass<-ifelse(result$yhat>0.5,1,0)


write.csv(result,paste(subdir,"LogisticTestingResults.csv",sep="/"),quote=FALSE)
print ('RMSE Testing')
print (rmse(as.numeric(result$yhat)-as.numeric(result$y)))
######### End RMSE Testing


write.csv(file='./regressionGLM_NormalMM.csv',
          x=regressionGLM(training_labels, training))

#result<-cbind.data.frame(res,as.numeric(testing$UserType)-1)

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

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 2)




fcontrol <- rfeControl(functions=rfFuncs, method="cv", number=10)

subsets <- c(1:5, 10, 15, 20, 25)
rfProfile <- rfe(training, training_labels, sizes = subsets, rfeControl = fcontrol)

write.csv(rfProfile,paste(subdir,"Features_RFE",sep="/"),quote=FALSE)
colnames(training)

set.seed(808)
# Gradient Boosting Machine
gbmFit1 <- train(training_labels ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
		 rfeControl=fcontrol,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = TRUE)
gbmFit1

print("Printing the list of predictors used by GBM")
features_gbm<-predictors(gbmFit1)

write.csv(features_gbm,paste(subdir,"Features_GBM",sep="/"),quote=FALSE)

results_tr_gbm1<-predict(gbmFit1,newdata=training)
write.csv(rfProfile,paste(subdir,"Results_tr_gbm1",sep="/"),quote=FALSE)

results_ts_gbm1<-predict(gbmFit1,newdata=testing)
write.csv(rfProfile,paste(subdir,"Results_ts_gbm1",sep="/"),quote=FALSE)

#GLMNET- Gradient Linear Model with Regularization
glmFit1 <- train(training_labels ~ ., data = training,
                 method = "glmnet",
                 trControl = fitControl,
                 rfeControl=fcontrol,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = TRUE)
glmFit1

results_ts_glmnet<-predict(glmFit1,newdata=testing)
write.csv(results_ts_glmnet,paste(subdir,"Results_ts_glmnet",sep="/"),quote=FALSE)
results_tr_glmnet<-predict(glmFit1,newdata=training)
write.csv(results_tr_glmnet,paste(subdir,"Results_tr_glmnet",sep="/"),quote=FALSE)

print("Printing the list of predictors used by GlMNet")
features_glmnet=predictors(glmFit1)

write.csv(features_glmnet,paste(subdir,"Features_GLMNET",sep="/"),quote=FALSE)
#SVM Linear

svmLinear1 <- train(training_labels ~ ., data = training,
                 method = "svmLinear",
                 trControl = fitControl,
                 rfeControl=fcontrol,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = TRUE)
svmLinear1

results_ts_svmL<-predict(svmLinear1,newdata=testing)
write.csv(results_ts_svmL,paste(subdir,"Results_ts_svmL",sep="/"),quote=FALSE)

results_tr_svmL<-predict(svmLinear1,newdata=training)
write.csv(results_tr_svmL,paste(subdir,"Results_tr_svmL",sep="/"),quote=FALSE)

print("Printing the list of predictors used by SVMLinear")
features_svmL<-predictors(svmLinear1)

write.csv(features_svmL,paste(subdir,"Features_SVML",sep="/"),quote=FALSE)


# SVM Poly

svmPoly1 <- train(training_labels ~ ., data = training,
                 method = "svmPoly",
                 trControl = fitControl,
                 rfeControl=fcontrol,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = TRUE)
svmPoly1

results_ts_svmP<-predict(svmPoly1,newdata=testing)
write.csv(results_ts_svmP,paste(subdir,"Results_ts_svmp",sep="/"),quote=FALSE)
results_tr_svmP<-predict(svmPoly1,newdata=training)
write.csv(results_tr_svmP,paste(subdir,"Results_tr_svmp",sep="/"),quote=FALSE)

print("Printing the list of predictors used by SVMPoly")
features_svmP<-predictors(svmPoly1)

write.csv(features_svmP,paste(subdir,"Features_svmP",sep="/"),quote=FALSE)

#SVM RBF
svmRBF1 <- train(training_labels ~ ., data = training,
                 method = "svmRBF",
                 trControl = fitControl,
                 rfeControl=fcontrol,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = TRUE)
svmRBF1

results_ts_svmr<-predict(svmRBF1,newdata=testing)

write.csv(results_ts_svmr,paste(subdir,"Results_ts_Svmr",sep="/"),quote=FALSE)

results_tr_svmr<-predict(svmRBF1,newdata=training)

write.csv(results_tr_svmr,paste(subdir,"Results_tr_svmr",sep="/"),quote=FALSE)

print("Printing the list of predictors used by SVMRBF")
features_svmr<-predictors(svmRBF1)
write.csv(features_svmr,paste(subdir,"Features_SVMR",sep="/"),quote=FALSE)
}
stopCluster(cl)
