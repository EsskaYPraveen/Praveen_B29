# Praveen_B29
Internship-R-Code Files

#removing all data and values from the specified environment
rm(list=ls())


#Loading Libraries
install.packages("randomForest")
library(corrplot)
library(caret)
library(ROCR)
library(e1071)
library(rpart)
library(C50)
library(class)
library(randomForest)
library(xgboost)
library(mlr)
#Reading the Data Set
skin<-read.table("Day1.txt",header = TRUE)

setwd("C:/Users/hi/Desktop/INSOFEE/INTERNSHIP")

attach(skin)
View(skin)
dim(skin)
table(skin$Target)
str(skin)
summary(skin)
#No Need to USe PCA
#No Feature Selection
#Classification Problem
#SVM,Logistic,Decision Trees
lapply(skin[,c(1:3)],sd)

#Finding Relations
skin_wc<-subset(skin,select=-c(Target))
skin_target<-skin[,"Target"]
summary(skin_target)
corrplot(cor(skin_wc))
boxplot(Red~Target,data=skin,xlab="Skin Samples",ylab="Red Colour",title="Classification OF Skin Samples")
axis(side=1,at=c(1,2),labels=c("Skin","Non-Skin"))

boxplot(Blue~Target,data=skin,xlab="Skin Samples",ylab="Blue Colour",title="Classification OF Skin Samples")
axis(side=1,at=c(1,2),labels=c("Skin","Non-Skin"))

boxplot(Green~Target,data=skin,xlab="Skin Samples",ylab="Green Colour",title="Classification OF Skin Samples")
axis(side=1,at=c(1,2),labels=c("Skin","Non-Skin"))

barplot(table(Target),xlab="Skin Colour",ylab = "Frequency" ,main="Relative Frequency")

#Normalization

scale(skin_wc,center = mins,scale = maxs-mins)
Normalization<-function(x)
{
  return(x/255)
  
}
skin_rand<-as.data.frame(apply(skin_wc,2,Normalization))

skin_r<-cbind(skin_rand,skin_target)
summary(skin_r)
skin_r$skin_target<-as.character(skin_r$skin_target)
colnames(skin_r)[4]<-"Target"
skin_r[which(skin_r$Target=="1"),"Target"]<-"0"
skin_r[which(skin_r$Target=="2"),"Target"]<-"1"
skin_r$Target<-as.factor(skin_r$Target)

skin_r$Target<-as.character(skin_r$Target)
skin_r[which(skin_r$Target=="1"),"Target"]<-"0"
skin_r[which(skin_r$Target=="2"),"Target"]<-"1"
skin_r$Target<-as.factor(skin_r$Target)


#Splitting Train And Test
set.seed(027)
train_rows<-createDataPartition(skin_r$Target,p=0.7,list=FALSE)
train1<-skin_r[train_rows,]
test1<-skin_r[-train_rows,]
str(skin_r$Target)

head(train1$Target)
#Models_Logistic Regression
model_log<-glm(Target~.,data = train1,family = "binomial")
str(pred1)
pre_train<-predict(model_log,type = "response")
pred_ttrain1<-ifelse(pre_train>=0.7,"1","0")
log_confff<-confusionMatrix(pred_ttrain1,train1$Target)
str(pred1)
pred1<-prediction(pre_train,train1$Target)
levels(pred1@fp)

perf<-ROCR::performance(pred1,measure = "tpr",x.measure = "fpr")

ROCR::plot(perf,print.cutoffs.at=seq(0.1,0.1))
perfauc<-ROCR::performance(pred1,measure="auc")
auc_score<-perfauc@y.values[[1]]

pred_test<-predict(model_log,test1,type="response")
pred_test1<-ifelse(pred_test>=0.7,"1","0")
log_conf<-confusionMatrix(pred_test1,test1$Target)

#Logistic Regression with Cross Validation

trc_log<-trainControl(method = "cv",number = 10)
model_log_cv<-train(Target~.,data = train1,method="glm",trControl=trc_log,family="binomial")
table(pred_log_cv)
pre_train_log_cv<-predict(model_log_cv,train1)
log_cv_train_conf<-confusionMatrix(pre_train_log_cv,train1$Target)



pre_log_cv<-predict(model_log_cv,test1)
log_cv_conf<-confusionMatrix(pre_log_cv,test1$Target)


#Logistic Only with Red Attribute
model_log1<-glm(Target~Red,data = train1,family = "binomial")
summary(model_log1)
pre_train<-predict(model_log1,type = "response")
pred1<-prediction(pre_train,train1$Target)
perf<-performance(pred1,measure = "tpr",x.measure="fpr")
plot(perf,print.cutoffs.at=seq(0.1,0.1))
perfauc<-performance(pred1,measure = "auc")
auc_score<-perfauc@y.values[[1]]

pred_test11<-predict(model_log1,test1,type="response")
pred_test11<-ifelse(pred_test11>=0.7,"1","0")
log_conf111<-confusionMatrix(pred_test11,test1$Target)






#Models_Decision Trees
model_rpart<-rpart(Target~.,data = train1)
pre_rpart<-predict(model_rpart,test1,type="class")

rpart_conf<-confusionMatrix(pre_rpart,test1$Target)
model_rpart$variable.importance


model_rpart1<-rpart(Target~.,data = train1,control=rpart.control(cp=0.05,xval=10))
pred_rpart1<-predict(model_rpart1,test1,type = "class")

model_rpart1_conf<-confusionMatrix(pred_rpart1,test1$Target)
summary(model_rpart1)
#Rpart Tuning parameters with cv

modelLookup("rpart")
train_rpart<-trainControl(method = "cv",number = 5 )
tune<-expand.grid(cp=seq(0,0.1,0.01))
model_rpart_cv<-train(Target~.,data = train1,method="rpart",family="binomial",tuneGrid=tune)

tune.rpart()

#Model_C5.0

model_c50<-C5.0(Target~.,data=train1,rules=T)
summary(model_c50)
pre_c50<-predict(model_c50,test1,type="class")
c50_conf<-confusionMatrix(pre_c50,test1$Target)
C5imp(model_c50,metric = "usage")

#SVM
model_svm<-svm(Target~.,data=train1)
pre_svm<-predict(model_svm,test1,type="class")
svm_conf<-confusionMatrix(pre_svm,test1$Target)

#Random Forest
model_rf<-randomForest(Target~.,data = train1)
pre_rf<-predict(model_rf,test1)
rf_conf<-confusionMatrix(pre_rf,test1$Target)




