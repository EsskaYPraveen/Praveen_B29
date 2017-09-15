#Cover Type Data Set

#Libraries
install.packages("doParallel")
library(MASS)
library(C50)
library(car)
library(caret)
library(DMwR)
library(rpart)
library(e1071)

#Reading the DataSet
getwd()
setwd("C:/Users/hi/Desktop/INSOFEE/INTERNSHIP")
cover<-read.table("covtype.data",sep=",")
View(cover)
attach(cover)
str(cover_1)
prop.table(table(cover$V55))
dim(cover)
sum(is.na(cover))
cover_wc<-subset(cover,select=-c(55))



#PreProcessing
#Converting Required Features into Factors


cover[,c(11:55)]<-lapply(cover[,c(11:55)],factor)
fin_wtt<-nearZeroVar(cover)
table(cover$V41)
str(cover)
cover_num<-cover[,-c(11:55)]
cover_cat<-cover[,c(11:55)]
cover_target<-cover[,c(55)]

#Standardization
cover_numst<-scale(cover_num,center = TRUE,scale = TRUE)

#Finding Relations
corrplot(cor(cover_num),method = "number")
ffff<-findCorrelation(cor(cover_num),cutoff=0.7)
par(mfrow=c(2,2))
boxplot(cover$V7~cover$V55)
boxplot(cover$V9~cover$V55)
boxplot(cover$V2~cover$V55)

cover_1<-cbind(cover_numst,cover_cat,cover_target)
cover_1<-cover_1[,-c(55)]
cover_11<-cover_1[,-c("cover_target")]
cover_11<-subset(cover_1,select=-c(cover_target))
dim(cover_1)
str(cover)
#Data Seperation
set.seed(0227)
trainnrows<-createDataPartition(cover_1$cover_target,p=0.3,list=FALSE)
train_cover<-cover_1[trainnrows,]
trainnrows1<-createDataPartition(cover_1$cover_target,p=0.7,list=FALSE)

train_cover1<-train_cover[trainnrows1,]
test_cover1<-train_cover[-trainnrows1,]


set.seed(0228)
trainnrows<-createDataPartition(cover_1$cover_target,p=0.5,list=FALSE)
train_cover<-cover_1[trainnrows,]
trainnrows1<-createDataPartition(cover_1$cover_target,p=0.7,list=FALSE)

train_cover1<-train_cover[trainnrows1,]
test_cover1<-train_cover[-trainnrows1,]
table(test_cover1$cover_target)



#PCA
pr_cover<-prcomp(train_cover)
names(pr_cover)
pr_cover$x[1:4,1:3]
pr_cover$rotation[1:10,1:4]


cover_pr<-data.frame(target=train_cover$cover_target,pr_cover$x)

train_cover_pr<-cover_pr[,1:20]
model_rpart_cover<-rpart(train_cover_pr$cover_target~.,data=train_cover_pr,method = "class")

  #Model Building
library(glmnet)
x=model.matrix(train_cover1$cover_target~.,data=train_cover1)

model_cover_glmnet<-glmnet(x,train_cover1$cover_target,family = "multinomial",nlambda = 50,alpha = 1,type.multinomial = "grouped")


#DECISION TREES
model_cover_rpart<-rpart(cover_target~.,data=train_cover1,method = "class",cp=0.02)
pre_cover_rpart<-predict(model_cover_rpart,test_cover1,type="class")
model_cover_raprt_conf<-confusionMatrix(pre_cover_rpart,test_cover1$cover_target)
model_cover_rpart$variable.importance
summary(model_cover_rpart)
dim(cover)
plot(model_cover_rpart)
text(model_cover_rpart)
cover_target~V1+V14+V4+V12+V36+V24+V6+V52+V20+V15+V19

model_cover_rpart$variable.importance

#TUNING
model_cover_rpart2<-train(cover_target~.,data=train_cover1,method="rpart",trControl=trainControl(cp=c(0.01,0.1,1)))
model_cover_rpart_tune<-tune.rpart(cover_target~.,data=train_cover1,cp = c(0.002,0.005,0.01,0.015,0.02,0.03))
model_cover_rpart_tune1<-tune.rpart(cover_target~.,data=train_cover1,cp = c(0.1,0.02,0.03))

#TAKEN IMPORTANT PARAMETERS FROM RPART

model_cover_rpart_imp<-rpart(cover_target~V1+V14+V4+V12+V36+V24+V6+V52+V20+V15+V19,data=train_cover1,method = "class",cp=0.02)
pre_cover_rpart_imp<-predict(model_cover_rpart_imp,test_cover1,type="class")
model_cover_raprt_conf_imp<-confusionMatrix(pre_cover_rpart_imp,test_cover1$cover_target)


#Rpart with tuned with cross validation

model_cover_rpart2<-rpart(cover_target~.,data=train_cover1,method = "class",control = rpart.control(cp=c(0.02),xval=5))
pre_cover_rpart2<-predict(model_cover_rpart2,test_cover1,type="class")
model_cover_raprt_conf2<-confusionMatrix(pre_cover_rpart2,test_cover1$cover_target)




#MOdel c50
model_cover_c50<-C5.0(cover_target~.,data=train_cover1)
pre_cover_c50<-predict(model_cover_c50,test_cover1)
model_cover_c50_conf<-confusionMatrix(pre_cover_c50,test_cover1$cover_target)
summary(model_cover_c50)
prcomp(cover_11)

#Tune for C50

names(getModelInfo())
modelLookup("C5.0")
model_cover_c501<-train(cover_target~.,data=train_cover1,method="C5.0",trControl=trainControl(method = "cv",number=5))

sum(is.na(train_cover1))
#model Naive Bayes
model_cover_naive<-naiveBayes(cover_target~.,data=train_cover1)
pre_cover_naive<-predict(model_cover_naive,test_cover1)
model_cover_naive_conf<-confusionMatrix(pre_cover_naive,test_cover1$cover_target)
head(train_cover)



#SVM
model_cover_svm<-svm(cover_target~.,data = train_cover1)
pre_cover_svm<-predict(model_cover_svm,test_cover1)
model_cover_svm_conf<-confusionMatrix(pre_cover_svm,test_cover1$cover_target)


model_cover_svm1<-svm(cover_target~.,data = train_cover)
pre_cover_svm1<-predict(model_cover_svm1,test_cover1)
model_cover_svm_conf1<-confusionMatrix(pre_cover_svm1,test_cover1$cover_target)


#RANDOM FOREST
library(randomForest)
#Error: cannot allocate vector of size 1.5 Gb
model_rf_cover<-randomForest(cover_target~.,data = train_cover1)
sum(is.na(train_cover1))
dim(train_cover1)
model_rf_fin
importance(model_rf_cover)
pred_rf_cover<-predict(model_rf_cover,test_cover1)
model_conf_fin<-confusionMatrix(pred_rf,test_fin$Target)

##Specific to target 4 & 5

target_4_5<-cover[which(cover$V55 == '4' | cover$V55 == '5'),]
nrow(target_4_5)
target_4_5<-as.vector(target_4_5)
class(target_4_5)
cover_4_5<-cover[target_4_5,]
target_4_5_num<-target_4_5[,-c(11:55)]
target_4_5_cat<-target_4_5[,c(11:55)]
target_4_5_target<-target_4_5[,c(55)]
target_4_5_numst<-scale(target_4_5_num,center = TRUE,scale = TRUE)
dim(train_cover11)
head(target_4_5_11)
target_4_5_1<-cbind(target_4_5_numst,target_4_5_cat,target_4_5_target)
target_4_5_11<-subset(target_4_5_1,select=-c(V55))
trainnrows11<-createDataPartition(target_4_5_11$target_4_5_target,p=0.7,list=FALSE)

train_cover11<-target_4_5_11[trainnrows11,]
test_cover11<-target_4_5_11[-trainnrows11,]
table(test_cover11$target_4_5_target)




library(rpart)
model_cover_rpart_4_5<-rpart(target_4_5_target~.,data=train_cover11,method = "class",cp=0.02)

pre_cover_rpart_4_5_train<-predict(model_cover_rpart_4_5,train_cover11,type="class")
model_cover_raprt_train_conf_4_5<-confusionMatrix(pre_cover_rpart_4_5_train,train_cover11$target_4_5_target)

model_cover_rpart_4_5$variable.importance

pre_cover_rpart_4_5<-predict(model_cover_rpart_4_5,test_cover11,type="class")
model_cover_raprt_conf_4_5<-confusionMatrix(pre_cover_rpart_4_5,test_cover11$target_4_5_target)
View(target_4_5)



head(train_cover1)

pr_cover<-prcomp(cover)
pr_cover$rotation
summary(pr_cover)



