#Cover Type Data Set

#Libraries
install.packages("doParallel")
library(MASS)
library(C50)
library(car)
library(caret)
library(rpart)
library(e1071)
library(cluster)
#Reading the DataSet
getwd()
setwd("C:/Users/hi/Desktop/INSOFEE/INTERNSHIP")
cover<-read.table("covtype.data",sep=",")
prop.table(table(cover$V55),1)
str(cover)

#Decriptive Stats
cover[,c(11:55)]<-lapply(cover[,c(11:55)],factor)
barplot(table(cover$V55),main="Class Imbalance On Target Variable",
        col = c("lightblue", "mistyrose", "cyan",
                "lavender", "cornsilk","lightgreen"),xlab = "Target",ylab = "Frequency")
boxplot(cover$V1~cover$V55,horiz=TRUE,xlab="Class Levels",ylab="Elevation In Meters",
        col=rainbow(20),main="Relation between Elevation and Forest Cover types")


boxplot(cover$V2~cover$V55,horiz=TRUE,xlab="Class Levels",ylab="Aspect/Azimuth In Meters",
        col=rainbow(20),main="Relation between Aspect and Forest Cover types")



boxplot(cover$V3~cover$V55,horiz=TRUE,xlab="Class Levels",ylab="Slope In Meters",
        col=rainbow(20),main="Relation between Slope and Forest Cover types")



boxplot(cover$V4~cover$V55,horiz=TRUE,xlab="Class Levels",ylab="Horizontal Distance In Meters",
        col=rainbow(20),main="Relation between Horizontal Distance and Forest Cover types")

model_cover_naive_conf
boxplot(cover$V5~cover$V55,horiz=TRUE,xlab="Class Levels",ylab="Vertical Distance In Meters",
        col=rainbow(20),main="Relation between Vertical Distance and Forest Cover types")

library(ggplot2)
table(cover$V50,cover$V55)

ggplot(cover,aes(x=V18, fill=V55)) + geom_bar() +
  xlab("Class Levels") + ylab("Soil Type")  +
  ggtitle("Relation between Soil Distance and Forest Cover types")

ggplot(cover,aes(x=V15, fill=V55)) + geom_bar() +
  xlab("Class Levels") + ylab("Soil Type")  +
  ggtitle("Relation between Vertical Distance and Forest Cover types")


ggplot(cover,aes(x=V50, fill=V55)) + geom_bar() +
  xlab("Class Levels") + ylab("Soil Type")  +
  ggtitle("Relation between Vertical Distance and Forest Cover types")

ggplot(cover,aes(x=V40, fill=V55)) + geom_bar() +
  xlab("Class Levels") + ylab("Soil Type")  +
  ggtitle("Relation between Vertical Distance and Forest Cover types")



#SUBSET DATA
trainnrows9<-createDataPartition(cover$V55,p=0.07,list=FALSE)
cover<-cover[trainnrows9,]
dim(cover)

cover_wt<-subset(cover,select=-c(V55))
str(cover_wt)

#TO FInd Distance Matrix but received error
dist<-daisy(cover,metric="gower")

#Error: cannot #allocate vector of size 3.1 Gb

install.packages("FactoMineR")
library(FactoMineR)
res<-MCA(cover_wt,quanti.sup = c(1:10),quali.sup = c(11:54))


#


cover[,c(11:55)]<-lapply(cover[,c(11:55)],factor)

cover_num<-cover[,-c(11:55)]
cover_cat<-cover[,c(11:54)]
cover_target<-cover[,c(55)]
#Standardization
cover_numst<-scale(cover_num,center = TRUE,scale = TRUE)

cover_1<-cbind(cover_numst,cover_cat,cover_target)
str(cover_1)

set.seed(0229)
trainnrows1<-createDataPartition(cover_1$cover_target,p=0.7,list=FALSE)

train_cover1<-cover_1[trainnrows1,]
test_cover1<-cover_1[-trainnrows1,]
#Surrogate



#Model Building
library(rpart.plot)
model_cover_rpart<-rpart(cover_target~.,data=train_cover1,method = "class")
dim(train_cover1)
printcp(model_cover_rpart)
plotcp(model_cover_rpart)
rpart.plot(model_cover_rpart)
rpart.plot(model_cover_rpart,type = 3,extra=101,fallen.leaves = TRUE)
model_cover_rpart$variable.importance
summary(model_cover_rpart)
pre_cover_rpart_train<-predict(model_cover_rpart,type="class")
model_cover_raprt_conf_train<-confusionMatrix(pre_cover_rpart_train,train_cover1$cover_target)
dim(train_cover1)

pre_cover_rpart<-predict(model_cover_rpart,test_cover1,type="class")
model_cover_raprt_conf<-confusionMatrix(pre_cover_rpart,test_cover1$cover_target)

#To Generate Rules
install.packages("rattle")
require(rattle)
asRules(model_cover_rpart)
plot.rpart(model_cover_rpart)
text(model_cover_rpart,cex=0.7)
#Tuning Decision Trees

model_cover_rpart1<-rpart(cover_target~.,data=train_cover1,method = "class",
                          cp=0.02,control=rpart.control(minsplits=10))
pre_cover_rpart1<-predict(model_cover_rpart1,test_cover1,type="class")
model_cover_raprt_conf1<-confusionMatrix(pre_cover_rpart1,test_cover1$cover_target)
asRules(model_cover_rpart1)
aa<-importance(model_rf_pca_cover)

#RANDOM FOREST
library(randomForest)
model_rf_pca_cover<-randomForest(cover_target~.,data=train_cover1,importance=TRUE)
pre_cover_rf<-predict(model_rf_pca_cover,test_cover1,type="class")
model_cover_rf_conf<-confusionMatrix(pre_cover_rf,test_cover1$cover_target)
importance(model_rf_pca_cover)
pre_cover_rf_train<-predict(model_rf_pca_cover,type="class")
model_cover_rf_conf_train<-confusionMatrix(pre_cover_rf_train,train_cover1$cover_target)


model_rf_pca_cover1<-randomForest(cover_target~.,data=train_cover1,ntrees=600,mtry=7)
pre_cover_rf1<-predict(model_rf_pca_cover1,test_cover1,type="class")
model_cover_rf_conf1<-confusionMatrix(pre_cover_rf1,test_cover1$cover_target)


model_rf_pca_cover2<-randomForest(cover_target~.,data=train_cover1,ntrees=700,mtry=9)
pre_cover_rf2<-predict(model_rf_pca_cover2,test_cover1,type="class")
model_cover_rf_conf2<-confusionMatrix(pre_cover_rf2,test_cover1$cover_target)


model_rf_pca_cover3<-randomForest(cover_target~.,data=train_cover1,ntrees=1000,mtry=12)
pre_cover_rf3<-predict(model_rf_pca_cover3,test_cover1,type="class")
model_cover_rf_conf3<-confusionMatrix(pre_cover_rf3,test_cover1$cover_target)
model_rf_pca_cover$classes



model_cover_c50<-C5.0(cover_target~.,data=train_cover1)
pre_cover_c50<-predict(model_cover_c50,test_cover1)
pre_cover_c50_train<-predict(model_cover_c50,train_cover1)
model_cover_c50_conf_train<-confusionMatrix(pre_cover_c50_train,train_cover1$cover_target)
model_cover_c50_conf<-confusionMatrix(pre_cover_c50,test_cover1$cover_target)


#SVM

model_cover_svm<-svm(cover_target~.,data = train_cover1)
pre_cover_svm<-predict(model_cover_svm,test_cover1)
model_cover_svm_conf<-confusionMatrix(pre_cover_svm,test_cover1$cover_target)


#LOGISTIC
library(glmnet)
x=model.matrix(train_cover1$cover_target~.,data=train_cover1)
model_cover_glmnet<-glmnet(x,train_cover1$cover_target,family = "multinomial",
                           nlambda = 50,alpha = 1,type.multinomial = "grouped")


summary(model_cover_glmnet)
y=model.matrix(test_cover1$cover_target~.,data=test_cover1)
pre_cover_glm<-predict(model_cover_glmnet,y,type="class")
model_cover_glm_conf<-confusionMatrix(pre_cover_glm,test_cover1$cover_target)
head(test_cover1$cover_target)

str(train_cover1$cover_target)


#KNN

train1_Cover_wt<-subset(train_cover1,select=-c(cover_target))
test1_cover_wt<-subset(test_cover1,select=-c(cover_target))
model_knn_cover<-knn3(train1_Cover_wt,test1_cover_wt,train_cover1$cover_target,prob = TRUE ,k=1)
model_conf_knn<-confusionMatrix(model_knn_cover,test_cover1$cover_target)
dim(cover_target)

#ONly on 4 & 5
cover$V55<-as.character(cover$V55)
target_4_5<-cover[which(cover$V55 == '4' | cover$V55 == '5'),]
nrow(target_4_5)
table(target_4_5$V55)
summary(target_4_5_target)
target_4_5<-as.vector(target_4_5)
class(target_4_5_num_st_1)
cover_4_5<-cover[target_4_5,]
target_4_5_num<-target_4_5[,-c(11:55)]
target_4_5_cat<-target_4_5[,c(11:55)]
target_4_5_target<-target_4_5[,c(55)]
target_4_5_numst<-scale(target_4_5_num,center = TRUE,scale = TRUE)
target_4_5_numst<-as.data.frame(target_4_5_numst)
target_4_5_num_st_1<-cbind(target_4_5_numst,target_4_5_target)

#Model Only on NUmericals & 4 & 5
trainnrows11_num<-createDataPartition(target_4_5_num_st_1$target_4_5_target,p=0.7,list=FALSE)

train_cover11<-target_4_5_num_st_1[trainnrows11_num,]
test_cover11<-target_4_5_num_st_1[-trainnrows11_num,]
train_cover11$target_4_5_target
model_cover_rpart_num45<-rpart(target_4_5_target~.,data=train_cover11,method = "class",cp=0.02)
pre_cover_rpart45<-predict(model_cover_rpart_num45,test_cover11,type="class")
model_cover_raprt_conf45<-confusionMatrix(pre_cover_rpart45,test_cover11$target_4_5_target)
model_cover_rpart_num45$variable.importance
summary(model_cover_rpart_num45)
model_rf_cover_num<-randomForest(target_4_5_target~.,data=train_cover11)
sum(is.na(train_cover1))
dim(train_cover1)
model_rf_fin
importance(model_rf_cover)
pred_rf_cover_num<-predict(model_rf_cover_num,test_cover11)
model_conf__cover_num_rf<-confusionMatrix(pred_rf_cover_num,test_cover11$Target)





#MOdel on NUmericals
cover_numst<-as.data.frame(cover_numst)
cover_numc<-cbind(cover_numst,cover_target)
class(cover_numc)
trainnrows11_nu<-createDataPartition(cover_numc$cover_target,p=0.7,list=FALSE)
train_cover11_nu<-cover_numc[trainnrows11_nu,]
test_cover11_nu<-cover_numc[-trainnrows11_nu,]
train_cover11_nu$cover_target
model_cover_rpart_num<-rpart(cover_target~.,data=train_cover11_nu,method = "class",cp=0.02)
pre_cover_rpart_num<-predict(model_cover_rpart_num,test_cover11_nu,type="class")
model_cover_raprt_conf_num<-confusionMatrix(pre_cover_rpart_num,test_cover11_nu$cover_target)

model_rf_cover_num<-randomForest(cover_target~.,data=train_cover11_nu)
sum(is.na(train_cover1))
dim(train_cover1)
model_rf_fin
importance(model_rf_cover)
pred_rf_cover_num<-predict(model_rf_cover_num,test_cover11_nu)
model_conf__cover_num_rf<-confusionMatrix(pred_rf_cover_num,test_cover11_nu$cover_target)





#


dim(train_cover11)
head(target_4_5_11)
target_4_5_1<-cbind(target_4_5_numst,target_4_5_cat,target_4_5_target)
target_4_5_11<-subset(target_4_5_1,select=-c(V55))
trainnrows11<-createDataPartition(target_4_5_11$target_4_5_target,p=0.7,list=FALSE)

train_cover11<-target_4_5_11[trainnrows11,]
test_cover11<-target_4_5_11[-trainnrows11,]
table(test_cover11$target_4_5_target)






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
library(corrplot)
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
trainnrows<-createDataPartition(cover_1$cover_target,p=0.1,list=FALSE)
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


set.seed(0229)
trainnrows1<-createDataPartition(cover_1$cover_target,p=0.7,list=FALSE)

train_cover1<-cover_1[trainnrows1,]
test_cover1<-cover_1[-trainnrows1,]

str(cover)
#PCA
pr_cover<-prcomp(train_cover111[setdiff(names(train_cover111),"V55")])
train_cover111_wt<-subset(train_cover111,select=-c(V55))
summary(pr_cover)
train_pca_final<-as.data.frame(predict(pr_cover,train_cover111_wt))
train_pca_final<-train_pca_final[,1:4]
train_pca_f<-data.frame(train_pca_final,train_cover111$V55)
head(train_pca_f)

test_cover111_wt<-subset(test_cover111,select=-c(V55))
test_pca_final<-as.data.frame(predict(pr_cover,test_cover111_wt))
test_pca_final<-test_pca_final[,1:4]
test_pca_f<-data.frame(test_pca_final,test_cover111$V55)
table(test_pca_f$test_cover111.V55)

#PCA MODEL BUILDING
library(randomForest)
model_rf_pca_cover<-randomForest(train_pca_f$train_cover111.V55~.,data=train_pca_f)





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
summary(model_cover_rpart)

#Rpart with tuned with cross validation

model_cover_rpart2<-rpart(cover_target~.,data=train_cover1,method = "class",control = rpart.control(cp=c(0.02),xval=5))
pre_cover_rpart2<-predict(model_cover_rpart2,test_cover1,type="class")
model_cover_raprt_conf2<-confusionMatrix(pre_cover_rpart2,test_cover1$cover_target)




#MOdel c50
model_cover_c50<-C5.0(cover_target~.,data=train_cover1)
pre_cover_c50<-predict(model_cover_c50,test_cover1)
model_cover_c50_conf<-confusionMatrix(pre_cover_c50,test_cover1$cover_target)

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



