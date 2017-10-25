
library(caret)
library(corrplot)
library(DMwR)
library(MASS)
library(dummies)
getwd()
setwd("C:/Users/hi/Desktop/INSOFEE/INTERNSHIP")
Abalone<-read.table("Abalone.txt",header = TRUE,sep = ",")
View(Abalone)
x=c(13,13.5)
library(rpart)
mse(x,mean(x))
str(Abalone)
sum(is.na(Abalone))
dim(Abalone)

head(rows)  
Abalone_wt<-subset(Abalone,select = -c(Sex,V8))
corrplot(cor(Abalone_wt),method ="number")
Abalone_wf<-subset(Abalone,select = -c(Sex))
Abalone_f<-dummy(Abalone$Sex)
str(Abalone_d)
Abalone_d<-cbind(Abalone_f,Abalone_wf)
Abalone_d<-subset(Abalone_d,select = -c(SexI))



rows=1:nrow(Abalone_d)
trainrow<-sample(rows,size=0.7*nrow(Abalone_d))
train_ab_d<-Abalone_d[trainrow,]
test_ab_d<-Abalone_d[-trainrow,]

dim(train_ab)
dim(test_ab)

model_glm_d<-lm(V8~.,data=train_ab_d)
pre_glm_d<-predict(model_glm_d,test_ab_d)
error_glm_d<-regr.eval(pre_glm_d,test_ab_d$V8)
summary(model_glm_d)

model_step_d<-stepAIC(model_glm_d)

library(car)
vif(model_glm_d)
str(train_ab_d)

model_glm_d1<-lm(V8~SexF+SexM+V3,data=train_ab_d)
pre_glm_d1<-predict(model_glm_d1,test_ab_d)
error_glm_d1<-regr.eval(pre_glm_d1,test_ab_d$V8)
summary(model_glm_d1)
error_glm_d

model_glm_d2<-lm(V8~V1,data=train_ab_d)
pre_glm_d2<-predict(model_glm_d2,test_ab_d)
error_glm_d2<-regr.eval(pre_glm_d2,test_ab_d$V8)
summary(model_glm_d2)


model_glm_d3<-lm(V8~SexF+SexM,data=train_ab_d)
pre_glm_d3<-predict(model_glm_d3,test_ab_d)
error_glm_d3<-regr.eval(pre_glm_d3,test_ab_d$V8)
summary(model_glm_d3)

model_glm_d4<-lm(V8~SexF+SexM+V3+V6+V5,data=train_ab_d)
pre_glm_d4<-predict(model_glm_d4,test_ab_d)
error_glm_d4<-regr.eval(pre_glm_d4,test_ab_d$V8)
summary(model_glm_d4)

library(FNN)
library(Metrics)
train_w_c_ab<-subset(train_ab_d,select=-c(V8))
test_w_c_ab<-subset(test_ab_d,select=-c(V8))

result<-0
for(i in seq(1,30,1))
{

model_knn<-knn.reg(train = train_w_c_ab,test = test_w_c_ab,train_ab_d$V8,k=i)
pre_knn_ab<-as.data.frame(model_knn$pred)
result[i]<-rmse(pre_knn_ab,test_ab_d$V8)

}

plot(x=seq(1,30,1),y=result)

model_knn1<-knn.reg(train = train_w_c_ab,test = test_w_c_ab,train_ab_d$V8,k=9)
model_knn1$n
summary(model_knn1)

pre_knn_ab1<-as.data.frame(model_knn1$pred)
result_knn_rm<-rmse(pre_knn_ab1,test_ab_d$V8)
result_knn_rm1<-mse(pre_knn_ab1,test_ab_d$V8)

error_glm_d


library(rpart)

rows1=1:nrow(Abalone)
trainrow1<-sample(rows1,size=0.7*nrow(Abalone))
train_ab<-Abalone[trainrow1,]
test_ab<-Abalone[-trainrow1,]

model_rpart<-rpart(V8~.,data=train_ab,method = "anova")
summary(model_rpart)
pre_rpart<-predict(model_rpart,test_ab)
error_rpart<-rmse(pre_rpart,test_ab$V8)

pre_rpart_tra<-predict(model_rpart,train_ab)
error_rpart_tra<-rmse(pre_rpart_tra,train_ab$V8)


model_rpart1<-rpart(V8~.,data=train_ab,method = "anova",control = rpart.control(cp=0.02))
pre_rpart1<-predict(model_rpart1,test_ab)
error_rpart1<-rmse(pre_rpart1,test_ab$V8)


model_rpart2<-rpart(V8~.,data=train_ab,method = "anova",control = rpart.control(cp=0.001))
pre_rpart2<-predict(model_rpart2,test_ab)
error_rpart2<-rmse(pre_rpart2,test_ab$V8)




