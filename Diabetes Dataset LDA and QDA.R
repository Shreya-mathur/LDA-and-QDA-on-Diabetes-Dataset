library(MMST)
library(MASS)
library(klaR)
library(ggplot2)
library(GGally)
##Part-a##
load("C:/Users/Shreya/Downloads/Diabetes.RData")
head(Diabetes)
names(Diabetes)
pairs(Diabetes[1:5])
cols <- character(nrow(Diabetes))
cols[]<-"green"
cols[Diabetes$group == "Normal"] <- "blue"
cols[Diabetes$group == "Chemical_Diabetic"] <- "red"
x11()
pairs(Diabetes[1:5],col=cols)
##Try##
set.seed(1)
train <- sample(1:nrow(Diabetes), .66*nrow(Diabetes))
diab_train <- Diabetes[train,]
diab_test <- Diabetes[-train,]
Diabetes$group<-as.factor(Diabetes$group)
ggpairs(Diabetes, columns = 1:5)
x11()
ggpairs(Diabetes, columns = 1:5, ggplot2::aes(colour=group))
##Part-b##
##LDA##
y_true_train<- as.numeric(diab_train$group)-1
y_true_test<-as.numeric(diab_test$group)-1

lda_fit<-lda(group~.,data=diab_train)
lda_pred_train<-predict(lda_fit,newdata = diab_train)
lda_pred_test<-predict(lda_fit,newdata = diab_test)
y_hat_train <- as.numeric(lda_pred_train$class)-1
y_hat_test <- as.numeric(lda_pred_test$class)-1
cbind(y_true_train,y_hat_train)
cbind(y_true_test,y_hat_test)
lda_train_error <- sum(abs(y_true_train - y_hat_train))/length(y_true_train)
lda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)
lda_train_error
lda_test_error
##QDA##

qda_fit<-qda(group~.,data=diab_train)
qda_pred_train<-predict(qda_fit,newdata = diab_train)
qda_pred_test<-predict(qda_fit,newdata = diab_test)
y_hat_train_q <- as.numeric(qda_pred_train$class)-1
y_hat_test_q <- as.numeric(qda_pred_test$class)-1
cbind(y_true_train,y_hat_train_q)
cbind(y_true_test,y_hat_test_q)
qda_train_error <- sum(abs(y_true_train - y_hat_train_q))/length(y_true_train)
qda_test_error <- sum(abs(y_true_test - y_hat_test_q))/length(y_true_test)
qda_train_error
qda_test_error
##Part c##
relwt = c(1.86)
glufast =c(184)
glutest =c(68)
instest = c(122)
sspg = c(544)
group = "Normal"
df = data.frame(relwt,glufast,glutest,instest,sspg,group)
predict(qda_fit,df)
predict(lda_fit,df)
