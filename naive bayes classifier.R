library(dplyr)
library(caret)
library(Hmisc)
library(readr)
Insurance_Dataset_ <- read_csv("D:/new/Insurance Dataset .csv")
#View(Insurance_Dataset_)
Insurance_Dataset_ <- Insurance_Dataset_[,-c(1:5,7,14,16,18,21,26:27,31:32)]
str(Insurance_Dataset_)
data_factor <- as.data.frame(lapply(Insurance_Dataset_[,-c(16,19)],factor))
str(data_factor)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
data_norm<-as.data.frame(lapply(Insurance_Dataset_[,c(16,19)],FUN=normalize))
final_data <- data.frame(data_factor,data_norm)
str(final_data)
sum(is.na(final_data))
summary(final_data)

final_data["Days_spend_hsptl"] <- with(final_data,impute(final_data$Days_spend_hsptl,mode))
final_data["Description_illness"] <- with(final_data,impute(final_data$Description_illness,mode))
final_data["Mortality_risk"] <- with(final_data,impute(final_data$Mortality_risk,mode))
summary(final_data)
sum(is.na(final_data))

set.seed(3)
final_data_1<- final_data[sample(nrow(final_data)),]
train <- final_data_1[1:as.integer(0.70*nrow(final_data)),]
test <- final_data_1[-c(1:as.integer(0.70*nrow(final_data))),]

# Naive Bayes Model
library(e1071)
model_train<-naiveBayes(Result~.,data = train)

pred_train <- predict(model_train,train[,-18])
pred_train <- if_else(pred_train == 2,"Fraudulent","Genuine")
pred_train <- as.factor(pred_train)
confusionMatrix(pred_train,train$Result)

pred_test <- predict(model_train,test[,-18])
pred_test <- if_else(pred_test == 2,"Fraudulent","Genuine")
pred_test <- as.factor(pred_test)
confusionMatrix(pred_test,test$Result)



# Up Sampling
train <- upSample(train,train$Result)
model_train<-naiveBayes(Result~.,data = train[,-21])

pred_train <- predict(model_train,train[,-c(18,21)])
pred_train <- if_else(pred_train == 2,"Fraudulent","Genuine")
pred_train <- as.factor(pred_train)
confusionMatrix(pred_train,train$Result)

pred_test <- predict(model_train,test[,-c(18,21)])
pred_test <- if_else(pred_test == 2,"Fraudulent","Genuine")
pred_test <- as.factor(pred_test)
confusionMatrix(pred_test,test$Result)





# Down Sampling
train <- downSample(train,train$Result)
model_train<-naiveBayes(Result~.,data = train[,-21])

pred_train <- predict(model_train,train[,-c(18,21)])
pred_train <- if_else(pred_train == 2,"Fraudulent","Genuine")
pred_train <- as.factor(pred_train)
confusionMatrix(pred_train,train$Result)

pred_test <- predict(model_train,test[,-c(18,21)])
pred_test <- if_else(pred_test == 2,"Fraudulent","Genuine")
pred_test <- as.factor(pred_test)
confusionMatrix(pred_test,test$Result)
