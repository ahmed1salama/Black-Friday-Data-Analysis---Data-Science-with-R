rm(list=ls())
setwd("E:/4fourthyear\\second\\Big Data Analytics\\Project\\Black-Friday-Data-Analysis-Data-Science-with-R")
library(corrplot)
library(dplyr)
library(plyr)
library(caTools)
library(rpart)
library(rpart.plot)

data = read.csv("train_cleaned.csv")

sapply(data, FUN  = class)

#convert categorical data into factors
data$Occupation = as.factor(data$Occupation)
data$Product_Category_1 = as.factor(data$Product_Category_1)
data$Product_Category_2 = as.factor(data$Product_Category_2)
data$Product_Category_3 = as.factor(data$Product_Category_3)

#drop User_ID and Product_ID
data$Product_ID = NULL
data$User_ID = NULL

#data division train/test 
sample = sample.split(data$Purchase, SplitRatio = .8)
train = subset(data, sample == T)
test = subset(data, sample == F)

######################################################### Model 1 Linear Regression #########################################
#model building 
model = lm(Purchase ~., train)
summary(model)

# Train Evaluation
#train prediction
predict = predict(model, train[,-10]) 

#model evaluation
results = cbind(predict, train$Purchase)
colnames(results) = c('pred','real')
head(results)
results = as.data.frame(results)
sqrt(mean((results$real - results$pred)^2))/sd(train$Purchase)

# Test Evaluation
#test prediction
predict = predict(model, test[,-10]) 

#model evaluation
results = cbind(predict, test$Purchase)
colnames(results) = c('pred','real')
head(results)
results = as.data.frame(results)
sqrt(mean((results$real - results$pred)^2))/sd(test$Purchase)

 

######################################################### Model 2 Desicion Tree #########################################
#model building 
tree = rpart(Purchase ~., method = 'anova',train)
prp(tree)

# Train Evaluation
#train prediction
predicted = predict(tree, train[,-10])
predicted = as.data.frame(predicted)
head(predicted)

#model evalutaion 
result = cbind(train$Purchase, predicted$predicted)
colnames(result) = c('real','pred')
result = as.data.frame(result)
head(result)
sqrt(mean((result$real - result$pred)^2))/sd(train$Purchase)

# Test Evaluation
#test prediction
predicted = predict(tree, test[,-10])
predicted = as.data.frame(predicted)
head(predicted)

#model evalutaion 
result = cbind(test$Purchase, predicted$predicted)
colnames(result) = c('real','pred')
result = as.data.frame(result)
head(result)
sqrt(mean((result$real - result$pred)^2))/sd(test$Purchase)


