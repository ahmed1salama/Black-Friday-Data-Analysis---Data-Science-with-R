
# Data is obtained from: https://www.kaggle.com/sdolezel/black-friday
# Data is divided into two csv files 1-train.csv 2-test.csv
rm(list=ls())
setwd("E:/4fourthyear\\second\\Big Data Analytics\\Project\\Black-Friday-Data-Analysis-Data-Science-with-R")

# 1- Reading data
train  = read.csv("train.csv")
dim(train)

test = read.csv("test.csv")
dim(test)

# 2- Explore Variables
names(train)
str(train)

# 3- Data Cleaning 
# Basically our data cleaning is going to be dealing with NAs values
any(is.na(train))
head(train)

# from here we can see that we got NA values in columns product_Category_2 and product_Category_3
# but lets check for the whole variables
any(is.na(train$User_ID))
any(is.na(train$Product_ID))
any(is.na(train$Gender))
any(is.na(train$Age))
any(is.na(train$Occupation))
any(is.na(train$City_Category))
any(is.na(train$Stay_In_Current_City_Years))
any(is.na(train$Marital_Status))
any(is.na(train$Product_Category_1))
any(is.na(train$Product_Category_2))
any(is.na(train$Product_Category_3))
any(is.na(train$Purchase))

# now we are sure that only columns product_Category_2 and product_Category_3 have NA values 
# we can assume that having NA value means that the user did not purchase this product from those category
# which means that the user purchased count = 0 from this product from those category
# so we will replace NA values with zeros 
train[is.na(train$Product_Category_2),"Product_Category_2"] = 0
train[is.na(train$Product_Category_3),"Product_Category_3"] = 0
# now check again we will have no NA values
any(is.na(train$Product_Category_2))
any(is.na(train$Product_Category_3))
head(train)

# lets do the same for the test data
names(test)
any(is.na(test))
str(test)
head(test)

any(is.na(test$User_ID))
any(is.na(test$Product_ID))
any(is.na(test$Gender))
any(is.na(test$Age))
any(is.na(test$Occupation))
any(is.na(test$City_Category))
any(is.na(test$Stay_In_Current_City_Years))
any(is.na(test$Marital_Status))
any(is.na(test$Product_Category_1))
any(is.na(test$Product_Category_2))
any(is.na(test$Product_Category_3))

# now we are also sure that only columns product_Category_2 and product_Category_3 have NA values  

# lets replace them 
test[is.na(test$Product_Category_2),"Product_Category_2"] = 0
test[is.na(test$Product_Category_3),"Product_Category_3"] = 0

# check again
any(is.na(test$Product_Category_2))
any(is.na(test$Product_Category_3))
head(train)
# now we have cleaned our data let's output our new data to train_cleaned.csv, test_cleaned.csv
write.csv(train,"train_cleaned.csv",row.names=FALSE)
write.csv(test,"test_cleaned.csv",row.names=FALSE)

