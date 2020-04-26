
rm(list=ls())
setwd("E:/4fourthyear\\second\\Big Data Analytics\\Project\\Black-Friday-Data-Analysis-Data-Science-with-R")
library(dplyr)
library(ggplot2)
library(ggpubr)

# 1- Read the preprocessed(cleaned) data
# here we focus only on explanatory analysis so we will only use train data

data = read.csv("train_cleaned.csv")

summary(data)
head(data)

# 1-Gender
gender = data %>% select(User_ID, Gender) %>% group_by(User_ID) %>% distinct()  
head(gender)
summary(gender)

# gender distribution 
ggplot(data = gender) +
  geom_bar(mapping = aes(x = Gender, y = ..count.., fill = Gender)) +
  labs(title = 'Gender of Customers')
dev.copy(png, file="../gender_distribution.png", width=480, height=480)
dev.off()

# average purchasing amount vs gender
total_purchase_gender = data %>%
  select(Gender, Purchase) %>%
  group_by(Gender) %>%
  summarise(Total_Purchase = sum(as.numeric(Purchase)))
total_purchase_gender

ggplot(data = total_purchase_gender) +
  geom_bar(mapping = aes(x = Gender, y = Total_Purchase, fill = Gender), stat = 'identity') +
  labs(title = 'Total Purchase by Gender')

dev.copy(png, file="../Total_gender_purchase.png", width=480, height=480)
dev.off()

###################################################################################################################

# 2-best sellers
best_sellers = data %>% count(Product_ID, sort = T)
best_sellers

best_seller = data[data$Product_ID == "P00265242",]
head(best_seller)

# best seller vs gender
ggplot(data = best_seller) +
  geom_bar(mapping = aes(x = Gender, y = ..count.., fill = Gender)) +
  labs(title = 'Best Seller vs Gender Distribution')
dev.copy(png, file="../best_seller_gender_distribution.png", width=480, height=480)
dev.off()


# best seller gender distribution vs total purchasing by gender
plot1 = ggplot(data = gender) +
  geom_bar(mapping = aes(x = Gender, y = ..count.., fill = Gender)) +
  labs(title = 'Gender of Customers')

plot2 = ggplot(data = best_seller) +
  geom_bar(mapping = aes(x = Gender, y = ..count.., fill = Gender)) +
  labs(title = 'Best Seller vs Gender Distribution')

ggarrange(plot1, plot2, ncol=2)
dev.copy(png, file="../best_seller_vs_total_distribution.png", width=480, height=480)
dev.off()
##########################################################################################################################

# 3-Age
Age = data %>% select(User_ID, Age) %>% distinct() %>% count(Age)
Age
ggplot(data = Age) +
  geom_bar(mapping = aes(x = Age, y = n), stat= "identity") +
  labs(title = 'Age Distribution')

dev.copy(png, file="../Age_Distribution.png", width=480, height=480)
dev.off()

# products bought by buyers between 26-35 years old
Age_products  = data[data$Age=="26-35",c("Product_ID","Age")]  %>% count(Product_ID, sort = T) 
Age_products

ggplot(data = Age_products[1:5,]) +
  geom_bar(mapping = aes(x = Product_ID, y = n), stat= "identity") +
  labs(title = 'Products vs Age = 26-35')

dev.copy(png, file="../Age_Products_Distribution.png", width=500, height=500)
dev.off()
##########################################################################################################################

# 4-City

City_Count = data %>% select(City_Category,User_ID) %>% distinct() %>% count(City_Category)
City_Count

plot1 = ggplot(data = City_Count) +
  geom_bar(mapping = aes(x = City_Category, y = n , fill=City_Category), stat= "identity") +
  labs(title = 'Customer Frequency by City')

dev.copy(png, file="../Customer_frequency_city.png", width=500, height=500)
dev.off()

# total purchase count by city
City_Purchase_Count = data %>% select(City_Category) %>% count(City_Category)
City_Purchase_Count
plot2 = ggplot(data = City_Purchase_Count) +
  geom_bar(mapping = aes(x = City_Category, y = n , fill=City_Category), stat= "identity") +
  labs(title = 'City Purchase Count')
dev.copy(png, file="../City_Purchase_Count.png", width=500, height=500)
dev.off()

# total purchase amount by city
City_Purchase_amount = data %>% select(City_Category,Purchase) %>% group_by(City_Category) %>% summarise(City_Purchase_amount=sum(as.numeric(Purchase)))
City_Purchase_amount

plot3 = ggplot(data = City_Purchase_amount) +
  geom_bar(mapping = aes(x = City_Category, y = City_Purchase_amount , fill=City_Category), stat= "identity") +
  labs(title = 'City Purchase amount')
dev.copy(png, file="../City_Purchase_amount.png", width=500, height=500)
dev.off()
ggarrange(plot1, plot2, plot3, ncol=3)

dev.copy(png, file="../City_customers_Purchase_Count_amount.png", width=1000, height=500)
dev.off()

##########################################################################################################################

# 5-Stay in current City
stay_in_city = data %>% select(Stay_In_Current_City_Years,Purchase) %>% group_by(Stay_In_Current_City_Years) %>% summarise(Total_Purchase = sum(as.numeric(Purchase)))
stay_in_city

ggplot(data = stay_in_city) +
  geom_bar(mapping = aes(x = Stay_In_Current_City_Years, y = Total_Purchase , fill=Stay_In_Current_City_Years), stat= "identity") +
  labs(title = 'Total Purchase amount by Stay in city Years')
dev.copy(png, file="../Purchase_Stay_In_City_Years.png", width=500, height=500)
dev.off()
# stay in city duaration in years 
stay_duration = data %>% select(Stay_In_Current_City_Years,City_Category,User_ID) %>% distinct() %>% group_by(City_Category) %>% count(Stay_In_Current_City_Years)
stay_duration

ggplot(data = stay_duration) +
  geom_bar(mapping = aes(x = City_Category, y = n , fill=Stay_In_Current_City_Years), stat= "identity") +
  labs(title = 'Stay duration in city Years')
dev.copy(png, file="../Stay_Duaration_In_City_Years.png", width=500, height=500)
dev.off()

##########################################################################################################################

# 6-Marital Status
data$Marital_Status = as.character(data$Marital_Status)
marital_status = data %>% select(Marital_Status,User_ID) %>% distinct() %>% count(Marital_Status)
marital_status
ggplot(data = marital_status) +
  geom_bar(mapping = aes(x = Marital_Status, y = n , fill=Marital_Status), stat= "identity") +
  labs(title = 'Customers Marital Status')
dev.copy(png, file="../Customers_Marital_Status.png", width=500, height=500)
dev.off()
# purchase amount by marital status 
purchase_marital_status = data %>% select(Marital_Status,Purchase) %>% group_by(Marital_Status) %>% summarise(Total_Purchase = sum(as.numeric(Purchase)))
purchase_marital_status
ggplot(data = purchase_marital_status) +
 geom_bar(mapping = aes(x = Marital_Status, y = Total_Purchase , fill=Marital_Status), stat= "identity") +
 labs(title = 'Total Purchase by Marital Sttus')
dev.copy(png, file="../Purchase_Marital_Status.png", width=500, height=500)
dev.off()

##########################################################################################################################

# 7-Occupation 
occupation = data %>% select(Occupation, Purchase) %>% group_by(Occupation) %>% summarise(Total_Purchase = sum(as.numeric(Purchase)))
occupation
occupation = occupation[order(occupation$Total_Purchase, decreasing = T),]
occupation
ggplot(data = occupation) +
  geom_bar(mapping = aes(x = Occupation, y = Total_Purchase , fill=Occupation), stat= "identity") +
  labs(title = 'Total Purchase by Occupation')
dev.copy(png, file="../Purchase_Occupation.png", width=500, height=500)
dev.off()

##########################################################################################################################

# 7-Product categories
product_category_1 = data %>% select(Product_Category_1) %>% count(Product_Category_1)
product_category_1
plot1 = ggplot(data = product_category_1) +
  geom_bar(mapping = aes(x = Product_Category_1, y = n , fill=Product_Category_1), stat= "identity") +
  labs(title = 'Product Category 1')
product_category_2 = data %>% select(Product_Category_2) %>% count(Product_Category_2)
product_category_2
plot2 = ggplot(data = product_category_2) +
  geom_bar(mapping = aes(x = Product_Category_2, y = n , fill=Product_Category_2), stat= "identity") +
  labs(title = 'Product Category 2')
product_category_3 = data %>% select(Product_Category_3) %>% count(Product_Category_3)
product_category_3
plot3 = ggplot(data = product_category_3) +
  geom_bar(mapping = aes(x = Product_Category_3, y = n , fill=Product_Category_3), stat= "identity") +
  labs(title = 'Product Category 3')
ggarrange(plot1, plot2, plot3, nrow = 3)

dev.copy(png, file="../product_Categories.png", width=1000, height=500)
dev.off()
