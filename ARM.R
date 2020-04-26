# Association Rule Mining

library(arules)
library(arulesViz)
library(dplyr)
library(tidyverse)



rm(list=ls())
setwd("E:/4fourthyear\\second\\Big Data Analytics\\Project\\Black-Friday-Data-Analysis-Data-Science-with-R")

data = read.csv("train_cleaned.csv")

# Data preprocessing 
customers_products = data %>% select(User_ID , Product_ID) %>% group_by(User_ID) %>% arrange(User_ID) %>% mutate(id = row_number()) %>% spread(User_ID, Product_ID) %>% t()   
customers_products = customers_products[-1,]

# write new data
write.csv(customers_products, file = 'customers_products.csv')

customersProducts = read.transactions('customers_products.csv', sep = ',', rm.duplicates = TRUE)

inspect(head(customersProducts,2))

summary(customersProducts)

itemFrequencyPlot(customersProducts, topN=15 , main="Item Frequency" , col = "lightblue") 
dev.copy(png, file="../item_frequency_plot.png", width=480, height=480)
dev.off()

# Aprioir with support = 0.009 and confidence = 0.5
rules = apriori (customersProducts, parameter = list(supp = 0.009, conf = 0.70 ,minlen = 2))


rules_supp <- sort (rules, by="support", decreasing=TRUE)
inspect(head(rules_supp,6))


rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
inspect(head(rules_conf,6))


rules_lift <- sort (rules, by="lift", decreasing=TRUE)
inspect(head(rules_lift,6))
