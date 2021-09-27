#This is my Final Project for "57357 Data Analysis with R Training 2021"
library(tidyverse)
library("ggplot2")
setwd("../")
Data<-read.csv("Data sheet/census_income_original_2.csv")
summary(Data)
names(Data)
dim(Data)
#1. Is there a relationship between age and income?
rang<-range(Data$age)
The_small_people<-Data[which((Data$age)==rang[1]),]
The_grand_people<-Data[which((Data$age)==rang[2]),]
Data[Data$income == ">50K","income"] <- 0
Data[Data$income == "<=50K","income"] <- 1
as.integer(Data$income)
Data2 <- Data[order(Data$income,Data$capital.gain,Data$age,decreasing = TRUE),]
Data_Orderby_Age_income<-Data2[order(Data2$capital.loss),]
summary(Data_Orderby_Age_income)
i<-1
Total_Capital<-c(1:nrow(Data_Orderby_Age_income))
while(i<= nrow(Data)){
  Total_Capital[i]=(Data_Orderby_Age_income$capital.gain[i])-(Data_Orderby_Age_income$capital.loss[i])
  i=i+1
}
j<-1
while(j<=nrow(Data_Orderby_Age_income)){
  if(is.na(Data_Orderby_Age_income$income_c[j])|Data_Orderby_Age_income$income_c[j] =='Not applicable')
 Data_Orderby_Age_income$income[j] <- Total_Capital[j]
    j=j+1
}
Data_Orderby_Age_income_C<-Data_Orderby_Age_income[order(Data_Orderby_Age_income$income_c,decreasing = TRUE),]





#© Mahmoud Sayed Youssef