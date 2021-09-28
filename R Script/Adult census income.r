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
Data_Orderby_Age_income_C<-Data_Orderby_Age_income[order(Data_Orderby_Age_income$income_c,decreasing = TRUE),]
j<-1
while(j<=nrow(Data_Orderby_Age_income)&Data_Orderby_Age_income_C$income_c[j] =="Not applicable"){
  Data_Orderby_Age_income_C$income_c[j] <- Total_Capital[j]
    j=j+1
}
Data_Orderby_Age_income_C<-Data_Orderby_Age_income_C[order(Data_Orderby_Age_income_C$income_c,Data_Orderby_Age_income_C$age,decreasing = TRUE),]
rm(Data2,Data_Orderby_Age_income,i,j,Total_Capital)
the_50th_Rate_of_income<-sum(as.integer(Data_Orderby_Age_income_C$income_c[1:50]))/50
the_50th_Rate_of_Age<-sum(as.integer(Data_Orderby_Age_income_C$age[1:50]))/50
print("There is no relationship between the age and the income of the individual, as the average age of the top fifty individuals is 39 years and the average income is 9244.7 per year")
rm(the_50th_Rate_of_income,the_50th_Rate_of_Age,rang)
#********************************************************************************************

#2. Can we say that we have achieved gender equality?
count(Data_Orderby_Age_income_C,Gender)
Data_Orderby_Age_income_C[Data_Orderby_Age_income_C$Gender == "Male" | 
                            Data_Orderby_Age_income_C$Gender == "m "|
                            Data_Orderby_Age_income_C$Gender == "malee"|
                            Data_Orderby_Age_income_C$Gender == "male"|
                            Data_Orderby_Age_income_C$Gender == "m", "Gender"] <- "M"
Data_Orderby_Age_income_C[Data_Orderby_Age_income_C$Gender == "Female" | 
                            Data_Orderby_Age_income_C$Gender == "female"|
                            Data_Orderby_Age_income_C$Gender == "f", "Gender"] <- "F"
Data_Male<-Data_Orderby_Age_income_C[Data_Orderby_Age_income_C$Gender=="M",]
Data_Female<-Data_Orderby_Age_income_C[Data_Orderby_Age_income_C$Gender=="F",]
Sum_income_Female<-sum(as.integer(Data_Female$income_c),na.rm = TRUE)
Sum_income_Male<-sum(as.integer(Data_Male$income_c),na.rm = TRUE)
the_Rato_of_income_for_Male<-Sum_income_Male/nrow(Data_Male)
the_Rato_of_income_for_Female<-Sum_income_Female/nrow(Data_Female)
Education_Levels_Male<-count(Data_Male,education)
Education_Levels_Female<-count(Data_Female,education)
The_AVG_of_HSgrad_Male<-round((Education_Levels_Male$n[12]/nrow(Data_Male))*100,2)
The_AVG_of_Bachelor_Male<-round((Education_Levels_Male$n[10]/nrow(Data_Male))*100,2)
The_AVG_of_HSgrad_Female<-round((Education_Levels_Female$n[12]/nrow(Data_Female))*100,2)
The_AVG_of_Bachelor_Female<-round((Education_Levels_Female$n[10]/nrow(Data_Female))*100,2)
print(paste("The Number of Male is",nrow(Data_Male),
            "The Number of Female is",nrow(Data_Female),
            "Male to female ratio is",round((nrow(Data_Male)/nrow(Data))*100,2),"% for Male and",round((nrow(Data_Female)/nrow(Data))*100,2),"% for Female",
            "and I'm find that The average income of a male is",round(the_Rato_of_income_for_Male,3),"$",
            "and The average income of a Female is",round(the_Rato_of_income_for_Female,3),"$",
            "I analyzed the data of males and females in terms of education and found that the numbers are close, but we need to increase the effort in educating more women, as, for example, but not limited to.The percentage of males in secondary schools is",
            The_AVG_of_HSgrad_Male,"and The percentage of women in secondary schools is",
            The_AVG_of_HSgrad_Female,"Also, the percentage of males with a bachelor's degree is",
            The_AVG_of_Bachelor_Male,"But the percentage of women with a bachelor's degree is",
            The_AVG_of_Bachelor_Female))
rm(Data_Male,Data_Female,Sum_income_Female,Sum_income_Male,
   the_Rato_of_income_for_Male,the_Rato_of_income_for_Female,
   Education_Levels_Male,Education_Levels_Female,The_AVG_of_HSgrad_Male,
   The_AVG_of_Bachelor_Male,The_AVG_of_HSgrad_Female,
   The_AVG_of_Bachelor_Female
   )
#********************************************************************************************

#3. What other attributes would be an income predictor/s?
Data_orderBy_Hours<-Data_Orderby_Age_income_C[order(Data_Orderby_Age_income_C$hours.per.week,decreasing = TRUE),]
Data_orderBy_EducationNUM<-Data_Orderby_Age_income_C[order(Data_Orderby_Age_income_C$education.num ,decreasing = TRUE),]
the_50th_Rate_of_Hours<-sum(as.integer(Data_orderBy_Hours$hours.per.week [1:50]))/50
the_50th_Rate_of_Hours_INcome<-sum(as.integer(Data_orderBy_Hours$income_c [1:50]),na.rm = TRUE)/50
the_50th_Rate_of_Education_NUM<-sum(as.integer(Data_orderBy_EducationNUM$education.num [1:50]))/50
the_50th_Rate_of_EducationNUM_INcome<-sum(as.integer(Data_orderBy_EducationNUM$income_c [1:50]),na.rm = TRUE)/50
print(paste("Yes, there are some other characteristics that may affect the income, such as the degree of education(Education NUM), and the number of hours worked for each week",
      "The average working hours for the first 50 people are:",the_50th_Rate_of_Hours,"hours",
      "And the average income according to the working hours for the First 50 people is",the_50th_Rate_of_Hours_INcome,
      "$","Whereas, the average education level or education level for the First 50 people is",the_50th_Rate_of_Education_NUM,
      "The average income according to the degree or level of education for the First 50 people",
      the_50th_Rate_of_EducationNUM_INcome,"$"
      ))

#©Mahmoud Sayed Youssef