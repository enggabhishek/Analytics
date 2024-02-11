#Load library ----
library(car)
library(tidyverse)
library(stringr)
library(dplyr)
library(data.table)
library(magrittr)
library(sqldf)
library(ggplot2)
library(hash)
library(psych)
#To see the list of packages loaded
(.packages())

#Import 'StudentsPerformance.csv' dataset:

setwd("C:/Users/XYZ")

dataset<- read.csv("StudentsPerformance.csv", header=TRUE)

print(head(dataset))

#Remove 'test preparation course' column from the data set
dataset<- dataset[ -c(5) ]

#renaming the columns in the data set
colnames(dataset)<- c("gender", "race_or_ethnicity", "level_of_education", "lunch_status","math_scores","reading_scores","writing_scores")

#Create a column 'Total' to get the average scores math, reading and writing scores
dataset["total"] <- (dataset['math_scores'] + dataset['reading_scores'] + dataset['writing_scores'])/3

#box plot to show math scores based on genders
boxplot(math_scores~gender,data=dataset,col = c('purple','lightblue'))

#H0 difference between the mean of male and female based on the math are equal to 10
#H1 difference between the mean of male and female based on the math are not equal to 10
t.test(dataset$math_scores~dataset$gender, mu=10, alt = 'two.sided', conf = 0.95, var.eq = F, paired = F)

#box plot to show reading scores based on genders
boxplot(reading_scores~gender,data=dataset,col = c('violet','lightgreen'))

#H0 difference between the mean of male and female based on the reading scores are equal to 5
#H1 difference between the mean of male and female based on the reading scores are greater than 5
t.test(dataset$reading_scores~dataset$gender, mu=5, alt = 'greater', conf = 0.95, var.eq = F, paired = F)


#box plot to show writing scores based on genders
boxplot(writing_scores~gender,data=dataset,col = c('green','yellow'))

#H0 difference between the mean of male and female based on the writing scores are equal to 5
#H1 difference between the mean of male and female based on the writing scores are less than 5
t.test(dataset$writing_scores~dataset$gender, mu=5, alt = 'less', conf = 0.95, var.eq = F, paired = F)

#box plot to show total scores based on genders
boxplot(total~gender,data=dataset, col = c('red','green'))

#H0 difference between the mean of male and female based on the total scores are equal to 5
#H1 difference between the mean of male and female based on the total scores are less than 5
t.test(dataset$total~dataset$gender, mu=5, alt = 'less', conf = 0.95, var.eq = F, paired = F)
