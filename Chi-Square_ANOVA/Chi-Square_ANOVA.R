library(ggplot2)
library(psych)
library(dplyr)
library(MASS)
library(plyr)
library(magrittr)
library(tidyverse)
library(kableExtra)
library(broom)
library(reshape2)
library(flextable)
#==================================Q11-1.6======================================
#Significance Level
alpha1 <- 0.10

#Observed values of Blood Types
observed_values1 <- c(12,8,24,6)

#Create a vector of probabilities based on the expected values
prob_expected <- c(0.20, 0.28, 0.36, 0.16)

expected_values <- 50*prob_expected
expected_values

combined_df <- data.frame(observed_values1,expected_values)
colnames(combined_df) <- c('Observed Values', 'Expected Values')

#Summary of the combined_df
describe(combined_df)[0:5]%>%
  round(2) %>% 
  knitr::kable(align = "r")%>%
  kable_paper()

#Displaying box_plot for comparing the observed and expected values of blood type of patients
bx_plt <- ggplot(melt(combined_df), aes(x = variable, y=value)) + 
  ggtitle("Box plot showing population of different blood types of patients") +
  xlab('Blood Types') + ylab('Population') +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "lightblue",colour = "lightblue", size = 0.5, linetype = "solid")) +
  geom_boxplot(fill = rainbow(ncol(combined_df)))
show(bx_plt)

#Performing Chi-Square Goodness of Fitness Test 
output <- chisq.test(x = observed_values1, p = prob_expected)

testValue <- output$statistic #Chi-square test value
pValue<- output$p.value  #Chi-square p-value
doF<- output$parameter #degrees of freedom

#Creating flextable
flextable(data.frame(testValue,pValue,doF))

#H0: There is no difference in mean of the Blood Types.
#H1: There is the difference in mean of the Blood Types.
ifelse(output$p.value>alpha1, "Fail to reject null Hypothesis", "Reject Null Hypothesis")
#=================================Q11-1.8=======================================
#Significance Level
alpha2 <- 0.05

#Observed values of On-Time Performance by Airlines
observed_values2 <- c(125,10,25,40)

#Create a vector of probabilities based on the expected values
prob_expected2 <- c(0.708, 0.082, 0.09, 0.12)
expected_values2 <- 200*prob_expected2

airlines_df <- data.frame(observed_values2,expected_values2)
colnames(airlines_df) <- c('Observed Values', 'Expected Values')

#Summary of the airlines_df
describe(airlines_df)[0:5]%>%
  round(2) %>% 
  knitr::kable(align = "r")%>%
  kable_paper()

#Displaying box_plot for comparing the observed and expected values of airlines time- action performance  
bx_plt2 <- ggplot(melt(airlines_df), aes(x = variable, y=value)) + 
  ggtitle("Box plot showing time taken by different airlines to reach the destination") +
  xlab('Action') + ylab('Number of Airlines') +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "lightgreen",colour = "lightgreen", size = 0.5, linetype = "solid")) +
  geom_boxplot(fill = rainbow(ncol(airlines_df)))
show(bx_plt2)

#Performing Chi-Square Goodness of Fitness Test 
output2 <- chisq.test(x = observed_values2, p = prob_expected2)


testValue <- output2$statistic #Chi-square test value
pValue<- output2$p.value  #Chi-square p-value
doF<- output2$parameter #degrees of freedom

#Creating flex table
flextable(data.frame(testValue,pValue,doF))

#H0: There is no difference in mean of the On-Time Performance by Airlines.
#H1: There is the difference in mean of the On-Time Performance by Airlines.
ifelse(output2$p.value>alpha2, "Fail to reject null Hypothesis", "Reject Null Hypothesis")
#===================================Q11-2.8=====================================
#Significance Level
alpha3 <- 0.05

#Observed values of Ethnicity and Movie Admissions 
vector1 <- c(724,335,174,107)
vector2 <- c(370,292,152,140)
rows = 2

#Creating Matrix of observed values 
mtrx <- matrix(c(vector1,vector2), nrow = rows, byrow = TRUE)
row.names(mtrx) <- c('2013', '2014')
colnames(mtrx) <- c('Caucasian', 'Hispanic', 'African American', 'African American')

ethnicity_df <- data.frame(vector1, vector2)

colnames(ethnicity_df) <- c('2013', '2014')

#Summary of the ethnicity_df
describe(ethnicity_df)[0:5]%>%
  round(2) %>% 
  knitr::kable(align = "r")%>%
  kable_paper()

#Displaying box_plot for comparing the values of ethnicity and Movie Admissions in multiple years  
bx_plt3<- ggplot(melt(ethnicity_df), aes(x = variable, y=value)) + 
  ggtitle("Box plot showing Ethnicity and Movie Admissions in multiple years") +
  xlab('Year') + ylab('Total Admissions') +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "yellow",colour = "yellow", size = 0.5, linetype = "solid")) +
  geom_boxplot(fill = rainbow(ncol(airlines_df)))
show(bx_plt3)

#Performing Chi-Square Goodness of Fitness Test 
output3 <- chisq.test(mtrx)

testValue <- output3$statistic #Chi-square test value
pValue <- output3$p.value  #Chi-square p-value
doF<- output3$parameter #degrees of freedom ('rows - 1' * 'columns - 1')

#Creating flex table
flextable(data.frame(testValue,pValue,doF))

#H0: There is no difference in mean of the Ethnicity and Movie Admissions in the year 2013 and 2014.
#H1: There is the difference in mean of the Ethnicity and Movie Admissions in the year 2013 and 2014.
ifelse(output3$p.value>alpha3, "Fail to reject null Hypothesis", "Reject Null Hypothesis")
#===============================================================================
#====================================Q11-2.10===================================
#Significance Level
alpha4 <- 0.05

#Observed values of Women in the Military 
vector3 <- c(10791,7816,932,11819)
vector4 <- c(62491,62491,9525,54344)
rows = 4

#Creating Matrix of observed values
mtrx2 <- matrix(c(vector3,vector4), nrow = rows, byrow = TRUE)
row.names(mtrx2) <- c('Army', 'Navy', 'Marine Corps', 'Air Force')
colnames(mtrx2) <- c('Officers', 'Enlisted')
mtrx2

military_df <- data.frame(vector3, vector4)

colnames(military_df) <- c('Officers', 'Enlisted')

#Summary of the military_df
describe(military_df)[0:5]%>%
  round(2) %>% 
  knitr::kable(align = "r")%>%
  kable_paper()

#Displaying box_plot for comparing the values of anticipation of Women in the Military  
bx_plt4<- ggplot(melt(military_df), aes(x = variable, y=value)) + 
  ggtitle("Box plot showing anticipation of Women in the Military") +
  xlab('Status of Selection') + ylab('Total Participation') +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "lightblue",colour = "lightblue", size = 0.5, linetype = "solid")) +
  geom_boxplot(fill = rainbow(ncol(military_df)))
show(bx_plt4)

#Performing Chi-Square Goodness of Fitness Test 
output4 <- chisq.test(mtrx2)

testValue <- output4$statistic #Chi-square test value
pValue <- output4$p.value  #Chi-square p-value
doF<- output4$parameter #degrees of freedom ('rows - 1' * 'columns - 1')

#Creating flex table
flextable(data.frame(testValue,pValue,doF))

#H0: There is no difference in mean of the Women who has been enlisted or officers in the Military.
#H1: There is the difference in mean of the Women who has been enlisted or officers in the Military.
ifelse(output4$p.value>alpha4, "Fail to reject null Hypothesis", "Reject Null Hypothesis")
#====================================Q12-1.8====================================
#Significance Level
alpha5 <- 0.05

#Creating dataframe of observed values of different types of food
condiments <- data.frame('sodium' = c(270, 130, 230, 180, 80, 70, 200), food = rep('codiments', 7), stringsAsFactors = FALSE)
cereals <- data.frame('sodium' = c(260, 220, 290, 290, 200, 320, 140), food = rep('cereals', 7), stringsAsFactors = FALSE)
desserts <- data.frame('sodium' = c(100, 180, 250, 250, 300, 360, 300, 160), food = rep('desserts', 8), stringsAsFactors = FALSE)


#Creating data frame 'sodium' to consolidate the above data frames containing different types of food
sodium <- rbind(condiments, cereals, desserts)
sodium$food <- as.factor(sodium$food)

#Summary of the sodium
describe(sodium)[0:5]%>%
  round(2) %>% 
  knitr::kable(align = "r")%>%
  kable_paper()
#combined data of sodium content in different food
df.m <- melt(sodium, id.var = "food")

#Displaying box_plot for comparing the values of content of sodium in the different types of food  
bx_plt5<- ggplot(df.m, aes(x = variable, y=value)) + 
  ggtitle("Box plot showing content of sodium in different food") +
  xlab('Types of Food') + ylab('Sodium Content') +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "lightblue",colour = "lightblue", size = 0.5, linetype = "solid")) +
  geom_boxplot(aes(fill=food))
show(bx_plt5)

#Performing ANOVA test
anov1 <- aov(sodium ~ food, data = sodium)

#plotting graph based on ANOVA test
plot(anov1)

#Summary of the ANOVA test
summary(anov1)
a.summary <- summary(anov1)

#Showing Numerator
df.numerator <- a.summary[[1]][1, "Df"]
df.numerator

#Showing Denominator
df.denominator <- a.summary[[1]][2, "Df"]
df.denominator

#Showing F-value
F.value <- a.summary[[1]][[1, "F value"]]
F.value

#Showing p-value
p.value <- a.summary[[1]][[1, "Pr(>F)"]]
p.value

#Creating flex table
flextable(data.frame(df.numerator,df.denominator,F.value,p.value))

#H0: There is no difference in mean of the content of sodium in the different types of food.
#H1: There is the difference in mean of the content of sodium in the different types of food.
ifelse(p.value>alpha5, "Fail to reject null Hypothesis", "Reject Null Hypothesis")
#===================================Q12-2.10====================================
#Significance Level
alpha6 <- 0.01

#Creating dataframe of observed values of different types of foods
cereal <- data.frame('sales' = c(578, 320, 264, 249, 237), foods = rep('cereal', 5), stringsAsFactors = FALSE)
chocolateCandy <- data.frame('sales' = c(311, 106, 109, 125, 173), foods = rep('chocolateCandy', 5), stringsAsFactors = FALSE)
coffee <- data.frame('sales' = c(261, 185, 302, 689), foods = rep('coffee', 4), stringsAsFactors = FALSE)

#Creating data frame 'sales' to consolidate the above data frames containing different types of foods
sales <- rbind(cereal, chocolateCandy, coffee)
sales$foods <- as.factor(sales$foods)

#Summary of the sodium
describe(sales)[0:5]%>%
  round(2) %>% 
  knitr::kable(align = "r")%>%
  kable_paper()

#combined data of sales of different food
df.s <- melt(sales, id.var = "foods")

#Displaying box_plot for comparing the sales of the different types of food.
bx_plt6<- ggplot(df.s, aes(x = variable, y=value)) + 
  ggtitle("Box plot showing content of sales of different types food") +
  xlab('Types of Food') + ylab('Total Sales') +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "lightblue",colour = "lightblue", size = 0.5, linetype = "solid")) +
  geom_boxplot(aes(fill=foods))
show(bx_plt6)

#Performing ANOVA test
anov2 <- aov(sales ~ foods, data = sales)

#plotting graph based on ANOVA test
plot(anov2)


#Summary of the ANOVA test
summary(anov2)
a2.summary <- summary(anov2)

#Showing Numerator
df2.numerator <- a2.summary[[1]][1, "Df"]
df2.numerator

#Showing Denominator
df2.denominator <- a2.summary[[1]][2, "Df"]
df2.denominator

#Showing F-value
F2.value <- a2.summary[[1]][[1, "F value"]]
F2.value

#Showing p-value
p2.value <- a2.summary[[1]][[1, "Pr(>F)"]]
p2.value

#Creating flex table
flextable(data.frame(df2.numerator,df2.denominator,F2.value,p2.value))

#H0: There is no difference in mean of the sales of the different types of food.
#H1: There is the difference in mean of the sales of the different types of food.
ifelse(p2.value>alpha6, "Fail to reject null Hypothesis", "Reject Null Hypothesis")
#=================================Q12-2.12======================================
#Significance Level
alpha8 <- 0.05

#Creating dataframe of observed values of Per-Pupil Expenditures
easternThird <- data.frame('expenditures' = c(4946, 5953, 6202, 7243, 6113), pupil = rep('easternThird', 5), stringsAsFactors = FALSE)
middleThird <- data.frame('expenditures' = c(6149, 7451, 6000, 6479), pupil = rep('middleThird', 4), stringsAsFactors = FALSE)
westernThird <- data.frame('expenditures' = c(5282, 8605, 6528, 6911), pupil = rep('westernThird', 4), stringsAsFactors = FALSE)


#Creating dataframe of observed values to consolidate the above data frames containing different types of Expenditures
expenditures <- rbind(easternThird, middleThird, westernThird)
expenditures$pupil <- as.factor(expenditures$pupil)

#Summary of the expenditures
describe(expenditures)[0:5]%>%
  round(2) %>% 
  knitr::kable(align = "r")%>%
  kable_paper()

#combined data of expenditures done by different states of people
df.exp <- melt(expenditures, id.var = "pupil")

#Displaying box_plot for comparing the expenditures of pupil in different states.
bx_plt7<- ggplot(df.exp, aes(x = variable, y=value)) + 
  ggtitle("Box plot showing expenditures of pupil in different states") +
  xlab('Types of States') + ylab('Total Expenditures') +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "lightblue",colour = "lightblue", size = 0.5, linetype = "solid")) +
  geom_boxplot(aes(fill=pupil))
show(bx_plt7)

#Performing ANOVA test
anov3 <- aov(expenditures ~ pupil, data = expenditures)

#plotting graph based on ANOVA test
plot(anov3)

#Summary of the ANOVA test
summary(anov3)
a3.summary <- summary(anov3)

#Showing Numerator
df3.numerator <- a3.summary[[1]][1, "Df"]
df3.numerator

#Showing Denominator
df3.denominator <- a3.summary[[1]][2, "Df"]
df3.denominator

#Showing F-value
F3.value <- a3.summary[[1]][[1, "F value"]]
F3.value

#Showing p-value
p3.value <- a3.summary[[1]][[1, "Pr(>F)"]]
p3.value

#Creating flex table
flextable(data.frame(df3.numerator,df3.denominator,F3.value,p3.value))

#H0: There is no difference in mean of expenditures of the Per-Pupil Expenditures.
#H1: There is the difference in mean of expenditures of the Per-Pupil Expenditures.
ifelse(p3.value>alpha8, "Fail to reject null Hypothesis", "Reject Null Hypothesis")
#==================================Q12-3.10=====================================
#Significance Level
alpha9 <- 0.05

#Creating dataframe of observed values of Plant food A in different Growth-light categories
growthLight1A <- data.frame('plantA' = c(9.2, 9.4, 8.9), supplements = rep('growthLight1A', 3), stringsAsFactors = FALSE)
growthLight2A <- data.frame('plantA' = c(8.5, 9.2, 8.9), supplements = rep('growthLight2A', 3), stringsAsFactors = FALSE)

#Creating data frame to consolidate the values of data frame of above observed data of plant A
plantA <- rbind(growthLight1A, growthLight2A)
plantA$supplements <- as.factor(plantA$supplements)
plantA

#Summary of the Plant A growth
describe(plantA)[0:5]%>%
  round(2) %>% 
  knitr::kable(align = "r")%>%
  kable_paper()

#combined data of total growth of plant A
df.plantA <- melt(plantA, id.var = "supplements")

#Displaying box_plot for comparing the growth of plant A in different environment.
bx_plt8<- ggplot(df.plantA, aes(x = variable, y=value)) + 
  ggtitle("Box plot showing growth of plant A in different environments") +
  xlab('Types of Environment') + ylab('Total Growth') +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "lightblue",colour = "lightblue", size = 0.5, linetype = "solid")) +
  geom_boxplot(aes(fill=supplements))
show(bx_plt8)

#Performing ANOVA test
anov_A <- aov(plantA ~ supplements, data = plantA)

#plotting graph based on ANOVA test
plot(anov_A)

#Summary of ANOVA test
summary(anov_A)
a_plantA.summary <- summary(anov_A)

#Showing Numerator
plantA.numerator <- a_plantA.summary[[1]][1, "Df"]
plantA.numerator

#Showing Denominator
plantA.denominator <- a_plantA.summary[[1]][2, "Df"]
plantA.denominator

#Showing F-value
F.value <- a_plantA.summary[[1]][[1, "F value"]]
F.value

#Showing p-value
p.value <- a_plantA.summary[[1]][[1, "Pr(>F)"]]
p.value

#Creating flex table
flextable(data.frame(plantA.numerator,plantA.denominator,F.value,p.value))

#H0: There is no difference in mean of plant A growth in light 1 and light 2.
#H1: There is the difference in mean of plant A growth in light 1 and light 2.
ifelse(p_plantA.value>alpha9, "Fail to reject null Hypothesis", "Reject Null Hypothesis")

#Part 2

#Creating dataframe of observed values of Plant food B in different Growth-light categories
growthLight1B <- data.frame('plantB' = c(7.1, 7.2, 8.5), supplements = rep('growthLight1B', 3), stringsAsFactors = FALSE)
growthLight2B <- data.frame('plantB' = c(5.5, 5.8, 7.6), supplements = rep('growthLight2B', 3), stringsAsFactors = FALSE)

#Creating data frame to consolidate the values of data frame of above observed data of plant B
plantB <- rbind(growthLight1B, growthLight2B)
plantB$supplements <- as.factor(plantB$supplements)
plantB

#Summary of the Plant B growth
describe(plantB)[0:5]%>%
  round(2) %>% 
  knitr::kable(align = "r")%>%
  kable_paper()

#combined data of total growth of plant B
df.plantB <- melt(plantB, id.var = "supplements")

#Displaying box_plot for comparing the growth of plant B in different environment.
bx_plt9<- ggplot(df.plantB, aes(x = variable, y=value)) + 
  ggtitle("Box plot showing growth of plant B in different environments") +
  xlab('Types of Environment') + ylab('Total Growth') +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "lightblue",colour = "lightblue", size = 0.5, linetype = "solid")) +
  geom_boxplot(aes(fill=supplements))
show(bx_plt9)

#Performing ANOVA test
anov_B <- aov(plantB ~ supplements, data = plantB)

#plotting graph based on ANOVA test
plot(anov_B)

#Summary of ANOVA test
summary(anov_B)
a_plantB.summary <- summary(anov_B)

#Showing Numerator
plantB.numerator <- a_plantB.summary[[1]][1, "Df"]
plantB.numerator

#Showing Denominator
plantB.denominator <- a_plantB.summary[[1]][2, "Df"]
plantB.denominator

#Showing F-value
F_plantB.value <- a_plantB.summary[[1]][[1, "F value"]]
F_plantB.value

#Showing p-value
p.value <- a_plantB.summary[[1]][[1, "Pr(>F)"]]
p.value

#Creating flex table
flextable(data.frame(plantB.numerator,plantB.denominator,F_plantB.value,p.value))

#H0: There is no difference in mean of plant B growth in light 1 and light 2.
#H1: There is the difference in mean of plant B growth in light 1 and light 2.
ifelse(p_plantB.value>alpha9, "Fail to reject null Hypothesis", "Reject Null Hypothesis")
#=================================Baseball Data set=============================
#Significance Level
alpha10 <- 0.05

#Changing the Directory location
setwd("C:/Users/XYZ")

#Uploading Baseball dataset
bb_dataset<- read.csv("baseball.csv",header = TRUE)

#Creating decade column in the baseball data frame
bb_dataset <- bb_dataset %>%
  mutate(decade = paste0(Year  %/% 10 * 10))
                        

totalWins <- aggregate(bb_dataset$W, by=list(Category=bb_dataset$decade), FUN=sum)
totalWins

df <- data.frame(totalWins)
colnames(df) <- c('Decade', 'Wins')
df
#Creating plot taking decade on x-axis and wins on y-axis
ggplot(data=df, aes(x=paste0(Decade,'s'), y=Wins, group=1)) +
  geom_line(colour="red",linetype="dashed", size=2) +
  geom_point(fill="white", shape=21, size=3) +
  expand_limits(y=0) +
  xlab("Decade") + ylab("Wins total") +
  ggtitle("Decade v/s Wins") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(fill = "lightgreen",colour = "lightgreen", size = 0.5))

#Chi-Square Goodness of Fit Test
chi_test <- chisq.test(df$Wins, correct=FALSE)

testValue <- chi_test$statistic #Chi-square test value
pValue<- chi_test$p.value  #Chi-square p-value
doF<- chi_test$parameter #degrees of freedom


#Creating flex table
flextable(data.frame(testValue,pValue,doF))

#H0: There is no difference in mean of winning baseball matches of each decade.
#H1: There is the difference in mean of winning baseball matches of each decade.
ifelse(chi_test$p.value>alpha10, "Fail to reject null Hypothesis", "Reject Null Hypothesis")

#=================================Crop Data set=================================
cp_dataset <- read.csv("crop_data.csv",header = TRUE)

cp_dataset$density <- as.factor(cp_dataset$density)
cp_dataset$block <- as.factor(cp_dataset$block)
cp_dataset$fertilizer <- as.factor(cp_dataset$fertilizer)
cp_dataset

#Creating plot taking decade on x-axis and wins on y-axis
a <- aggregate(yield ~ fertilizer+density , cp_dataset, function(i) round(mean(i)))

bar2 <- ggplot(a, aes(factor(fertilizer), y=yield, fill=factor(density))) + 
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Yield v/s Fertilizer & Density") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_rect(colour = "black", size = 0.5, linetype = 'solid'))+
  xlab('Fertilizer') + ylab('Yield')
show(bar2)

#Anova Test
cp_anova <- aov(yield~fertilizer+density, data=cp_dataset)

#Plotting graph based on ANOVA test
plot(cp_anova)

#Summary of ANOVA test
summary(cp_anova)
a_cp.summary <- summary(cp_anova)

#Showing Numerator
df_a_cp.numerator <- c(a_cp.summary[[1]][1, "Df"],a_cp.summary[[1]][2, "Df"])
df_a_cp.numerator

#Showing Denominator
df_a_cp.denominator <- c(a_cp.summary[[1]][3, "Df"],a_cp.summary[[1]][3, "Df"])
df_a_cp.denominator

#Showing F-value
F.value <- c(a_cp.summary[[1]][[1, "F value"]],a_cp.summary[[1]][[2, "F value"]])
F.value

#Showing p-value
p.value <- c(a_cp.summary[[1]][[1, "Pr(>F)"]],a_cp.summary[[1]][[2, "Pr(>F)"]])
p.value

#Creating flex table
flextable(data.frame(df_a_cp.numerator,df_a_cp.denominator,F.value,p.value))

p_a_cp.value <- a_cp.summary[[1]][[1, "Pr(>F)"]]
p.value
#H0: There is no difference in mean of yield based on fertilizers.
#H1: There is the difference in mean of yield based on fertilizers.
ifelse(p_a_cp.value>alpha10, "Fail to reject null Hypothesis", "Reject Null Hypothesis")

#H0: There is no difference in mean of yield based on density of plants.
#H1: There is the difference in mean of yield based on density of plants.
ifelse(p_a_cp.value.value>alpha10, "Fail to reject null Hypothesis", "Reject Null Hypothesis")
#========================XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX=====================
#Empty the variables
rm(list = ls())
#Removing plots
dev.off()
