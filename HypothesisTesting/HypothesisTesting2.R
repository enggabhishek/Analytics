#################Load_library###############
library(tidyverse)
library(ggplot2)
library(MASS)
#Set the path of the directory
setwd("C:/Users/XYZ")

#Uploading the diabetes.csv dataset
#====================================Part 1==========================================

#box plot to show cats body weight based on their sex
ggplot(cats, aes(x = Sex, y = Bwt,color = Sex, shape= Sex))+
  geom_boxplot(outlier.shape = NA, fill = c('lightblue', 'lightgreen')) +
  geom_jitter(width = 0.2 ) +
  ggtitle("Boxplot representing the Body weight of the cats based on the genders") +
  xlab('Sex') + ylab('Body Weight')

#H0 difference between the mean of before and after sleeping quality scores are equal
#H1 difference between the mean of before and after sleeping quality scores are not equal
t.test(cats$Bwt~cats$Sex, mu=0, alt = 'two.sided', conf = 0.95, var.eq = F, paired = F)


#=====================================End of Part 1=============================================


#======================================Part 2=================================================
before_sleeping_quality <- c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
after_sleeping_quality <- c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)
zeros <- list(rep('N',10))
ones <- list(rep('Y',10))

#Creating a data frame to include the sleeping quality scores of the students before taking the meditation
df <- data.frame(unlist(before_sleeping_quality),unlist(zeros))
colnames(df)<- c('Scores','Type')

#Creating a data frame to include the sleeping quality scores of the students after taking the meditation
df1 <- data.frame(unlist(after_sleeping_quality),unlist(ones))
colnames(df1)<- c('Scores','Type')

#Removing unnecessary variables 
rm(before_sleeping_quality,after_sleeping_quality,zeros,ones)

#Concatenate the two data frames in to 'final_df' data frame
final_df <- rbind(df,df1)

#box plot to show sleeping scores of the students before and after taking meditation
ggplot(final_df, aes(x = Type, y = Scores, color = Type, shape= Type)) +
  geom_boxplot(outlier.shape = NA, fill = c('yellow','pink')) +
  geom_jitter(width = 0.2 ) +
  ggtitle("Boxplot representing the sleeping quality scores of the students before and after meditation") +
  xlab('Meditation Status') + ylab('Sleeping Scores')

#Showing the variance of sleeping scores of students before and after meditation

print(var(final_df$Scores[final_df$Type==0]))
print(var(final_df$Scores[final_df$Type==1]))

#H0 difference between the mean of before and after sleeping quality scores are equal
#H1 difference between the mean of before and after sleeping quality scores are not equal
t.test(final_df$Scores~final_df$Type, mu=0, alt = 'two.sided', conf = 0.95, var.eq = F, paired = F)

#=========================================End of Part2====================================================

#Empty the variables
rm(list = ls())
#Removing plots
dev.off()
