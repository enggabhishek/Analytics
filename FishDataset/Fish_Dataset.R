install.packages('jtools')
#################Load_library###############
library(tidyverse)
library(ggplot2)
library(jtools)
library(Hmisc)
library(dplyr)

#==================Set the path of the directory=========================
setwd("C:/Users/XYZ")

#Read the Fish.csv data set==================
dataset <- read.csv('Fish.csv', header = TRUE)
#===========Dropping duration and description column from the data set======================
dataset<- dataset[ -c(4,5) ]
colnames(dataset)<- c('Species', 'Weight','VerticalLength', 'Height','DiagonalWidth')

#=====================Bar chart for showing the highest weights of all the species of the fish===========================
weight <- list()
fish_species <- list()

for (i in unique(dataset$Species))
{
  fish_species<- append(fish_species,i)
  d <- dataset[dataset$Species == i,]
  weight <- append(weight,max(d$Weight))
}

#Creating fish_dataset as a dataframe to store fish speices and its respective counts
fish_dataset <- data.frame(unlist(fish_species),unlist(weight))
colnames(fish_dataset) <- c('Species', 'Weight')

#removing unnecessary variables
rm(weight,d,fish_species)

bc <- ggplot(data = fish_dataset)+
geom_bar(aes(x = reorder(Species,Weight),y = Weight), stat = 'identity' , color= 'black', fill = rainbow(length(fish_dataset$Species)),width = 0.90, position = position_dodge(0.15))+
theme(axis.text.x = element_text(angle = 90, vjust = 0.001, hjust=0.1),plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"),panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
ggtitle("Species of Fish vs Weight") +
xlab("List of Species") + ylab("Weight")+
geom_text(aes(x=Species, y=Weight, label = Weight,vjust = -0.95,angle = 0),size = 3)
show(bc)

#=====================Bar chart for showing the counts of all the species of the fish===========================

counts <- list()
fish_species <- list()
     
for (i in unique(dataset$Species))
{
  fish_species<- append(fish_species,i)
  d <- dataset[dataset$Species == i,]
  counts <- append(counts,length(d$Weight))
}
#Creating fish_dataset as a dataframe to store fish speices and its respective counts
fish_dataset <- data.frame(unlist(fish_species),unlist(counts))
colnames(fish_dataset) <- c('Species', 'Counts')

#removing unnecessary variables
rm(counts,d,fish_species)

bc2 <- ggplot(data = fish_dataset)+
  geom_bar(aes(x = reorder(Species,Counts),y = Counts), stat = 'identity' , color= 'black', fill = rainbow(length(fish_dataset$Species)),width = 0.90, position = position_dodge(0.15))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.001, hjust=0.1),plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"),panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  ggtitle("Species of Fish vs Counts") +
  xlab("List of Species") + ylab("Counts") +
  geom_text(aes(x=Species, y=Counts, label = Counts,vjust = -0.95,angle = 0),size = 3)
show(bc2)
#==================Show the correlation chart of the given fish dataset excluding 'Species' column==========

df<- dataset
res<- rcorr(as.matrix(df %>% dplyr::select(2:5)))
print(res)

#=========================Regression Table========================================

#====================================Part 1=======================================
#Regression graph to show the variation of Fish Weight and its Vertical Length
pt <- ggplot(data = dataset,aes(x = Weight, y= VerticalLength))+
  geom_point() + 
  geom_smooth(method = 'lm',se = FALSE)+ ggtitle("Scatter Chart showing the variation of fish's Weight vs its Vertical length") +
  xlab("Weight") + ylab("Vertical Length") + theme(panel.background = element_rect(fill = "lightgreen"))
show(pt)

#Details of Regression line to fit between data points containing Vertical Length and Weight of the fish
fit<- lm(VerticalLength ~ Weight, data = dataset)
summ(fit)

#====================================Part 2=======================================
#Regression graph to show the variation of Fish Weight and its Height
pt <- ggplot(data = dataset,aes(x = Weight, y= Height))+
  geom_point() + 
  geom_smooth(method = 'lm',se = FALSE, col = 'darkgreen')+ ggtitle("Scatter Chart showing the variation of fish's Weight vs its Height") +
  xlab("Weight") + ylab("Height") + theme(panel.background = element_rect(fill = "pink"))
show(pt)

#Details of Regression line to fit between data points containing Height and Weight of the fish
fit<- lm(Height ~ Weight, data = dataset)
summ(fit)

#====================================Part 3=======================================
#Regression graph to show the variation of Fish Weight and its Diagonal Width
pt <- ggplot(data = dataset,aes(x = Weight, y= DiagonalWidth))+
  geom_point() + 
  geom_smooth(method = 'lm',se = FALSE, col = 'red')+ ggtitle("Scatter Chart showing the variation of fish's Weight vs its DiagonalWidth") +
  xlab("Weight") + ylab("Diagonal Width") + theme(panel.background = element_rect(fill = "lightblue"))
show(pt)

#Details of Regression line to fit between data points containing Diagonal Width and Weight of the fish
fit<- lm(DiagonalWidth ~ Weight, data = dataset)
summ(fit)
#=================================End of Action====================================
#Clean Canvas----
#Empty the variables
rm(list = ls())
#Removing plots
dev.off()