#File: Module 3 Project 3
#Project: Introduction to Analytics
#Author : Fnu Abhishek

#
#Clean Canvas----
#Empty the variables
rm(list = ls())
#Removing plots
dev.off()

#Printing the name of the author
print("Fnu Abhishek")

install.packages('car')
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
#To see the list of packages loaded
(.packages())

#Import 'inchBio.csv' dataset:

setwd("C:/Users/Abhishek Mishra/Documents/ALY6000/Week 6")

dataset<- read.csv("Netflix_Movies_DataSet.csv", header=TRUE)
print(tail(dataset))

#Removing records which have zero ratings
dataset <- dataset[!(is.na(dataset$rating) | dataset$rating==""),]
print(unique(dataset$rating))

#===================Replacing comma/space from votes column values===================
dataset$votes <- str_replace_all(dataset$votes,',','')

dataset$votes <- str_replace_all(dataset$votes,' ','')

dataset$votes <- as.integer(dataset$votes)
#=============================End of Action===============================================

#Removing the 'TV'shows from the dataset
dataset <- dataset[!grepl("TV", dataset$certificate),]

#=============================Removing invalid characters from year column=============================
dataset$year<- gsub('[a-zA-Z()+]*','',dataset$year)
dataset$year<- gsub('[-]*','',dataset$year)
dataset$year<- gsub(' ','',dataset$year)
dataset$year<-substr(dataset$year,1,4)
dataset$year <- as.integer(dataset$year)
#=============================End of Action===============================================

#========================Removing invalid characters from stars column=====================================
dataset$stars<- gsub("'",'',dataset$stars)
dataset$stars<- gsub("    Stars:",'',dataset$stars)
dataset$stars<- gsub("Star:",'',dataset$stars)
dataset$stars<- gsub("  ",'',dataset$stars)
dataset$stars<- gsub(",  ,",',',dataset$stars)
dataset$stars<- gsub('"','',dataset$stars)
dataset$stars<- gsub(',','-',dataset$stars)
dataset$stars<- gsub('- -','-',dataset$stars)
dataset$stars<- gsub('\\[','',dataset$stars)
dataset$stars<- gsub('\\]','',dataset$stars)
#=============================End of Action================================================

#===========Replacing ',' character with '-' character in Genre column=====================
dataset$genre<- gsub(',','#',dataset$genre)
dataset$genre<- gsub('# ','#',dataset$genre)

#===========Dropping duration and description column from the dataset======================
dataset<- dataset[ -c(4,7) ]

#===========Fetch sub strings from genre column======================
list_genre <- unlist(strsplit(dataset$genre, "#"))
print(length(list_genre))

genre = hash()

for (x in list_genre)
  {
    if (is.null(genre[[x]]))
    {
      a <- 1
      genre[[x]]<-a
    }
    else
    {
      v <- genre[[x]] + 1
      genre[[x]]<-v
    }
}

rm( list= c("Biography","Adventure","Musical", "Family","Fantasy","Film-Noir","Game-Show","History","Mystery","News","Reality-TV","Talk-Show","War","Western"), envir=genre)

print(keys(genre))

percent_value<-round((100*values(genre)/sum(values(genre))),1)

cols = c("#FF0000","#FFD700","#008000","#F08080","#0000CD","#F0E68C","#4B0082","#FF69B4","#800000","#48D1CC","#FF4500","#EE82EE","#9ACD32")

pie(values(genre),labels=paste0(percent_value, "%"), 
    main="Pie Chart of Genre",cex = 0.8,
    col=cols)
legend("topright", keys(genre), cex = 0.7,
              fill = cols)

#==================Fetch sub strings from stars column======================
#============Retrieving the list of actors from stars column using '-' as separator=======
list_stars <- unlist(strsplit(dataset$stars, "- "))
print(list_stars)
dataset$stars
stars = hash()
#=============Counting the number of movies done by each actor using the following loop=========
for (x in list_stars)
{
  if (is.null(stars[[x]]))
  {
    a <- 1
    stars[[x]]<-a
  }
  else
  {
    v <- stars[[x]] + 1
    stars[[x]]<-v
  }
}

#====================Sorting the values of the hash data set===================
values_list = unlist(sort(values(stars), decreasing = TRUE))

#===============Need to plot first five values from 'values_list'========================
top_stars= list();
final_Top_Actors_map = hash()

#==================Creating a 'final_Top_Actors_map' hash map to store the counts and mean of ratings of each movie done by an actor==========
for (y in values_list[1:5])
{
  for (x in keys(stars))
  {
    if (stars[[x]]==y && !(x %in% top_stars))
    {
      top_stars <- append(top_stars,x)
      first_star_rating <- dataset[dataset$stars %like% x, ]
      final_Top_Actors_map[[x]]<-c(y+mean(first_star_rating$rating))
    }
  }
}
#=============Sorting the values of final_Top_Actors_map to retrieve the list of actors who have done most number of movies and have higher ratings=======
values_list = unlist(sort(values(final_Top_Actors_map), decreasing = TRUE))

top_actors = hash()

#=============Finally retrieving the list of actors who have done most number of movies based on their ratings which includes the data set with actual count of movies=======

for (y in values_list[1:5])
{
 
  for (x in keys(final_Top_Actors_map))
  {
    if(final_Top_Actors_map[[x]]==y)
    {
      top_actors[[x]]<-stars[[x]]
      
    }
  }
}
top_actors_list = unlist(sort(values(top_actors), decreasing = TRUE))

#==========Plotting the bar chart to represent the Top 5 Artists which have acted in max number of movies=====================
barplot(top_actors_list[1:5], col=c('red','green','yellow','blue','pink'), main ='Top 5 Artists',
        horiz=FALSE, ylim = c(0,30),xlab= 'Actors', ylab = 'Number of movies',cex.names=0.9)
#===================Deleting unused variables=====================

rm(x,y,v,a,first_star_rating,top_stars)

#============Fetching the list of years from the data set=====================
year<-sort(unique(dataset$year))
movie_rating= list()
movie_title =list()

for (i in year)
{
  y_ds <- dataset[dataset$year ==i,]
  movie_rating<- append(movie_rating,max(y_ds$rating))
  y_ds<- y_ds[y_ds$rating == max(y_ds$rating),]
  y_ds<- y_ds[y_ds$votes == max(y_ds$votes),]
  movie_title<-append(movie_title,max(y_ds$title))
}
year<-list
rm(final_df)

final_df = data.frame(unlist(year),unlist(movie_rating),unlist(movie_title))

colnames(final_df) <- c("year","rating","title")

print(final_df$rating)



plot(final_df$year,final_df$rating, main="Scatterplot of Best Movie in each year",
     xlab="Year ", ylab="Ratings", pch=19,ylim=c(0,10),col = rainbow(length(final_df$year)))

pc <- ggplot(data = final_df) +
  geom_bar(aes(x=reorder(title, year), y =rating),stat="identity",color="black",fill=  rainbow(length(final_df$year)),width = 0.90, position = position_dodge(0.15))+
  ylim(c(0,10))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.001, hjust=0.1),plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = "white"),panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  ggtitle("Movies having highest rating in each year since 1932") +
  xlab("List of Movies") + ylab("Movie Ratings") +
  geom_label(aes(y = rating, label = paste0(final_df$year)))


  geom_line(aes(x=reorder(Species,-counts), y=cumcounts),group = 1,stat='identity', color='cyan4') +
  geom_point(aes(x=reorder(Species,-counts), y=cumcounts),size=2, color="cyan4") +
  scale_y_continuous(labels =c(0,d$cumcounts),breaks=c(0,d$cumcounts),sec.axis = dup_axis(~ .*(d$cumfreq*100/d$cumcounts),breaks=c(0,round(d$cumfreq*100)),labels = c(0,round(d$cumfreq*100)),name=''))
show(pc)



