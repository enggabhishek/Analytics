library(ggplot2)
library(magrittr)
library(xtable)
library(stringr)
library(corrplot)
library(ggcorrplot)
library(glmnet)
library(Metrics)
library(dplyr)
library(flextable)
library(officer)
library(ggrepel)
#Setting Up the directory
setwd("C:/Users/XYZ")

#creating data frames from the csv files
dataset <- read.csv("Boston Assessment.csv", header=TRUE)


#Removing null and invalid values from YR_BUILT column and adding the valid data into s_class data set
strc_class <- dataset[!(is.na(dataset$STRUCTURE_CLASS) | dataset$STRUCTURE_CLASS==""|dataset$STRUCTURE_CLASS=="0"|dataset$STRUCTURE_CLASS=="1"|dataset$STRUCTURE_CLASS=="4"),]
s_class <- dataset
s_class <- s_class[!(is.na(s_class$YR_BUILT)| s_class$YR_BUILT=="" | s_class$YR_BUILT == 0),]
s_class <- s_class[s_class$YR_BUILT>=1920 & s_class$YR_BUILT <= 2015,]


kit_type <- dataset[!(is.na(dataset$U_KIT_TYPE)| dataset$U_KIT_TYPE=="" | dataset$U_KIT_TYPE == '0'),]

s_class <- s_class[!(is.na(s_class$YR_BUILT)| s_class$YR_BUILT=="" | s_class$YR_BUILT == 0),]
#Creating a decade column
s_class <- s_class %>%
  mutate(decade = paste0(YR_BUILT  %/% 10 * 10))

#Removing null and invalid values from Building Cost, Land Cost and Gross Tax column
s_class <- s_class[!(is.na(s_class$AV_BLDG)| s_class$AV_BLDG==""),]
s_class <- s_class[!(is.na(s_class$AV_LAND)| s_class$AV_LAND==""),]
s_class <- s_class[!(is.na(s_class$GROSS_TAX)| s_class$GROSS_TAX==""),]

lst_decade<-sort(unique(s_class$decade))

building_cost = list()
land_cost = list()
gross_tax = list()

for (i in lst_decade)
{
  df <- s_class[s_class$decade == i,]
  building_cost<- append(building_cost,mean(df$AV_BLDG))
  land_cost<- append(land_cost,mean(df$AV_LAND))
  gross_tax<- append(gross_tax,mean(df$GROSS_TAX))
}

final_df <- data.frame(unlist(lst_decade),unlist(building_cost),unlist(land_cost),unlist(gross_tax))
colnames(final_df) <- c("Decade","BuildingCost","LandCost", "GrossTax")
rm(building_cost,land_cost,gross_tax)

#=========================================Summary Chart===================================
strc_class <- dataset[!(is.na(dataset$STRUCTURE_CLASS) | dataset$STRUCTURE_CLASS==""|dataset$STRUCTURE_CLASS=="0"|dataset$STRUCTURE_CLASS=="1"|dataset$STRUCTURE_CLASS=="4"),]
s_class$STRUCTURE_CLASS
des <- strc_class %>%
  dplyr::select(AV_LAND,AV_BLDG, GROSS_TAX,LAND_SF,GROSS_AREA,LIVING_AREA,STRUCTURE_CLASS)%>%
  group_by(STRUCTURE_CLASS)
  
des <- des[!(is.na(des$AV_BLDG)| des$AV_BLDG==""),]
des <- des[(des$AV_LAND!=0 & des$AV_BLDG != 0),]
des <- des[!(is.na(des$LAND_SF)| des$LAND_SF==""),]
des <- des[!(is.na(des$GROSS_AREA)| des$GROSS_AREA==""),]
des <- des[!(is.na(des$LIVING_AREA)| des$LIVING_AREA==""),]

summary_table <- des %>% 
  summarize(
    mean_mpg = mean(AV_BLDG),
    median_mpg = median(AV_BLDG),
    sd_mpg = sd(AV_BLDG),
    min_mpg = min(AV_BLDG),
    max_mpg = max(AV_BLDG)
  )
summary_table
write.csv(summary(des), "summary.csv")

#======================================================Graphs=============================================================

k_type <-sort(unique(kit_type$U_KIT_TYPE))
average_vals <- aggregate(AV_BLDG ~ U_KIT_TYPE, data = kit_type, mean)

#Creating a bar chart to show the average building cost of different kitchen style
ggplot(kit_type, aes(x = U_KIT_TYPE, y = AV_BLDG)) +
  stat_summary(fun.y = mean, geom = "bar", fill = rainbow(length(unique(kit_type$U_KIT_TYPE))), aes(label = round(..y.., 1))) +
  xlab("Kitchen Type") + ylab("Building Cost ($)") + 
       ggtitle("A visualization using bars to depict the building costs of various kitchen style.") +
theme(axis.text.x = element_text(vjust = 0.001, hjust=0.1, size = 20, face = 'bold'),
      axis.text.y = element_text(vjust = 0.001, hjust=0.1, size = 20, face = 'bold'),
      axis.title.x = element_text(face = "bold", size = 20),
      axis.title.y = element_text(face = "bold", size = 20),
      panel.background = element_rect(size = 0.5),
      plot.title = element_text(face = "bold",size = 20)) +
  geom_text(data = average_vals, aes(label = paste0("", round(AV_BLDG, 1)), fontface = 'bold'), vjust = -0.95, size = 6)

#Average Cost of buildings in every decade
ggplot(final_df, aes(x = Decade, y = BuildingCost)) +
  geom_bar(fill= rainbow(length(unique(final_df$Decade))), stat = 'identity') + xlab("Decade") + ylab("Building Cost ($)") +
  ggtitle("A visualization using bars to depict the building expenses across several decades") +
  theme(axis.text.x = element_text(vjust = 0.001,hjust=0.1, size = 20, face = 'bold'),
        axis.text.y = element_text(vjust = 0.001, hjust=0.1, size = 20, face = 'bold'),
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        panel.background = element_rect(size = 0.5),
        plot.title = element_text(face = "bold",size = 20)) +
  geom_text(aes(label = round(BuildingCost,2),fontface = 'bold'), vjust = -0.95,position = position_dodge(width = 4), size = 6)


#Average Land Cost of buildings for sale in every decade
ggplot(final_df, aes(x = Decade, y = LandCost)) +
  geom_bar(fill= rainbow(length(unique(final_df$Decade))), stat = 'identity') + xlab("Decade") + ylab("Land Cost ($)") +
  ggtitle("A visualization using bars to depict the land cost across several decades") +
  theme(text = element_text(size = 20, face = 'bold')) +
  geom_text(aes(label = round(LandCost,2),fontface = 'bold'), vjust = -0.95,position = position_dodge(width = 4), size = 6)

#Average Tax applied on buildings for sale in every decade
ggplot(final_df, aes(x = Decade, y = GrossTax)) +
  geom_bar(fill= 'blue',stat ='identity') + xlab("Decade") + ylab("Gross Tax ($)") +
  ggtitle("A bar chart displaying the gross tax across several decades") +
  theme(axis.text.x = element_text(vjust = 0.001, hjust=0.1, size = 20, face = 'bold'),
        axis.text.y = element_text(vjust = 0.001, hjust=0.1, size = 20, face = 'bold'),
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        panel.background = element_rect(size = 0.5),
        plot.title = element_text(face = "bold",size = 20)) +
  geom_text(aes(label = round(GrossTax,2),fontface = 'bold'), vjust = -0.95,position = position_dodge(width = 4), size = 6)

#Total Building available for sale in every decade
building_count <- count(s_class, decade)
ggplot(building_count, aes(x = decade,n), y = n) +
  geom_col() +
  geom_bar(color = 'purple', fill = 'purple', stat = 'identity') +  xlab("Decade") + ylab('Total Count') +
  ggtitle("Total Housing available for rent in every decade") +
  theme(axis.text.x = element_text(vjust = 0.001, hjust=0.1, size = 20, face = 'bold'),
        axis.text.y = element_text(vjust = 0.001, hjust=0.1, size = 20, face = 'bold'),
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        panel.background = element_rect(size = 0.5),
        plot.title = element_text(face = "bold",size = 20)) +
  geom_text(aes(label = round(n,2),fontface = 'bold'), vjust = -0.95,position = position_dodge(width = 4), size = 6)

#======================================================Graphs=============================================================
#Removing invalid values of Structure_Class column from dataset data frame and saving the valid data in to k_df data frame
k_df <- dataset[!(is.na(dataset$STRUCTURE_CLASS)| dataset$STRUCTURE_CLASS == "" |
                    dataset$STRUCTURE_CLASS == "Q"| dataset$STRUCTURE_CLASS == "1"|
                    dataset$STRUCTURE_CLASS == "4"| dataset$STRUCTURE_CLASS == "0"|
                    dataset$STRUCTURE_CLASS == "G"),]

#Creating dummy variables based on different building STRUCTURE_CLASS in k_df dataset
k_df$STRUCTURE_CLASS_R <- ifelse(k_df$STRUCTURE_CLASS == "R", 1, 0)
k_df$STRUCTURE_CLASS_E <- ifelse(k_df$STRUCTURE_CLASS == "E", 1, 0)
k_df$STRUCTURE_CLASS_D <- ifelse(k_df$STRUCTURE_CLASS == "D", 1, 0)
k_df$STRUCTURE_CLASS_C <- ifelse(k_df$STRUCTURE_CLASS == "C", 1, 0)
k_df$STRUCTURE_CLASS_B <- ifelse(k_df$STRUCTURE_CLASS == "B", 1, 0)
k_df$STRUCTURE_CLASS_A <- ifelse(k_df$STRUCTURE_CLASS == "A", 1, 0)

#Creating data frame containing different Structure Class and its respective counts
pie_df2 <- data.frame("StructureClass"= c("R","D","C","E","B","A"), 
                      "Count" = c(sum(k_df$STRUCTURE_CLASS_R),sum(k_df$STRUCTURE_CLASS_D),
                                  sum(k_df$STRUCTURE_CLASS_C),sum(k_df$STRUCTURE_CLASS_E),
                                  sum(k_df$STRUCTURE_CLASS_B),sum(k_df$STRUCTURE_CLASS_A)))

#===============================Creating Pie chart based on Structure Class and its total count===========================
ggplot(pie_df2, aes(x = "", y = Count, fill = StructureClass))+
  geom_bar(stat = "identity", width = 3) +
  theme_void()+
  scale_fill_brewer(palette = "Set3") +
  coord_polar("y",start = 0) +
  geom_label_repel(aes(label = paste0(round(100*Count/sum(Count),2), "%")),
                   fontface = "bold", size = 6, show.legend = FALSE)
#=========================================================================================================================
new <- dataset %>%
  dplyr::select(AV_LAND,AV_BLDG, GROSS_TAX,LAND_SF,GROSS_AREA,LIVING_AREA)
corr <- cor(new, use = 'pairwise')
corr
#Exported Correlation table in 'corr.csv' file
write.csv(corr, file='corr.csv')
#==========================================Plot_Correlation_Matrix=======================================================
ggcorrplot(corr, method = 'circle', type = 'upper',ggtheme = ggplot2::theme_minimal,
           colors = c("lightgreen", "yellow", "red"),title = "Correlation Matrix") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5))
#========================================================================================================================
new[is.na(new)] <- 0
#=======================================Splitting the College Data Set in Train Test data================================

#==================Scatter Plot Land Cost vs Building Cost======================
ggplot(dataset, aes(AV_LAND, AV_BLDG)) + 
  geom_point(color = "green", size = 6) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 2) +
  ggtitle("Scatter Plot comparing land cost variations against building cost") +
  xlab("Land Cost ($)") + ylab("Building Cost ($)") +
  theme(text = element_text(size = 20, face = 'bold'))+
  stat_regline_equation(label.x = 50000000, label.y = 1200000000)


m1 <- lm(AV_BLDG ~ AV_LAND, data = dataset)
summary(m1)$r.squared
#Creating linear regression model variable and fetching the details
model1 <- lm(AV_LAND ~ AV_BLDG + LAND_SF, data = dataset)

#Data frame to get the output of linear model variable in data frame
df <- data.frame(model1$coefficients,summary(model1)$r.squared,AIC(model1))
colnames(df)<- c('Coefficients', 'R-Square','AIC')
write.csv(df, 'out.csv')

#=======================================Scatter Plot Land Cost vs Land Surface Area======================================
ggplot(dataset, aes(AV_LAND, LAND_SF)) + 
  geom_point(color = "blue", size = 6) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 2) +
  ggtitle("Scatter Plot comparing land cost variations against land surface area") +
  xlab("Land Cost ($)") + ylab("Surface Area (ft^2)") + 
  theme(text = element_text(size = 20, face = 'bold'))+
  stat_regline_equation(label.x = 50000000, label.y = 100000000)

m2 <- lm(LAND_SF ~ AV_LAND, data = dataset)
summary(m2)$r.squared
#===================Scatter Plot Gross Tax vs Living Area=======================
ggplot(dataset, aes(GROSS_TAX, LIVING_AREA)) + 
  geom_point(color = "purple", size = 6) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 2) +
  ggtitle("Scatter comparing gross tax variations against living area") +
  xlab("Gross Tax ($)") + ylab("Living Area (ft^2)") +
  theme(text = element_text(size = 20, face = 'bold'))+
  stat_regline_equation(label.x = 500000, label.y = 2000000)

m3 <- lm(LIVING_AREA ~ GROSS_TAX, data = dataset)
summary(m3)$r.squared

#Creating linear regression model variable and fetching the details
model2 <- lm(LIVING_AREA ~ GROSS_TAX, data = dataset)

#Data frame to get the output of linear model variable in data frame
df2 <- data.frame(model2$coefficients,summary(model2)$r.squared,AIC(model2))
colnames(df2)<- c('Coefficients', 'R-Squared','AIC')
write.csv(df2, 'out2.csv')
#======================================================================================================================

#Empty the variables
rm(list = ls())

#Removing plots
dev.off()