#Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(forcats) #sort ggplot by amount
library(cowplot) #Themes and tools for ggplot
library(ggthemes)#themes for ggplot
library(rpart)   #Decision Tree
library(rpart.plot)   #Decision Tree - Plots
library(caret)   #Split data
library(corrplot)
library(DT)      #Display Table

source('ui.R', local = TRUE)
source('server.R')

#Data
original_df <- read.csv("Data/WA_Fn-UseC_-HR-Employee-Attrition.csv")
df <- original_df
colnames(df)[1] = "Age"
df$Attrition= as.integer(as.factor(df$Attrition))-1
df$BusinessTravel= as.integer(as.factor(df$BusinessTravel))
df$Department= as.integer(as.factor(df$Department))
df$Gender= as.integer(as.factor(df$Gender))
df$JobRole= as.integer(as.factor(df$JobRole))
df$MaritalStatus= as.integer(as.factor(df$MaritalStatus))
df$OverTime= as.integer(as.factor(df$OverTime))
df$EducationField= as.integer(as.factor(df$EducationField))
df$StandardHours<-NULL
df$PerformanceRating<-NULL
df$Over18<-NULL
df$EmployeeCount<-NULL
df$JobLevel<-NULL
df$DailyRate<-NULL
df$HourlyRate<-NULL
df$DailyRate<-NULL
df$MonthlyRate<-NULL
df$PercentSalaryHike<-NULL

set.seed(666)

shinyApp(
  ui = UI,
  server = SERVER
)