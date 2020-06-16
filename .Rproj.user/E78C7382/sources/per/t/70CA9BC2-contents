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
library(plotly)  #Display Table
library(GGally)      
library(pROC)
library(randomForest)
library(party)

source('ui.R', local = TRUE)
source('server.R')

#Data
original_df <- read.csv("Data/WA_Fn-UseC_-HR-Employee-Attrition.csv")
df <- original_df
colnames(df)[1] = "Age"
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
df$EmployeeNumber<-NULL
df$MaritalStatus<-NULL
df$EducationField<-NULL
df$RelationshipSatisfaction<-NULL
df$WorkLifeBalance<-NULL
df$YearsSinceLastPromotion<-NULL
df$JobInvolvement<-NULL

#Convert df_num for the correlation matrix
df_num <- df
df_num$Attrition= as.integer(as.factor(df_num$Attrition))-1
df_num$BusinessTravel= as.integer(as.factor(df_num$BusinessTravel))
df_num$Department= as.integer(as.factor(df_num$Department))
df_num$Gender= as.integer(as.factor(df_num$Gender))
df_num$JobRole= as.integer(as.factor(df_num$JobRole))
df_num$OverTime= as.integer(as.factor(df_num$OverTime))

set.seed(666)

# Split data
trainIndex <- createDataPartition(df$Attrition, p=0.8, list=FALSE, times=1)
train <- df[trainIndex,]
test <- df[-trainIndex,]

#model Decision tree
model_dt <- rpart(Attrition ~ ., data=train)
model_dt <- rpart(Attrition ~ ., data=train,control=rpart.control(minsplit=10,cp=0.001))
model_dt
plot(model_dt, uniform=TRUE, branch=0.6, margin=0.05)
text(model_dt, all=TRUE, use.n=TRUE)

rpart.plot(model_dt, extra=0, type=2)

conf_mat = predict(model_dt, newdata = test, type = "class") 
conf_mat_info <- confusionMatrix(conf_mat,test$Attrition)
conf_mat_info

#model randomForest
model_rf <- randomForest::randomForest(Attrition ~ .,data = train)
model_rf <- randomForest::randomForest(Attrition ~ .,data = train,ntree=100, mtry=2)
model_rf
randomForest::varImpPlot(model_rf)
model_rf

#party
model_tp <- party::ctree(Attrition ~ .,data = train)
plot(model_tp, type="simple")
model_tp

conf_mat = predict(model_tp, newdata = test, type = "response") 
conf_mat_info <- confusionMatrix(conf_mat,test$Attrition)
conf_mat_info

#ROC
predict = predict(model_dt, newdata = test, type = "prob")[,1]
par(pty="s")
roc(response = test$Attrition, predictor = predict,plot=TRUE, legacy.axes = TRUE, percent = TRUE, xlab= "False Positive Perc.", ylab= "True Positive Perc.",
    col="#377EB8",lwd=4, print.auc=TRUE)


# Complicated DecisionTree, Is there a way to determine variable importance?
var_imp <- data.frame(model$variable.importance)
var_imp$features <- rownames(var_imp)
var_imp <- var_imp[, c(2, 1)]
var_imp$importance <- round(var_imp$model.variable.importance, 2)
var_imp$model.variable.importance <- NULL


colorCount <- length(unique(var_imp$features))
feature_importance <- var_imp 

  ggplot(feature_importance, aes(x=reorder(features, importance), y=importance, fill=features)) + 
    geom_bar(stat='identity') + 
    coord_flip() + 
    geom_label(aes(label=paste0(importance, "%")), colour = "white", fontface = "italic", hjust=0.6) + 
    theme_minimal() +
    theme(legend.position="none")

shinyApp(
  ui = UI,
  server = SERVER
)
