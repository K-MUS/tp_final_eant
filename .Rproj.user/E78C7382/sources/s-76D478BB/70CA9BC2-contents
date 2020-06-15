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
model <- rpart(Attrition ~ ., data=train)
model <- rpart(Attrition ~ ., data=train,control=rpart.control(minsplit=10,cp=0.001))
model
plot(model, uniform=TRUE, branch=0.6, margin=0.05)
text(model, all=TRUE, use.n=TRUE)

rpart.plot(model, extra=0, type=2)

conf_mat = predict(model, newdata = test, type = "class") 
conf_mat_info <- confusionMatrix(conf_mat,test$Attrition)
conf_mat_info

#model randomForest
model <- randomForest::randomForest(Attrition ~ .,data = train)
model <- randomForest::randomForest(Attrition ~ .,data = train,ntree=100, mtry=2)
model
randomForest::varImpPlot(model)
model

#party
model <- party::ctree(Attrition ~ .,data = train)
plot(model, type="simple")
model

conf_mat = predict(model, newdata = test, type = "response") 
conf_mat_info <- confusionMatrix(conf_mat,test$Attrition)
conf_mat_info

#ROC
predict = predict(model, newdata = test, type = "prob")[,1]
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
  # theme(legend.position="none", strip.background = element_blank(), strip.text.x = element_blank(),
  #         plot.title=element_text(hjust=0.5, color="white"), plot.subtitle=element_text(color="white"), plot.background=element_rect(fill="#0D7680"),
  #         axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
  #         axis.title=element_text(colour="white"),
  #         legend.background = element_rect(fill="#FFF9F5",
  #                                          size=0.5, linetype="solid",
  #                                          colour ="black"))
  

shinyApp(
  ui = UI,
  server = SERVER
)
