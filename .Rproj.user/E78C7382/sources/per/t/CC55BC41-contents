#------------------------------------------------------------
#Tabs
#------------------------------------------------------------
source('tabs.R')
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Intro", tabName = "intro",icon=icon("file-alt")),
        menuItem("EDA", tabName = "eda",icon = icon("chart-bar")),
        menuItem("Model", tabName = "model",icon = icon("th-large")),
          menuSubItem("Decision Tree", tabName = "model_dt",icon = icon("tree-deciduous", lib = "glyphicon")),
          menuSubItem("Random Forest", tabName = "model_rf",icon = icon("tree")),
        menuItem("Predict", tabName = "predict",icon = icon('dashboard')),
        menuItem("DataSet", tabName = "datos",icon=icon("database"))
    )
)

#------------------------------------------------------------
#Body
#------------------------------------------------------------
body <- dashboardBody(
      tags$style(HTML("
    .box.box-solid.box-primary>.box-header {
      color:#fff;
      background:#95C8D8
                        }
    
    .box.box-solid.box-primary{
    border-bottom-color:#95C8D8;
    border-left-color:#95C8D8;
    border-right-color:#95C8D8;
    border-top-color:#95C8D8;
    }
    
    ")),  
    tabItems(
        tab_intro,
        tab_eda,
        tab_model,
        tab_predict,
        tab_datos
    )
)

#------------------------------------------------------------
# Shiny - UI
#------------------------------------------------------------
UI <- dashboardPage(skin = c("blue"),  
        dashboardHeader(title = "Attrition Analysis"),
        sidebar,
        body
      )
