#------------------------------------------------------------
#Tabs
#------------------------------------------------------------
source('tabs.R')
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Intro", tabName = "intro"),
        menuItem("EDA", tabName = "eda"),
        menuItem("Datos", tabName = "datos")
    )
)

#------------------------------------------------------------
#Body
#------------------------------------------------------------
body <- dashboardBody(
    tabItems(
        tab_intro,
        tab_eda,
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
