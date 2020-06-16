#------------------------------------------------------------
# EDA - Correlation
#------------------------------------------------------------
box_eda_corr <- tabPanel("Correlation", 
                  fluidRow(                         
                         box(
                           title = "Correlation Matrix", width = 8, status = "primary", solidHeader = TRUE,
                           plotOutput("eda_corr_plot", click = "eda_corr_plot_click", height = "600px"),
                           
                         ),
                           box(
                             title = "Correlation Detail", width = 4, status = "primary", solidHeader = TRUE,
                             "Select a cell from the Correlation Matrix", br(), #, "More text",
                             #verbatimTextOutput("corr_info"),
                             conditionalPanel("output.corr_clicked",
                               plotOutput("eda_corr_click_info", height="320px")
                             )
                           )
                  )
)
#------------------------------------------------------------
# EDA - Attrition
#------------------------------------------------------------
box_eda_attrition <- tabPanel("Attrition", 
                         fluidRow(                         
                           box(
                             title = "Attrition", width = 4, status = "primary", solidHeader = TRUE,
                             plotOutput("eda_attr_attr", height = "320px"),
                             
                           ),
                           box(
                             title = "Attrition by Educational Level", width = 4, status = "primary", solidHeader = TRUE,
                             plotOutput("eda_attr_edu_level", height = "320px"),
                             
                           ),
                           box(
                             title = "Attrition by Job", width = 4, status = "primary", solidHeader = TRUE,
                             plotOutput("eda_attr_job", height = "320px"),
                             
                           )
                        ),
                        fluidRow(                         
                          box(
                            title = "Attrition by Age", width = 3, status = "primary", solidHeader = TRUE,
                            plotOutput("eda_attr_age", height = "320px"),
                            
                          ),
                          box(
                            title = "Attrition by Monthly Income", width = 3, status = "primary", solidHeader = TRUE,
                            plotOutput("eda_attr_income", height = "320px"),
                            
                          ),
                          box(
                            title = "Attrition by Distance from Home", width = 3, status = "primary", solidHeader = TRUE,
                            plotOutput("eda_attr_distance", height = "320px"),
                            
                          ),
                          box(
                            title = "Attrition by Years Current Manager", width = 3, status = "primary", solidHeader = TRUE,
                            plotOutput("eda_attr_manager", height = "320px"),
                            
                          )
                        )                        
)

#------------------------------------------------------------
# EDA - Monthly Income
#------------------------------------------------------------
box_eda_monthly <- tabPanel("Monthly Income", 
                              fluidRow(                         
                                box(
                                  title = "Monthy Income by Gender", width = 3, status = "primary", solidHeader = TRUE,
                                  plotOutput("eda_monthly_gender", height = "320px"),
                                  
                                ),
                                box(
                                  title = "Monthy Income (mean) by Job Satisfaction", width = 6, status = "primary", solidHeader = TRUE,
                                  plotOutput("eda_monthly_job", height = "320px"),

                                )
                                # box(
                                #   title = "Gender", width = 3, status = "primary", solidHeader = TRUE,
                                #   plotOutput("eda_attr_gender", height = "320px"),
                                #   
                                # )
                              )
                              # fluidRow(                         
                              #   box(
                              #     title = "Attrition by Age", width = 3, status = "primary", solidHeader = TRUE,
                              #     plotOutput("eda_attr_age", height = "320px"),
                              #     
                              #   ),
                              #   box(
                              #     title = "Attrition by Monthly Income", width = 3, status = "primary", solidHeader = TRUE,
                              #     plotOutput("eda_attr_income", height = "320px"),
                              #     
                              #   ),
                              #   box(
                              #     title = "Attrition by Distance from Home", width = 3, status = "primary", solidHeader = TRUE,
                              #     plotOutput("eda_attr_distance", height = "320px"),
                              #     
                              #   ),
                              #   box(
                              #     title = "Attrition by Years Current Manager", width = 3, status = "primary", solidHeader = TRUE,
                              #     plotOutput("eda_attr_manager", height = "320px"),
                              #     
                              #   )
                              # )                        
)
#------------------------------------------------------------
# Intro
#------------------------------------------------------------
tab_intro <- tabItem(tabName = "intro",
                     h2("Problem"),
                     p("The consulting world suffers from high rotation and this represent a problem to the companies"),
                     tags$ul(
                       tags$li("The market price usually is higher than the average of the company employees"), 
                       tags$li("The time that it's spent in training and onboarding of new employee"),
                       tags$li("When several people leaves an area the others starts to wonder and why a lot of people are leaving"),
                     ),
                     h2("Benefits of the project"),
                     p("Help the companies to detect possible attrition and resign from his employees and also to detect patters on which variables are determinating"),
                     h2("Data"),
                     p("Help the companies to detect possible attrition and resign from his employees and also to detect patters on which variables are determinating"),                     
)

#------------------------------------------------------------
# EDA
#------------------------------------------------------------
tab_eda <- tabItem(tabName = "eda",
                   tabBox(
                     title = "EDA - Exploratory Data Analysis",
                     # The id lets us use input$box_eda on the server to find the current tab
                     id = "box_eda", height = "100%", width = "100%",
                     box_eda_corr,
                     box_eda_attrition,
                     box_eda_monthly,
                     tabPanel("Tab2", DT::dataTableOutput("original_datos") )
                   )
)

#------------------------------------------------------------
# Model
#------------------------------------------------------------
tab_model <- tabItem(tabName = "model",
                       box(
                         title = "Parameters", width = 4, status = "primary", solidHeader = TRUE,
                         h2("parameters"),
                         verbatimTextOutput("conf_mat_info")
                       ),
                       box(
                         title = "Results", width = 8, status = "primary", solidHeader = TRUE,
                         h2("Result")
                       )
)
#------------------------------------------------------------
# Model - Decision Tree
#------------------------------------------------------------
tab_model_dt <- tabItem(tabName = "model_dt",
                      box(
                        title = "ROC", width = 4, status = "primary", solidHeader = TRUE,
                        plotOutput("roc_dt")#, height = "320px")
                      ),
                      box(
                       title = "Confusion Matrix & Stadistics", width = 4, status = "primary", solidHeader = TRUE,
                       verbatimTextOutput("conf_mat_info_dt")
                      ),
                      box(
                        title = "Importance Variables", width = 4, status = "primary", solidHeader = TRUE,
                        plotOutput("imp_var_dt")#, height = "320px")
                      )                      
)
#------------------------------------------------------------
# Model - random forest
#------------------------------------------------------------
tab_model_rf <- tabItem(tabName = "model_rf",
                      box(
                        title = "ROC", width = 4, status = "primary", solidHeader = TRUE,
                        plotOutput("roc_rf")#, height = "320px")
                      ),
                      box(
                        title = "Confusion Matrix & Stadistics", width = 4, status = "primary", solidHeader = TRUE,
                        verbatimTextOutput("conf_mat_info_rf")
                      ),
                      box(
                        title = "Importance Variables", width = 4, status = "primary", solidHeader = TRUE,
                        plotOutput("imp_var_rf")#, height = "320px")
                      )                      
)
#------------------------------------------------------------
# Predict
#------------------------------------------------------------
tab_predict <- tabItem(tabName = "predict",
                  fluidRow(
                         box( 
                           title = "Relevant Parameters", width = 3, status = "primary", solidHeader = TRUE,
                             #age
                             sliderInput("i_age", "Age?", min = 14, max = 100, value = 38),                         
                             #travel
                             selectInput("i_travel", "Travel Frequency", 
                                         choices = as.list(levels(train$BusinessTravel)),
                                         # choices = list("Non-Travel" = "Non-Travel", 
                                         #                "Rarely" = "Travel_Rarely", 
                                         #                "Frequently" = "Travel_Frequently"), 
                                         selected = 1),
                             #monthlyIncome
                             sliderInput("i_monthly_income", "Monthly Income", min = 1000, max = 25000, value = 15000),
                             #JobRole
                             selectInput("i_role", "Job Role", 
                                         choices = list("Sales Executive" = "Sales Executive", 
                                                        "Research Scientist" = "Research Scientist", 
                                                        "Laboratory Technician" = "Laboratory Technician",
                                                        "Manufacturing Director" = "Manufacturing Director",
                                                        "Healthcare Representative" = "Healthcare Representative",
                                                        "Manager" = "Manager",
                                                        "Sales Representative" = "Sales Representative",
                                                        "Research Director" = "Research Director",
                                                        "Human Resources" = "Human Resources"
                                         ), 
                                         selected = 3),     
                             #JobSatisfaction
                             selectInput("i_job_satisfaction", "Job Satisfaction", 
                                         choices = list("Low" = 1, 
                                                        "Medium"  = 2, 
                                                        "High" = 3,
                                                        "Very High"   = 4), 
                                         selected = 2),  
                             #Overtime
                             selectInput("i_overtime", "Overtime", 
                                         choices = list("No" = "No", 
                                                        "Yes"  = "Yes"), 
                                         selected = 2),                           
                             #StockOptionLvl
                             sliderInput("i_stock", "Stock Option level", min = 0, max = 3, value = 1),
                             #YearsCompany
                             sliderInput("i_years_company", "Years at Company", min = 0, max = 40, value = 6),                                                                         
                             #Years Current Role
                             sliderInput("i_years_role", "Years at Current Role", min = 0, max = 40, value = 2),                                                                         
                             #Years Same Manager
                             sliderInput("i_years_manager", "Years with Current Manager", min = 0, max = 40, value = 2),                                                                         
                           ),
                       column(width = 3,                       
                         box(
                           title = "Less Relevant Parameters", width = NULL, status = "primary", solidHeader = TRUE,
                           #department
                           selectInput("i_department", "Department", 
                                       choices = list("Human Resources" = "Human Resources", 
                                                      "Research & Development" = "Research & Development", 
                                                      "Sales" = "Sales"), 
                                       selected = 3),
                           #Distance from home
                           sliderInput("i_distance", "Distance from home", min = 1, max = 30, value = 15),
                           #Education
                           selectInput("i_education", "Education", 
                                       choices = list("Below College" = 1, 
                                                      "College"  = 2, 
                                                      "Bachelor" = 3,
                                                      "Master"   = 4,
                                                      "Doctor"   = 5), 
                                       selected = 3),
                           #EnviromentSatisfaction
                           selectInput("i_env_satisfaction", "Enviroment Satisfaction", 
                                       choices = list("Low" = 1, 
                                                      "Medium"  = 2, 
                                                      "High" = 3,
                                                      "Very High"   = 4), 
                                       selected = 2),
                           # #JobInvolvement
                           # selectInput("i_involvement", "Job Involement", 
                           #             choices = list("Low" = 1, 
                           #                            "Medium"  = 2, 
                           #                            "High" = 3,
                           #                            "Very High"   = 4), 
                           #             selected = 2),                         
                           #Gender
                           selectInput("i_gender", "Gender", 
                                       choices = list("Female" = "Female", 
                                                      "Male"  = "Male"), 
                                       selected = 2),  
                           #NumCompanyWorked
                           sliderInput("i_companyworked", "No Company Worked", min = 1, max = 10, value = 2),
                           #TotalWorkingYears
                           sliderInput("i_totalyworked", "Total Working Years", min = 0, max = 40, value = 18),                                                
                           #Training
                           sliderInput("i_training", "Training Times Last Year", min = 0, max = 10, value = 2),                                                
                         ),
                         box(width = NULL,align="center",
                             actionButton("predict", label = "Predict"),                           
                         )
                       ),
                       column(width = 3,
                         verbatimTextOutput("case")
                         # verbatimTextOutput("model")
                       ),
                       valueBoxOutput("PredictBox", width = 3),                              
                  )
                       # box(
                       #   title = "Result", width = 8, status = "primary", solidHeader = TRUE,
                       #   h2("Results")
                       # )
)

#------------------------------------------------------------
# Datos
#------------------------------------------------------------
tab_datos <- tabItem(tabName = "datos",
  fluidRow(
      # column(
        box(
          title = "Data", width = 12, status = "primary", solidHeader = TRUE,
          DT::DTOutput("original_datos2")
        )
        # ,width=12)
  )
)

