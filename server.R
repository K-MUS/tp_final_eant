#library(shiny)

# Define server logic required to draw a histogram
SERVER <- shinyServer(function(input, output) {
    # The currently selected tab from the first box
    output$box_eda_selected <- renderText({
        input$box_eda
    })

#------------------------------------------------------------
# General
#------------------------------------------------------------    
    data_table <- reactive({
        df_num
    })
    df_table <- reactive({
        df
    })
#------------------------------------------------------------
# EDA - Correlation
#------------------------------------------------------------    
    output$eda_corr_plot <- renderPlot({
        corrplot(cor(data_table()), method="circle", tl.col="#3982B7",type="full")
    }, height = 600 #, width =510
    )    
    
    # output$corr_info <- renderText({
    #     paste0("x=", as.character(round(input$eda_corr_plot_click$x)), "\ny=", as.character(round(input$eda_corr_plot_click$y)))
    # })
    
    output$corr_clicked <- reactive({
        result <- ifelse(is.null(input$eda_corr_plot_click$x), FALSE, TRUE)
        return(result)
    })
    
    output$eda_corr_click_info <- renderPlot({
        x_column <- ifelse(is.null(input$eda_corr_plot_click$x), ncol(data_table()), round(input$eda_corr_plot_click$x))
        y_column <- ifelse(is.null(input$eda_corr_plot_click$y), 1, round(input$eda_corr_plot_click$y))
        n_columns <- ncol(data_table())+1
        #df_sub <- df[,c("Age","DailyRate","DistanceFromHome","HourlyRate","MonthlyIncome","MonthlyRate","NumCompaniesWorked","PercentSalaryHike","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager","TotalWorkingYears","TrainingTimesLastYear","StockOptionLevel")]
        df_sub <- data_table()
        
        if( x_column <= ncol(data_table() ) & y_column <= ncol(data_table()) & x_column > 0 & y_column > 0 ){
            if(colnames(df_sub)[n_columns - y_column] == "Attrition"){
                ggplot(df_sub, aes_string(colnames(df_sub)[x_column])) +
                    geom_density(alpha = 0.25, aes(fill = factor(Attrition))) +
                    guides(fill=guide_legend(title="Attrition:")) + 
                    theme_bw() +
                    theme(legend.position = "bottom") +
                    scale_fill_manual(values=c("#eb5e28", "#006494"))
            } else if(colnames(df_sub)[x_column] == "Attrition"){
                ggplot(df_sub, aes_string(colnames(df_sub)[n_columns-y_column])) +
                    geom_density(alpha = 0.25, aes(fill = factor(Attrition))) +
                    guides(fill=guide_legend(title="Attrition:")) + 
                    theme_bw() +
                    theme(legend.position = "bottom") +
                    scale_fill_manual(values=c("#eb5e28", "#006494"))
            } else if(colnames(df_sub)[x_column] != colnames(df_sub)[n_columns-y_column]) {
                x_name = colnames(df_sub)[x_column]
                y_name = colnames(df_sub)[n_columns-y_column]
                ggplot(df_sub, aes_string(x_name, y_name) ) +
                    stat_bin2d()
            } else {
                ggplot(df_sub, aes_string("factor(Attrition)", colnames(df_sub)[x_column])) +
                    geom_violin(alpha = 0.2, aes(fill = factor(Attrition))) + theme_bw() +
                    scale_fill_manual(values=c("#eb5e28", "#006494")) +
                    guides(fill=FALSE) + xlab("Attrition") +
                    theme(legend.position = "bottom")
            }
        }
    }, height = 320)

#------------------------------------------------------------
# EDA - Attrition
#------------------------------------------------------------
    # Attrition Bars
    output$eda_attr_attr <- renderPlot({
        df_table() %>% group_by(Attrition) %>% summarise(Count=n()) %>%
        
        ggplot( aes(x=Attrition, y=Count)) + 
            geom_bar(stat="identity", fill="#7fc8f8") +
            coord_flip() + 
            #geom_text(aes(x=Attrition, y=0.01, label= Count),hjust=-0.8, vjust=-1, size=3, colour="black", fontface="bold", angle=360) + 
            
            scale_fill_manual(values=c("#B5EAD7", "#FF9AA2")) + 
            scale_color_manual(values=c("#B5EAD7","#FF9AA2")) + 
            geom_label(aes(label=Count, fill = Attrition), colour = "Black", fontface = "bold") + 
            
            labs(x="Employee Attrition",y="Amount") + 
            theme_few() +
            theme(legend.position="none")
            
    })
    
    # Attrition - Education Level    
    output$eda_attr_edu_level <- renderPlot({
        df_tmp <- df_table()
        
        # Give names for the different education levels.
        df_tmp$Educational_Levels <-  ifelse(df$Education == 1, "Without College D.",
                                         ifelse(df$Education == 2 , "College D.",
                                                ifelse(df$Education == 3, "Bachelors D.",
                                                       ifelse(df$Education == 4, "Masters D.", "Phd D."))))
        
        # I want to know in terms of proportions if we are loosing key talent here.
        df_tmp <- df_tmp %>% select(Educational_Levels, Attrition) %>% group_by(Educational_Levels, Attrition) %>% 
            summarize(n=n())# %>% 
            
            ggplot(df_tmp, aes(x=fct_reorder(Educational_Levels,n), y=n, fill=Attrition, color=Attrition)) + geom_bar(stat="identity") + 
                facet_wrap(~Attrition) + 
                coord_flip() + 
                scale_fill_manual(values=c("#B5EAD7", "#FF9AA2")) + 
                scale_color_manual(values=c("#B5EAD7","#FF9AA2")) + 
                geom_label(aes(label=n, fill = Attrition), colour = "Black", fontface = "bold") + 
                labs(x="", y="Number of Employees") + 
                theme_few() +
                theme(legend.position="none") 
    })
    
    # Attrition - Job Satisfaction    
    output$eda_attr_job <- renderPlot({
        df_tmp <- df_table()
        
        df_tmp$JobSatisfaction <-  ifelse(df$JobSatisfaction == 1, "Low",
                                             ifelse(df$JobSatisfaction == 2 , "Medium",
                                                    ifelse(df$JobSatisfaction == 3, "High",
                                                           ifelse(df$JobSatisfaction == 4, "Very High",""))))


        # I want to know in terms of proportions if we are loosing key talent here.
        df_tmp %>% select(JobSatisfaction, Attrition) %>% group_by(JobSatisfaction, Attrition) %>% 
            summarize(n=n()) %>% 
        
        ggplot( aes(x=fct_reorder(JobSatisfaction, n, .desc = FALSE ), y=n, fill=Attrition, color=Attrition)) + geom_bar(stat="identity") + 
            facet_wrap(~Attrition) + 
            coord_flip() + 
            scale_fill_manual(values=c("#B5EAD7", "#FF9AA2")) + 
            scale_color_manual(values=c("#B5EAD7","#FF9AA2")) + 
            geom_label(aes(label=n, fill = Attrition), colour = "Black", fontface = "bold") + 
            labs(x="", y="Number of Employees") + 
            theme_few() +
            theme(legend.position="none") 
    })
    
    # Attrition Age
    output$eda_attr_age <- renderPlot({
        ggplot(df_table(), aes(x=Attrition, y=Age, color=Attrition, fill=Attrition)) + 
            geom_boxplot() + 
            scale_fill_manual(values=c("#fdc5f5", "#59a5d8")) + 
            scale_color_manual(values=c("#f7aef8", "#386fa4")) +
            #coord_flip() + 
            #labs(title="Are there any Gender Disparities in Income?") +       
            theme_few() +
            theme(legend.position="none")
    })    
    
    # Attrition Monthly Income
    output$eda_attr_income <- renderPlot({
        ggplot(df_table(), aes(x=Attrition, y=MonthlyIncome, color=Attrition, fill=Attrition)) + 
            geom_boxplot() + 
            scale_fill_manual(values=c("#fdc5f5", "#59a5d8")) + 
            scale_color_manual(values=c("#f7aef8", "#386fa4")) +
            #coord_flip() + 
            #labs(title="Are there any Gender Disparities in Income?") +       
            theme_few() +
            theme(legend.position="none")
    })
    
    # Attrition Distance from Home
    output$eda_attr_distance <- renderPlot({
        ggplot(df_table(), aes(x=Attrition, y=DistanceFromHome, color=Attrition, fill=Attrition)) + 
            geom_boxplot() + 
            scale_fill_manual(values=c("#fdc5f5", "#59a5d8")) + 
            scale_color_manual(values=c("#f7aef8", "#386fa4")) +
            #coord_flip() + 
            #labs(title="Are there any Gender Disparities in Income?") +       
            theme_few() +
            theme(legend.position="none")
    })
    
    # Attrition Years same Manager
    output$eda_attr_manager <- renderPlot({
        ggplot(df_table(), aes(x=Attrition, y=YearsWithCurrManager, color=Attrition, fill=Attrition)) + 
            geom_boxplot() + 
            scale_fill_manual(values=c("#fdc5f5", "#59a5d8")) + 
            scale_color_manual(values=c("#f7aef8", "#386fa4")) +
            #coord_flip() + 
            #labs(title="Are there any Gender Disparities in Income?") +       
            theme_few() +
            theme(legend.position="none")
    })

    # Monthly Gender
    output$eda_monthly_gender <- renderPlot({
        ggplot(df_table(), aes(x=Gender, y=MonthlyIncome, color=Gender, fill=Gender)) + 
            geom_boxplot() + 
            scale_fill_manual(values=c("#fdc5f5", "#59a5d8")) + 
            scale_color_manual(values=c("#f7aef8", "#386fa4")) +
            #coord_flip() + 
            #labs(title="Are there any Gender Disparities in Income?") +       
            theme_few() +
            theme(legend.position="none")
    })

    # Monthly Job satisfaction
    output$eda_monthly_job <- renderPlot({
        df_tmp <- df_table()
        
        df_tmp$JobSatisfaction <-  ifelse(df$JobSatisfaction == 1, "Low",
                                          ifelse(df$JobSatisfaction == 2 , "Medium",
                                                 ifelse(df$JobSatisfaction == 3, "High",
                                                        ifelse(df$JobSatisfaction == 4, "Very High",""))))
        
        df_tmp <- df_tmp %>% select(JobSatisfaction, MonthlyIncome, Attrition) %>% group_by(JobSatisfaction, Attrition) %>%
            summarize(med=median(MonthlyIncome))
        
        ggplot(df_tmp, aes(x=fct_reorder(JobSatisfaction, -med), y=med, color=Attrition)) + 
            geom_point(size=5) +
            geom_line(size=3) +
            geom_segment(aes(x=JobSatisfaction,
                             xend=JobSatisfaction,
                             y=0,
                             yend=med)) +
            facet_wrap(~Attrition) +
            labs(y="Median Income", x="Level of Job Satisfaction") +
            coord_flip() +
            scale_color_manual(values=c("#58FA58", "#FA5858")) +
            geom_text(aes(x=JobSatisfaction, y=0.01, label= paste0("$ ", round(med,2))),
                      hjust=-0.5, vjust=-0.5, size=4,
                      colour="black", fontface="italic",
                      angle=360) +
            theme_few()
            # theme(legend.position="none", axis.text.x = element_text(angle=65, vjust=0.6), plot.title=element_text(hjust=0.5), strip.background = element_blank(),
            # strip.text = element_blank())
    })
    
#------------------------------------------------------------
# Predict
#------------------------------------------------------------
    #v = reactiveValues(df_pred = NULL)
    
    pred_case <- reactive({
        Age                   = as.integer(input$i_age)
        BusinessTravel        = as.factor(input$i_travel)
        Department            = as.factor(input$i_department)
        DistanceFromHome      = as.integer(input$i_distance)
        Education             = as.integer(input$i_education)
        EnvironmentSatisfaction = as.integer(input$i_env_satisfaction)
        Gender                = as.factor(input$i_gender)
        JobRole               = as.factor(input$i_role)
        JobSatisfaction       = as.integer(input$i_job_satisfaction)
        MonthlyIncome         = as.integer(input$i_monthly_income)
        NumCompaniesWorked    = as.integer(input$i_companyworked)
        OverTime              = as.factor(input$i_overtime)
        StockOptionLevel      = as.integer(input$i_stock)
        TotalWorkingYears     = as.integer(input$i_totalyworked)
        TrainingTimesLastYear = as.integer(input$i_training)
        YearsAtCompany        = as.integer(input$i_years_company)
        YearsInCurrentRole    = as.integer(input$i_years_role)
        YearsWithCurrManager  = as.integer(input$i_years_manager)
        
        str(BusinessTravel)
         
        test <- data.frame(Age,               
                      BusinessTravel,
                      Department,            
                      DistanceFromHome,
                      Education,     
                      EnvironmentSatisfaction,
                      Gender,
                      JobRole,   
                      JobSatisfaction,
                      MonthlyIncome,
                      NumCompaniesWorked,
                      OverTime,     
                      StockOptionLevel,
                      TotalWorkingYears,     
                      TrainingTimesLastYear,
                      YearsAtCompany,
                      YearsInCurrentRole,
                      YearsWithCurrManager)

        #test <- as.data.frame(test)
        # test$BusinessTravel = as.factor(test$BusinessTravel)
        test
    })
    
    pred_result_prob <-  eventReactive(input$predict, {
        # values <- reactiveValues(Age                   = input$i_age,
        #                          BusinessTravel        = input$i_travel,
        #                          Department            = input$i_department,
        #                          DistanceFromHome      = input$i_distance,
        #                          Education             = input$i_education,
        #                          EnvironmentSatisfaction = input$i_env_satisfaction,
        #                          Gender                = input$i_gender,
        #                          JobRole               = input$i_role,
        #                          MonthlyIncome         = input$i_monthly_income,
        #                          NumCompanyWorked      = input$i_companyworked,
        #                          OverTime              = input$i_overtime,
        #                          StockOptionLevel      = input$i_stock,
        #                          TotalWorkingYears     = input$i_totalyworked,
        #                          TrainingTimesLastYear = input$i_training,
        #                          YearsAtCompany        = input$i_years_company,
        #                          YearsInCurrentRole    = input$i_years_role,
        #                          YearsWithCurrManager  = input$i_years_manager)

        pred <- predict(model, newdata = pred_case(), type = "class")
    })

    
        # case_predict <- eventReactive(input$predict, {
    #     values <- reactiveValues(Age                   = input$i_age,
    #                              BusinessTravel        = input$i_travel,
    #                              Department            = input$i_department,
    #                              DistanceFromHome      = input$i_distance,
    #                              Education             = input$i_education,
    #                              EnvironmentSatisfaction = input$i_env_satisfaction,
    #                              Gender                = input$i_gender,
    #                              JobRole               = input$i_role,
    #                              MonthlyIncome         = input$i_monthly_income,
    #                              NumCompanyWorked      = input$i_companyworked,
    #                              OverTime              = input$i_overtime,
    #                              StockOptionLevel      = input$i_stock,
    #                              TotalWorkingYears     = input$i_totalyworked,
    #                              TrainingTimesLastYear = input$i_training,
    #                              YearsAtCompany        = input$i_years_company,
    #                              YearsInCurrentRole    = input$i_years_role,
    #                              YearsWithCurrManager  = input$i_years_manager)
    # })
    
    output$console_text <- renderText({
        #case_predict()
        #case_predict()[1]
#        text <- paste(" Age:", v$df_pred[1,1], "\n")
                      # "Business Travel:", case_predict()$BusinessTravel,"\n",
                      # "Department:", case_predict()$Department,"\n",
                      # "Distance From Home:", case_predict()$DistanceFromHome,"\n",
                      # "Education:", case_predict()$Education,"\n",
                      # "Enviroment Satisfaction:", case_predict()$EnvironmentSatisfaction,"\n",
                      # "Gender:", case_predict()$Gender,"\n",
                      # "Job Role:", case_predict()$JobRole,"\n",
                      # "Monthly Income:", case_predict()$MonthlyIncome,"\n",
                      # "No Company Worked:", case_predict()$NumCompanyWorked,"\n",
                      # "Overtime:", case_predict()$OverTime,"\n",
                      # "Stock Option Level:", case_predict()$StockOptionLevel,"\n",
                      # "Total Working Years:", case_predict()$TotalWorkingYears,"\n",
                      # "Training in Last Year:", case_predict()$TrainingTimesLastYear,"\n",
                      # "Years at Company:", case_predict()$YearsAtCompany,"\n",
                      # "Years in Current Role:", case_predict()$YearsInCurrentRole,"\n",
                      # "Years with Current Manager:", case_predict()$YearsWithCurrManager,"\n"
                      # )
        pred_result_prob()
    })    

    # output$PredictBox <- renderValueBox({
    #     valueBox(
    #         pred_result_prob(), "test", icon = icon("thumbs-up", lib = "glyphicon"),
    #         color = "yellow"
    #     )
    # })
#------------------------------------------------------------
# Data
#------------------------------------------------------------
    
    output$original_datos = DT::renderDataTable({
        df_num
    })
    
    output$original_datos2 = DT::renderDT(
        data_table(), options = list(autoWidth = TRUE,scrollX=TRUE)
    )

    outputOptions(output, "corr_clicked", suspendWhenHidden = FALSE)    
}
)


