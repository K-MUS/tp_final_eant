#library(shiny)
nbins = list(Age = 20,
             BusinessTravel = 3,
             Department = 3, 
             DistanceFromHome = 10, 
             Education = 5, 
             EducationField = 6, 
             EnvironmentSatisfaction = 4, 
             Gender = 2, 
             JobInvolvement = 4, 
             JobRole = 9, 
             JobSatisfaction = 4, 
             MaritalStatus = 3, 
             MonthlyIncome = 30, 
             NumCompaniesWorked = 10, 
             OverTime = 2, 
             RelationshipSatisfaction = 4, 
             StockOptionLevel = 4, 
             TotalWorkingYears = 30, 
             TrainingTimesLastYear = 7, 
             WorkLifeBalance = 4, 
             YearsAtCompany = 20, 
             YearsInCurrentRole = 15,
             YearsSinceLastPromotion = 15,
             YearsWithCurrManager = 10)

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
        df
    })
    
#------------------------------------------------------------
# EDA - Correlation
#------------------------------------------------------------    
    # df_corr <- reactive({
    #     #numeric <- df %>% select(Age,DailyRate,DistanceFromHome,HourlyRate,MonthlyIncome,MonthlyRate,NumCompaniesWorked,PercentSalaryHike,YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,YearsWithCurrManager,TotalWorkingYears,TrainingTimesLastYear,StockOptionLevel)
    #     #numeric <- df[,c("Age","DailyRate","DistanceFromHome","HourlyRate","MonthlyIncome","MonthlyRate","NumCompaniesWorked","PercentSalaryHike","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager","TotalWorkingYears","TrainingTimesLastYear","StockOptionLevel")]  
    #     cor(df)
    # })
    
    output$eda_corr_plot <- renderPlot({
        corrplot(cor(data_table()), method="circle", tl.col="#3982B7",type="full")
    }, height = 600 #, width =510
    )    
    
    # output$corr_info <- renderText({
    #     paste0("x=", as.character(round(input$eda_corr_plot_click$x)), "\ny=", as.character(round(input$eda_corr_plot_click$y)))
    # })
    
    output$eda_corr_click_info <- renderPlot({
        x_column <- ifelse(is.null(input$eda_corr_plot_click$x), ncol(data_table()), round(input$eda_corr_plot_click$x))
        y_column <- ifelse(is.null(input$eda_corr_plot_click$y), 1, round(input$eda_corr_plot_click$y))
        n_columns <- ncol(data_table())+1
        #df_sub <- df[,c("Age","DailyRate","DistanceFromHome","HourlyRate","MonthlyIncome","MonthlyRate","NumCompaniesWorked","PercentSalaryHike","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager","TotalWorkingYears","TrainingTimesLastYear","StockOptionLevel")]
        df_sub <- data_table()
        
        if(colnames(df_sub)[n_columns - y_column] == "Attrition"){
            ggplot(df_sub, aes_string(colnames(df_sub)[x_column])) +
                geom_density(alpha = 0.2, aes(fill = factor(Attrition))) +
                guides(fill=guide_legend(title="Attrition:")) + theme_bw() +
                theme(legend.position = "bottom")
        } else if(colnames(df_sub)[x_column] == "Attrition"){
            ggplot(df_sub, aes_string(colnames(df_sub)[n_columns-y_column])) +
                geom_density(alpha = 0.2, aes(fill = factor(Attrition))) + theme_bw() +
                guides(fill=guide_legend(title="Attrition:")) +
                theme(legend.position = "bottom")
        } else if(colnames(df_sub)[x_column] != colnames(df_sub)[n_columns-y_column]) {
            x_name = colnames(df_sub)[x_column]
            y_name = colnames(df_sub)[n_columns-y_column]
            ggplot(df_sub, aes_string(x_name, y_name)) +
                stat_bin2d(bins = c(nbins[[x_name]], nbins[[y_name]])) +
                guides(colour = guide_legend(override.aes = list(alpha = 1)),
                       fill = guide_legend(override.aes = list(alpha = 1))) + theme_bw() +
                theme(legend.position = "bottom")
        } else {
            ggplot(df_sub, aes_string("factor(Attrition)", colnames(df_sub)[x_column])) +
                geom_violin(alpha = 0.2, aes(fill = factor(Attrition))) + theme_bw() +
                guides(fill=FALSE) + xlab("Attrition") +
                theme(legend.position = "bottom")
        }
    }, height = 280)
    
    output$original_datos = DT::renderDataTable({
        df
    })
    
    output$original_datos2 = DT::renderDataTable({
        data_table()
    })
    
}
)
