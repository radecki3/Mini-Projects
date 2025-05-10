#imports
install.packages("openintro")
install.packages("shinythemes")
library(shiny)
library(ggplot2)
library(openintro)
library(dplyr)
library(shinythemes)

#get data
data(loans_full_schema)
loans_data <- na.omit(loans_full_schema)

#creates another dataframe with (numeric) outliers removed, which will be an option to call later
loans_no_out <- loans_data

#gets numerical + categorical vars
vars <- names(loans_data)
num_vars <- names(Filter(is.numeric, loans_data))
cat_vars <- setdiff(vars, num_vars)

#loop over numeric vars, calculate outliers
for (var in num_vars) {
  Q1 <- quantile(loans_no_out[[var]], 0.25)
  Q3 <- quantile(loans_no_out[[var]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - (1.5*IQR)
  upper_bound <- Q3 + (1.5*IQR)
  #if outside of typical bounds, get rid of it
  good_vals <- (loans_no_out[[var]] >= lower_bound) & (loans_no_out[[var]] <= upper_bound)
  loans_no_out <- loans_no_out[good_vals,]
}

#UI stuff
ui <- fluidPage(
  #title
  titlePanel("Lending Club Loan Data"),
  
  #sidebar with a bunch of different selection options
  sidebarLayout(
    sidebarPanel(
      #adds an option to remove outliers
      checkboxInput("rmv_out", 
                    label = tags$span("Remove Outliers", style= "font-size:18px;"), 
                    value = TRUE),
      #divider between controls
      hr(style= "border-top: 3px solid #000000;"),
      
      h4("Plot Controls"),
      
      #scatterplot
      selectInput("scatter_x_var", "Scatterplot X-axis", choices = num_vars, selected = "annual_income"),
      selectInput("scatter_y_var", "Scatterplot Y-axis", choices = num_vars, selected = "paid_total"),
      selectInput("scatter_col", "Color Scatterplot By", 
                  choices = c("None", cat_vars), selected = "loan_purpose"),
      
      #divider between controls
      hr(style= "border-top: 1px solid #000000;"),
      
      #boxplot
      selectInput("box_num_var", "Boxplot Variable", choices = num_vars, selected = "annual_income"),
      selectInput("box_group_var", "Boxplot Grouping", choices = cat_vars, selected = "grade"),
      
      #divider between controls
      hr(style= "border-top: 1px solid #000000;"),
      
      #barplot
      selectInput("bar_cat_var", "Barplot Variable", choices = cat_vars, selected = "loan_purpose"),

      #divider between controls
      hr(style= "border-top: 3px solid #000000;"),
    
      #linear regression
      h4("Linear Regression Controls"),
      selectInput("lin_reg_y", "Y (Response)", choices = num_vars, selected = "annual_income"),
      selectInput("lin_reg_x", "X (Predictor)", choices = num_vars, selected = "emp_length"),
      
      #divider between controls
      hr(style= "border-top: 3px solid #000000;"),
      
      #correlation slider
      sliderInput("n_corr", 
                  label = "Top Correlations to Show", 
                  min = 1, max = 50, value = 10, step = 1),
      
      #sidebar width
      width=3
    ),
    
    #adds panel tabs which bring you to different plots
    mainPanel(
      tabsetPanel(
        #about section
        tabPanel("About",
                 fluidRow(
                   column(width=12,
                     tags$div(style = "padding: 20px; font-size: 20px;",
                              h3("About This App"),
                              p("This Shiny app explores the OpenIntro Loan data from the Lending Club."),
                              p("Users can explore typical visualizations including scatter plots, box plots, and bar plots."),
                              p("Also included is some simple linear regression modeling and correlation analysis."),
                              p("Users have the option of removing outliers if desired (recommended)."),
                              p("Make sure to SCROLL DOWN! There is some sidebar content that may get cut off."),
                              p("Created by Martin Radecki for STAT385 Final Project.")
                     )
                   )
                 )
        ),
        #scatterplot
        tabPanel("Scatterplot",
          fluidRow(
            column(width=12, plotOutput("scatter", height="600px")) #make sure to fit whole tab size
          )
        ),
        #boxplot
        tabPanel("Boxplot",
          fluidRow(
            column(width=12, plotOutput("box", height="600px"))
          )
        ),
        #barplot
        tabPanel("Barplot",
                 fluidRow(
                   column(width = 12, plotOutput("bar", height = "600px"))
                 )
        ),
        #linear regression
        tabPanel("Linear Regression", plotOutput("regplot", height = "600px"), 
                 verbatimTextOutput("regsummary"),
                 verbatimTextOutput("regstats")
        ),
        #correlation
        tabPanel("Correlations",plotOutput("corrplot", height = "600px"),
                 verbatimTextOutput("corrtext"))
      )
    )
  ),
  #theme
  theme = shinytheme("slate")
)

#Server Stuff
server <- function(input, output) {
  
  #will get the outlier-cleaned data if box is checked
  filtered_data <- reactive({
    if (input$rmv_out) {
      loans_no_out
    } else {
      loans_data
    }
  })
  
  #scatterplot
  output$scatter <- renderPlot({
    #coloring
    color <- if (input$scatter_col == "None"){
      NULL
    } else {
      aes_string(color = input$scatter_col)
    }
    #plot
    ggplot(filtered_data(), aes_string(x = input$scatter_x_var, y = input$scatter_y_var)) +
      (if (!is.null(color)) color else geom_blank()) +
      geom_point(alpha = 0.8, size=3) +
      labs(x = input$scatter_x_var, y = input$scatter_y_var, color = input$scatter_col) +
      theme_minimal() +
      #changes some font sizes
      theme(
        axis.title.x = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=12),
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5)
      )
  })
  
  #boxplot
  output$box <- renderPlot({
    #plot
    ggplot(filtered_data(), aes_string(x = input$box_num_var, y = input$box_group_var, fill = input$box_group_var)) +
      stat_boxplot(geom='errorbar') + #this adds the vertical lines onto the whiskers
      geom_boxplot() +
      labs(x = input$box_num_var, y = input$box_group_var) +
      theme_minimal() + 
      #changes some font sizes
      theme(
        axis.title.x = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=12),
        legend.background = element_rect(fill = "white", color = "black", size = 0.5)
      )
  })
  
  #barplot
  output$bar <- renderPlot({
    #plot
    ggplot(filtered_data(), aes_string(x = input$bar_cat_var)) +
      geom_bar(fill='darkred') +
      labs(x = input$bar_cat_var, y = "Count") +
      theme_minimal() +
      #changes some font sizes
      theme(
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14, angle = 45, hjust=1),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold")
      )
  })
  
  #linar regression plot
  output$regplot <- renderPlot({
    #plot
    ggplot(filtered_data(), aes_string(x = input$lin_reg_x, y = input$lin_reg_y)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE, color = "darkred") +
      labs(x = input$lin_reg_x, y = input$lin_reg_y) +
      theme_minimal() +       
      #changes some font sizes
      theme(
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14, angle = 45, hjust=1),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold")
      ) 
  })
  
  #linear regression raw stats
  output$regstats <- renderPrint({
    #fit model
    model <- lm(as.formula(paste(input$lin_reg_y, "~", input$lin_reg_x)), data = filtered_data())
    summary(model)
  })
  
  #linear regression calculated/clean stats
  output$regsummary <- renderPrint({
    #fit model
    model <- lm(as.formula(paste(input$lin_reg_y, "~", input$lin_reg_x)), data = filtered_data())
    #calculate stats
    r2 <- summary(model)$r.squared
    pval <- summary(model)$coefficients[2, 4]
    #print output
    cat("Model Fit Summary:\n")
    cat(sprintf("R-squared: %.4f\n", r2))
    cat(sprintf("P-value for slope: %.4g\n", pval))
  })
  
  #correlation matrix plot
  output$corrplot <- renderPlot({
    # select only numeric columns
    df <- filtered_data()[, num_vars]
    #remove columns with zero variance
    df <- df[, apply(df, 2, function(x) sd(x, na.rm = TRUE) > 0)]
    #correlations
    corr_matrix <- cor(df)
    #heatmap
    heatmap(corr_matrix, margins = c(7, 7))
  })
  
  #correlation numbers
  output$corrtext <- renderPrint({
    #same correlation calculations
    df <- filtered_data()[, num_vars]
    df <- df[, apply(df, 2, function(x) sd(x, na.rm = TRUE) > 0)]
    corr_matrix <- cor(df)
    #save correlations as a dataframe
    corr_df <- as.data.frame(as.table(corr_matrix))
    names(corr_df) <- c("x", "y", "corr")
    #remove self-correlations
    corr_df <- corr_df[corr_df$x != corr_df$y, ]
    #remove duplicates
    corr_df <- corr_df[!duplicated(t(apply(corr_df[, 1:2], 1, sort))), ]
    # sort and show top
    corr_df <- corr_df[order(-abs(corr_df$corr)), ]
    cat("Top Absolute Correlations:\n\n")
    for (i in 1:min(input$n_corr, nrow(corr_df))) {
      cat(sprintf("%s vs %s: %.3f\n", corr_df$x[i], corr_df$y[i], corr_df$corr[i]))
    }
  })
  
}

#shiny app
shinyApp(ui = ui, server = server)
