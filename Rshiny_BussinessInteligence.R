# Load required packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(rsconnect)

# Shiny app UI
ui <- fluidPage(
  titlePanel("E-commerce Sales Prediction"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file", accept = c(".csv")),
      hr(),
      h4("Enter Predictor Data"),
      numericInput("x1", "Monthly Visitors:", min = 0, value = 150000),
      numericInput("x2", "Transaction Amount:", min = 0, value = 8000),
      numericInput("x3", "Number of Items:", min = 0, value = 5),
      numericInput("x4", "Customer Rating:", min = 0, value = 8.5),
      numericInput("x5", "Monthly Sales:", min = 0, value = 20000),
      actionButton("predict_btn", "Predict", class = "btn-primary", width = "100%"),
      tags$style(HTML('.btn-primary {background-color: #007BFF; border: none; color: white; padding: 15px 32px; text-align: center; text-decoration: none; display: inline-block; font-size: 16px;}'))
    ),
    mainPanel(
      plotOutput("sales_plot"),
      verbatimTextOutput("prediction_output")
    )
  )
)

# Shiny app server
server <- function(input, output) {
  # Function to create example data
  create_example_data <- function() {
    return(data.frame(
      Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
    ))
  }
  
  # Reactive function to predict sales and visualize
  predict_sales <- eventReactive(input$predict_btn, {
    data <- create_example_data()
    
    # Predict sales using a simple linear regression model (you can use a more complex model)
    model <- lm(y ~ Month, data = data)
    predicted_sales <- predict(model, newdata = data.frame(Month = data$Month))
    
    # Create a data frame for visualization
    result_data <- data.frame(Month = data$Month, y = data$y, Predicted = predicted_sales)
    
    # Visualize predicted sales over time
    sales_plot <- ggplot(result_data, aes(x = Month, y = y)) +
      geom_line(color = "#007BFF", size = 1) +
      geom_point(color = "#007BFF", size = 3) +
      geom_point(data = result_data[result_data$Month == "Dec", ], aes(x = Month, y = Predicted), color = "#FF4500", size = 3) +
      labs(title = "Monthly Sales Prediction",
           x = "Month",
           y = "Sales") +
      theme_minimal()
    
    # Print the plot for debugging
    print(sales_plot)
    
    # Return results
    list(
      sales_plot = sales_plot,
      prediction_output = paste("Predicted Sales:", round(predicted_sales, 2))
    )
  })
  
  # Display predicted sales plot
  output$sales_plot <- renderPlot({
    predict_sales()$sales_plot
  })
}

# Run the Shiny app
shinyApp(ui, server)

getwd()
#2
data2 <- data.frame(
  adplacement = c("day1", "day2", "day3", "day4", "day5", "day6", "day7", "day8", "day9", "day10"),
  leftsidebar = c(2.5,2.7,2.8,2.6,3.0,2.4,2.9,2.5,2.6,2.7),
  centerpage = c(3.8,3.5,4.0,3.7,3.9,3.6,4.1,3.4,3.8,3.9),
  rightsidebar= c(3.1,2.9,3.0,3.2,3.3,2.8,3.4,3.1,3.2,3.5)
)
# Install and load required libraries
install.packages(c("shiny", "tidyverse", "shinythemes", "shinydashboard"))

library(shiny)
library(tidyverse)
library(shinythemes)
library(shinydashboard)



# Example CTR data
example_data <- data.frame(
  AdPlacement = rep(c("Left Sidebar", "Center Page", "Right Sidebar"), each = 10),
  CTR = c(
    2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7,
    3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9,
    3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5
  )
)

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = "CTR Analysis App",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    fileInput("file", "Upload CSV file", accept = c(".csv")),
    br(),
    h4("User-Input CTRs"),
    numericInput("left_sidebar_ctr", "Left Sidebar CTR:", min = 0, value = 3),
    numericInput("center_page_ctr", "Center Page CTR:", min = 0, value = 4),
    numericInput("right_sidebar_ctr", "Right Sidebar CTR:", min = 0, value = 3),
    br(),
    actionButton("analyze_btn", "Analyze", class = "btn-primary", width = "100%"),
    br(),
    br(),
    tags$style(HTML('.btn-primary {background-color: #4CAF50; border: none; color: white; padding: 15px 32px; text-align: center; text-decoration: none; display: inline-block; font-size: 16px;}'))
  ),
  
  dashboardBody(
    fluidRow(
      box(
        title = "CTR Visualization",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("ctr_plot", height = 300)
      ),
      box(
        title = "Summary Output",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        verbatimTextOutput("summary_output")
      )
    )
  )
)


# Server
server <- function(input, output) {
  # Reactive function to analyze CTR
  analyze_ctr <- eventReactive(input$analyze_btn, {
    if (!is.null(input$file)) {
      uploaded_data <- read.csv(input$file$datapath)
    } else {
      # Use example dataset if no file is uploaded
      uploaded_data <- example_data
    }
    
    # Add user-input CTR data
    user_data <- data.frame(
      AdPlacement = factor(c("Left Sidebar", "Center Page", "Right Sidebar"), levels = levels(uploaded_data$AdPlacement)),
      CTR = c(input$left_sidebar_ctr, input$center_page_ctr, input$right_sidebar_ctr)
    )
    
    # Combine datasets
    combined_data <- rbind(uploaded_data, user_data)
    
    # Perform pairwise t-tests for each AdPlacement level
    t_test_results <- pairwise.t.test(combined_data$CTR, combined_data$AdPlacement, p.adjust.method = "bonferroni")
    
    # Visualize CTR
    ctr_plot <- ggplot(combined_data, aes(x = AdPlacement, y = CTR)) +
      geom_boxplot(fill = "#4CAF50", color = "#333333", alpha = 0.7) +
      labs(title = "Click-Through Rates by Ad Placement",
           x = "Ad Placement",
           y = "CTR") +
      theme_minimal()
    
    # Return results
    list(
      t_test_results = t_test_results,
      combined_data = combined_data,
      ctr_plot = ctr_plot
    )
  })
  
  # Display CTR plot
  output$ctr_plot <- renderPlot({
    analyze_ctr()$ctr_plot
  })
  
  # Display summary output
  output$summary_output <- renderPrint({
    result <- analyze_ctr()
    cat("CTR Analysis Summary:\n")
    cat("----------------------------\n")
    print(result$t_test_results)
  })
}

# Run the Shiny app
shinyApp(ui, server)
rsconnect::deployApp("C:\\Users\\skenc\\Downloads\\Rshinyuas", appPrimaryDoc = "uasbi2.R")
