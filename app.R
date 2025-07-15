# Install the necessary packages if not already installed
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("ggplot2")
# install.packages("DT")
# install.packages("readxl")

# Load the required libraries
library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)
library(readxl)
library(rmarkdown)
# Define the UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$style(HTML("
    body {background-color: #e0f2f1;} /* Light green background */
    h1 {color: #3b5998;} /* Deep blue for heading */
    .sidebar {background-color: #2f4f4f;} /* Dark slate gray for sidebar */
    .main-panel {background-color: #f0e68c;} /* Khaki for main panel */
  ")),
  
  titlePanel("Advanced Data Analysis App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("analysis_type", "Choose Analysis Type", choices = c("Single Dataset", "Compare Datasets")),
      
      # File upload input
      fileInput("file", "Upload File", accept = c("text/csv", ".csv", ".xlsx", ".xls")),
      
      # Dynamic UI for variable selection (depends on analysis type)
      uiOutput("var_select"),
      
      # Input options for graph and labels
      selectInput("graphType", "Select Graph Type", choices = c("Boxplot", "Scatterplot", "Regression", "Bar Chart", "Histogram", "Line Plot")),
      textInput("xLabel", "X-axis Label", ""),
      textInput("yLabel", "Y-axis Label", ""),
      textInput("title", "Graph Title", ""),
      
      actionButton("analyze", "Run Analysis"),
      downloadButton("downloadReport", "Download Results")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", dataTableOutput("dataTable")),
        tabPanel("Normality Test", verbatimTextOutput("normalityTest")),
        tabPanel("T-Test", verbatimTextOutput("tTest")),
        tabPanel("ANOVA", verbatimTextOutput("anovaTest")),
        tabPanel("Graphs", plotOutput("plot"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive function to read the uploaded file
  data <- reactive({
    req(input$file)
    file_ext <- tools::file_ext(input$file$name)
    if (file_ext == "csv") {
      read.csv(input$file$datapath)
    } else if (file_ext %in% c("xlsx", "xls")) {
      read_excel(input$file$datapath)
    } else {
      stop("Unsupported file type")
    }
  })
  
  # Display the uploaded data
  output$dataTable <- renderDataTable({
    req(data())
    datatable(data())
  })
  
  # Dynamic UI to select variables based on analysis type
  output$var_select <- renderUI({
    req(data())
    colnames <- names(data())
    
    if (input$analysis_type == "Compare Datasets") {
      # For comparison analysis: select two variables
      tagList(
        selectInput("var1", "Select Variable 1", choices = colnames),
        selectInput("var2", "Select Variable 2", choices = colnames)
      )
    } else {
      # For single dataset analysis: only one variable
      selectInput("var1", "Select Variable", choices = colnames)
    }
  })
  
  # Perform Normality Test
  output$normalityTest <- renderPrint({
    req(input$analyze)
    req(data())
    shapiro.test(na.omit(data()[[input$var1]]))
  })
  
  # Perform T-Test (if applicable)
  output$tTest <- renderPrint({
    req(input$analyze)
    req(data())
    if (input$analysis_type == "Compare Datasets") {
      t.test(na.omit(data()[[input$var1]]), na.omit(data()[[input$var2]]))
    }
  })
  
  # Perform ANOVA (if applicable)
  output$anovaTest <- renderPrint({
    req(input$analyze)
    req(data())
    if (input$analysis_type == "Compare Datasets") {
      fit <- aov(data()[[input$var1]] ~ data()[[input$var2]])
      summary(fit)
    }
  })
  
  # Plot Graphs
  output$plot <- renderPlot({
    req(input$analyze)
    req(data())
    graphType <- input$graphType
    plot <- NULL
    
    if (graphType == "Boxplot") {
      plot <- ggplot(data(), aes_string(x = input$var2, y = input$var1)) +
        geom_boxplot(fill = "#69b3a2", color = "#1b4f72")
    } else if (graphType == "Scatterplot") {
      plot <- ggplot(data(), aes_string(x = input$var1, y = input$var2)) +
        geom_point(color = "#2e86c1")
    } else if (graphType == "Regression") {
      plot <- ggplot(data(), aes_string(x = input$var1, y = input$var2)) +
        geom_point(color = "#2e86c1") +
        geom_smooth(method = "lm", se = FALSE, color = "#e74c3c")
    } else if (graphType == "Bar Chart") {
      plot <- ggplot(data(), aes_string(x = input$var1, fill = input$var2)) +
        geom_bar(position = "dodge", color = "#34495e")
    } else if (graphType == "Histogram") {
      plot <- ggplot(data(), aes_string(x = input$var1)) +
        geom_histogram(binwidth = 30, fill = "#d35400", color = "#c0392b")
    } else if (graphType == "Line Plot") {
      plot <- ggplot(data(), aes_string(x = input$var1, y = input$var2)) +
        geom_line(color = "#16a085")
    }
    
    plot +
      labs(x = input$xLabel, y = input$yLabel, title = input$title) +
      theme_minimal()
  })
  
  # Downloadable Report
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("analysis_results", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      writeLines(capture.output({
        cat("Normality Test:\n")
        print(shapiro.test(na.omit(data()[[input$var1]])))
        cat("\nT-Test:\n")
        if (input$analysis_type == "Compare Datasets") {
          print(t.test(na.omit(data()[[input$var1]]), na.omit(data()[[input$var2]])))
        }
        cat("\nANOVA:\n")
        if (input$analysis_type == "Compare Datasets") {
          fit <- aov(data()[[input$var1]] ~ data()[[input$var2]])
          print(summary(fit))
        }
      }), file)
    }
  )
}

# Run the app
shinyApp(ui, server)



