library(shiny)
library(shinythemes)
library(ISLR)
library(DataExplorer)
library(ggplot2)
require(dplyr)
library(ggcorrplot)
library(tidyr)
library(purrr)
library(printr)
library(pROC) 
library(ROCR) 
library(caret)
library(car)
library(rpart)
library(rpart.plot)
library(e1071) 
library(ISLR) 
library(markdown)
library(reticulate)
install_miniconda(path = miniconda_path(), update = TRUE, force = FALSE)
source_python("sentiment_analysis.py")

# Get the text output
text_output <- capture.output(sentiments)

# Create a function to plot the pie chart
plot_pie_chart <- function() {
  plt <- import("matplotlib.pyplot")
  plt$pie(sizes, labels=labels, colors=colors, autopct='%1.1f%%', startangle=140)
  plt$axis('equal')
  plt$show()
}

# Create a function to plot the histogram
plot_histogram <- function() {
  plt <- import("matplotlib.pyplot")
  plt$hist(sentiments, bins=20)
  plt$xlabel("Sentiment Score")
  plt$ylabel("Number of Records")
  plt$title("Sentiment Analysis of " + header)
  plt$show()
}


ui <- fluidPage(
  navbarPage("Analyse!",
             tabPanel("Table",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Choose CSV File",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                          ),
                          tags$hr(),
                          checkboxInput("header", "Header", TRUE)
                        ),
                        mainPanel(
                          tableOutput("contents")
                        )
                      )
             ),
             tabPanel("Summary",
                      verbatimTextOutput("summary")
             ),
             tabPanel("Histogram"
                      ,mainPanel("Plotting Histograms to understand the distributions",
                                 fluidRow(
                                   splitLayout(cellWidths = c("32%", "30%","30%","30%","30%"), 
                                               plotOutput("Hist1"), plotOutput("Hist2"),plotOutput("Hist3"), plotOutput("Hist4"))
                                 )
                      )
             ),
             tabPanel("Categorical",
                      mainPanel("Plotting Bar Charts to understand the Categorical Variables",
                                fluidRow(
                                  splitLayout(cellWidths = c("30%", "30%","30%","30%","30%"), 
                                              plotOutput("Bar1"),plotOutput("Bar2"),plotOutput("Bar3"),plotOutput("Bar4"))
                                )
                      )
             ),
             tabPanel("Customer activeness",
                      mainPanel("Customer activeness",
                                fluidRow(
                                  splitLayout(cellWidths = c("73%", "73%"), 
                                              plotOutput("Churn1"),plotOutput("Churn2"))
                                )
                      )
                      
             ),
             tabPanel("sentiment analysis",
                      sidebarPanel(
                        h3("Text Output"),
                        verbatimTextOutput("text_output")
                      ),
                      mainPanel(
                        h3("Pie Chart"),
                        plotOutput("pie_chart"),
                        h3("Histogram"),
                        plotOutput("histogram")
                      )
                      
             ),

             tabPanel("About Us",
                      fluidRow(
                           includeMarkdown("about.md")
                        )
             )
             
  )
  
)


server <- function(input, output) {
  output$contents <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header)
  })
  output$summary <- renderPrint({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    summary(read.csv(inFile$datapath, header = input$header))
  })
  output$Hist1 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    hist(tab$Numoftwitts)
  }) 
  output$Hist2 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    hist(tab$Age)
    
  }) 
  output$Hist3 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    hist(tab$Numoffollowing_in_k, breaks = 5)
    
  }) 
  output$Hist4 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    hist(tab$Numoffollows_in_k, breaks = 10)
    
  }) 
  output$Bar1 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    ggplot(tab, aes(x = factor(Gender))) + geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
    
  }) 
  output$Bar2 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    ggplot(tab, aes(x = factor(Geography))) + geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
    
  }) 
  output$Bar3 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    ggplot(tab, aes(x = factor(Numoffollows_in_k))) + geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
    
  }) 
  output$Bar4 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    ggplot(tab, aes(x = factor(IsActiveMember))) + geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
    
  }) 
  output$Churn1 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    ggplot(tab, aes(x = IsActiveMember)) + geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")+
      labs(title = "Active vs inActive", x = "Active Status")
    table1 <- table(tab$IsActiveMember, tab$Geography, dnn=c("Active count", "Geography")) 
    barplot(table1, ylab="Frequency", xlab="Geography", main="Side-By-Side Bar Chart", 
            col=c("turquoise4", "turquoise2" ), beside=TRUE, width=.2)
    
    legend("right", title="IsActiveMember", legend= sort(unique(tab$IsActiveMember)),
           fill =c("turquoise4", "turquoise2" ), box.lty=0)
    
    
  }) 
  output$Churn2 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tab=read.csv(inFile$datapath, header = input$header)
    ggplot(tab, aes(x = factor(Geography))) +
      geom_bar(aes(fill = Gender),position="dodge") +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black") +
      scale_fill_manual(values=c('#999999','#E69F00'))
    
  })
  output$text_output <- renderPrint({
    text_output
  })
  output$pie_chart <- renderPlot({
    plot_pie_chart()
  })
  output$histogram <- renderPlot({
    plot_histogram()
  })
  
  
  
 
}

shinyApp(ui, server)