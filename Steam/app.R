#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(plyr)
library(tools)
library(shinythemes)
library(shinydashboard)
library(reshape2)
library(plotly)

steam <- read.csv("steam.csv",fileEncoding="UTF-8-BOM",header=TRUE, check.names = FALSE)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Steam Game Store Dashboard")

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Plots", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("DataTable", icon = icon("table"), tabName = "table"),
    
    # Select variable for color -----------------------------------
    radioButtons(inputId = "z",
                 label = "Color By: ",
                 choices = c("platforms","genres"),
                 selected = "genres"),
    
    # Select Game Genre
    x <- Filter(Negate(is.null), unique(steam$genres)),
    checkboxGroupInput(inputId = "selected_genre",
                       label = "Select genre(s):",
                       choices = x,
                       selected = c("Action","Indie","Strategy")),
    
    #Slider Input for year 
    sliderInput("yearSelect",
                "Release Year:",
                min = min(steam$release_year, na.rm = T),
                max = max(steam$release_year, na.rm = T),
                value = c(min(steam$release_year, na.rm = T), max(steam$release_year, na.rm = T)),
                step = 1),
    
    # Select sample size 
    numericInput(inputId = "n_samp", 
                 label = "Sample size:", 
                 min = 1, max = nrow(steam), 
                 value = 500),
    
    downloadButton('downloadData', 'Download data')
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("price"),
            valueBoxOutput("playtime"),
            valueBoxOutput("owner")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Game Price",
                            plotOutput(outputId = "scatterplot")),
                   tabPanel("Game PlayTime",
                            plotOutput(outputId = "boxplot")),
                   tabPanel("Estimated Owner",
                            plotOutput(outputId = "barchart")))
          )
  ),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Steam Game Store Data", DT::dataTableOutput(outputId = "steamtable"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body,skin = "purple")

# Define server function required to create the scatterplot ---------
server <- function(input, output) {
  
  # Create a subset of data filtering for selected game characters
  steam_subset <- reactive({
    req(input$selected_genre) # ensure availablity of value before proceeding
    # Slider Filter ----------------------------------------------
    filter(steam, release_year >= input$yearSelect[1] & release_year <= input$yearSelect[2] &
             genres %in% input$selected_genre)
  })
  
  # Create new df that is n_samp obs from seoected game characters
  steam_sample <- reactive({ 
    req(input$n_samp) # ensure availablity of value before proceeding
    sample_n(steam_subset(), input$n_samp)
  })
  
  
  # Scatter plot with trend line
  output$scatterplot <- renderPlot({
    dat <- steam_sample()
    ggplot(data = dat, aes_string(x = dat$release_year, y = dat$price, color = input$z)) +
      geom_point()+
      #add trend line 
      geom_smooth(method=lm, se=FALSE)+ 
        labs(x = "Date",
             y = "Price",
             title = "Game Price by Year"
        )
  })
  
  #box chart
  output$boxplot <- renderPlot({
    dat <- steam_sample()
    ggplot(data = dat, aes_string(x = input$z, y=dat$average_playtime, color=input$z)) + 
      geom_boxplot()+labs(title = paste("Boxplot: ","Average Playtime"," By ",input$z) )
  }
  )
  
  #Bar plot 
  output$barchart <- renderPlot({
    dat <- steam_sample()
    ggplot(data=dat,aes_string(x = input$z, y=dat$est_owners,fill=input$z)) +
      geom_bar(stat="identity")+labs(title = paste("Bar Chart: Average Estimated Owner By", input$z))
    
  })
  
  
  # Print data table if checked -------------------------------------
  output$steamtable <- DT::renderDataTable({
    DT::datatable(data = steam_sample(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  }
    
  )

  
  
  # Max Price info box ----------------------------------------------
  output$price <- renderInfoBox({
    steamin <- steam_sample()
    num <- round(max(steamin$price, na.rm = T), 2)
    
    infoBox("Max Price", value = num, subtitle = paste(nrow(steamin), "characters"), 
            icon = icon("balance-scale"), color = "yellow")
  })
  
  # Playtime mean info box ----------------------------------------------
  output$playtime <- renderValueBox({
    steamin <- steam_sample()
    num <- round(mean(steamin$average_playtime, na.rm = T), 2)
    
    valueBox(subtitle = "Avg Playtime", value = num, icon = icon("sort-numeric-asc"), color = "black")
  })
  
  # Number of owner mean info box ----------------------------------------------
  output$owner <- renderValueBox({
    steamin <- steam_sample()
    num <- round(mean(steamin$est_owners, na.rm = T), 2)
    
    valueBox(subtitle = "Avg No.Owners", value = num, icon = icon("sort-numeric-asc"), color = "light-blue")
  })
  
  
  
  # download data when button is clicked
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("steamdatasample",gsub(":","-",Sys.time()), ".csv", sep="")
    },
    content = function(file) {
      write.csv(steam_sample(), file, row.names = FALSE)
    })
  
  
  
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)

