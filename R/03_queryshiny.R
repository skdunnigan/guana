library(shiny)
library(shinythemes)

# load packages
library(tidyverse)
library(lubridate)
library(knitr)
library(gridExtra)
library(scales)
library(plotly)
library(psych)
library(readxl)
library(rmarkdown)
library(janitor)

# load data
dat_nut <- read_csv("dat_nut.csv")

shinyApp(
  ui = tagList(
    navbarPage(
      theme = shinythemes::shinytheme("yeti"),
      "Guana System Water Quality",
      tabPanel("Individual Sites",
               sidebarPanel(
                 # select stations
                 selectInput(inputId = "site", label = strong("Site:"),
                             choices = unique(dat_nut$site),
                             selected = unique(dat_nut$site)[1]
                 ),

                 selectInput(inputId = "parameter", label = strong("Variable:"),
                             c("Chlorophyll a" = "CHLa_UnC_1",
                               "Total Phosphorus" = "TP1",
                               "Total Nitrogen" = "TN1")),

                 textInput("txt", "Text input:", "general"),

                 sliderInput("slider", "Slider input:", 1, 100, 30),

                 tags$h5("Deafult actionButton:"),

                 actionButton("action", "Search"),

                 tags$h5("actionButton with CSS class:"),

                 actionButton("action2", "Action button", class = "btn-primary")
               ),


               mainPanel(
                 tabsetPanel(
                   tabPanel("Tab 1",
                            h4("Table"),
                            tableOutput("table"),
                            h4("Verbatim text output"),
                            verbatimTextOutput("txtout"),
                            h1("Header 1"),
                            h2("Header 2"),
                            h3("Header 3"),
                            h4("Header 4"),
                            h5("Header 5")
                   ),
                   tabPanel("Tab 2", "Probably going to be summary tables"),
                   tabPanel("Tab 3", "This panel is a work in progress!")
                 )
               )
      ),


      tabPanel("Navbar 2", "This panel is a work in progress!"),


      tabPanel("Navbar 3", "This panel is a work in progress!")
    )
  ),
  server = function(input, output) {
    output$txtout <- renderText({
      paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$table <- renderTable({
      head(cars, 4)
    })
  }
)
# shinyApp(ui = ui, server = server)
