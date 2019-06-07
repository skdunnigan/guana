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
dat <- read_csv("dat_nut.csv")

shinyApp(
  ui = tagList(
    navbarPage(
      theme = shinythemes::shinytheme("yeti"),
      "Guana System Water Quality",

      tabPanel("Individual Sites",

               sidebarPanel(

                 # select stations
                 selectInput(inputId = "site", label = strong("Site:"),
                             choices = unique(dat$site),
                             selected = unique(dat$site)[1]
                 ),

                 # select parameters
                 selectInput(inputId = "parameter", label = strong("Variable:"),
                             c("Chlorophyll a" = "CHLa_UnC_1",
                               "Total Phosphorus" = "TP1",
                               "Total Nitrogen" = "TN1")
                 ),

                 # select date range to plot
                 dateRangeInput("date", strong("Date range"),
                                start = min(dat_nut$date_sampled), end = max(dat_nut$date_sampled)
                 ),


               mainPanel(
                 tabsetPanel(
                   tabPanel("Plots",
                            plotlyOutput(outputId = "plotlyscatter"),
                   ),
                   tabPanel("Tab 2", "Probably going to be summary tables"),
                   tabPanel("Tab 3", "This panel is a work in progress!")
                 )
               )
               )
      ),


      tabPanel("Navbar 2", "This panel is a work in progress!"),


      tabPanel("Navbar 3", "This panel is a work in progress!")
    )
  ),

server = function(input, output) {

    # subset data, reactively
    dat2 <- reactive({
      req(input$site)
      req(input$date)
      req(input$parameter)
      dat %>%
        filter(site == input$SET,
               date >= as.Date(input$date[1]),
               date <= as.Date(input$date[2]))
    })

    # create plotly object
    output$plotlyscatter <- renderPlotly({

      req(input$site)
      req(input$date)
      req(input$parameter)

      # create the base plot
      p <- ggplot(dat2()) +
        geom_point(aes(x = date, y = parameter)) +
        labs(title = paste(input$parameter, "values collected at", input$site), x = "Date", y = paste(input$parameter)) +
        theme_classic()
    })

  }
)
# shinyApp(ui = ui, server = server)
