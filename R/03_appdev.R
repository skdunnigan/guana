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
library(here)

# load data
setwd('/cloud/project/')
dat <- read_csv("data/dat_new.csv")

ui <- fluidPage(
  sidebarPanel(
    # select site names
    selectInput(inputId = "site", label = strong("Site:"),
                 choices = unique(dat$site),
                 selected = unique(dat$site)[1]
    ),
    # select parameter
    selectInput(inputId = "parameter", label = strong("Parameter:"),
                choices = unique(dat$component_long),
                selected = unique(dat$component_long)[1]
    ),
    # select date
    dateRangeInput("date", strong("Date range"),
                   start = min(dat$date_sampled),
                   end = max(dat$date_sampled))
  ),

  mainPanel(
    plotlyOutput(outputId = "plotlyscatter")
  )
)

server <- function(input, output) {
  # subset data reactively
  dat2 <- reactive({
    req(input$site)
    req(input$parameter)
    req(input$date)
    dat %>%
      filter(site == input$site,
             date >= as.Date(input$date[1]),
             date <= as.Date(input$date[2]),
             parameter == input$parameter)
  })

  # create plotly object
  output$plotlyscatter <- renderPlotly({

    req(input$site)
    req(input$parameter)
    req(input$date)

    # create base plot
    p <- ggplot(dat2()) +
      geom_point(aes(x = date_sampled, y = result)) +
      labs(title = paste(input$parameter, " at ", input$site), x = "Date", y = input$parameter) +
      theme_classic()
  })
}



shinyApp(ui = ui, server = server)
