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
path <- here("data", "dat_new.csv")
dat <- read_csv(path)

ui <- tagList(
    navbarPage(
      theme = shinythemes::shinytheme("yeti"),
      "Guana System Water Quality",
      tabPanel("Individual Sites",
               sidebarPanel(
                 # select site names
                 selectInput(inputId = "site", label = strong("Select a site:"),
                             choices = unique(dat$site),
                             selected = unique(dat$site)[1]
                 ),
                 # select parameter
                 selectInput(inputId = "parameter", label = strong("Select a parameter:"),
                             choices = unique(dat$component_long),
                             selected = unique(dat$component_long)[1]
                 ),
                 # select date
                 dateRangeInput(inputId = "date", label = strong("Select a date range:"),
                                start = min(dat$date_sampled),
                                end = max(dat$date_sampled)
                 ),
                 # add threshold
                 numericInput(inputId = "threshold", label = strong("Enter threshold values:"),
                              value = 0)
               ),

               mainPanel(
                 tabsetPanel(
                   tabPanel("Plots",
                            plotlyOutput(outputId = "plotlyscatter")
                   ),
                   tabPanel("Tab 2", "Probably going to be summary tables"),
                   tabPanel("Tab 3", "This panel is a work in progress!")
                 )
               )
      ),
      tabPanel("Navbar 2", "This panel is a work in progress!"),
      tabPanel("Navbar 3", "This panel is a work in progress!")
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
             date_sampled >= as.Date(input$date[1]),
             date_sampled <= as.Date(input$date[2]),
             component_long == input$parameter)
  })

  # create plotly object
  output$plotlyscatter <- renderPlotly({

    req(input$site)
    req(input$parameter)
    req(input$date)
    req(input$threshold)

    # create base plot
    p <- ggplot(dat2()) +
      geom_point(aes(x = date_sampled, y = result)) +
      geom_hline(yintercept = input$threshold, linetype='longdash', color = 'gray18', size = 1.5)+
      labs(title = paste(input$parameter, "at", input$site), x = "Date", y = input$parameter) +
      theme_classic()
  })
}

shinyApp(ui = ui, server = server)
