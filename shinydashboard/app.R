# Content and functionality of the shiny dashboard application.

library(shinydashboard)
library(tidyverse)
library(RColorBrewer)
library(gridExtra)
library(lubridate)
library(reshape2)
library(shinyBS)

source("Shiny_prep_and_functions.R")
source("Shiny_vis_functions.R")

##
# Shiny

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Exploratory", tabName = "exploratory", icon = icon("object-align-bottom", lib = "glyphicon")),
      menuItem("Partial Dependence", tabName = "pdp", icon=icon("object-align-bottom", lib = "glyphicon")),
      menuItem("Bump Chart", tabName = "bc", icon=icon("object-align-bottom", lib = "glyphicon")),
      menuItem("Datasource", tabName = "source", icon = icon("th"))
    )
  ), skin = "black",
  
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Arial", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
    tabItems(
      # First tab content
      tabItem(tabName = "exploratory",
              fluidRow(
                column(4,
                       box(
                         
                         selectInput("country", "Country:",
                                     choices = names(country_list)),
                         bsTooltip(id = "country", title = "Select a country", 
                                   placement = "left", trigger = "hover"),
                         
                         checkboxInput("smoothed", "Smoothed", TRUE),
                         
                         textInput("lead", "Lead:", value = "0", width = NULL, placeholder = NULL),
                         
                         sliderInput("dateinterval", "Date Range",
                                     min = min(country_list[[1]]$date),
                                     max = max(country_list[[1]]$date),
                                     value = c(min(country_list[[1]]$date), max(country_list[[1]]$date))
                         ),
                         
                         checkboxInput("mc", "Mask Coverage", FALSE),
                         checkboxInput("dc", "Direct Contact", FALSE),
                         checkboxInput("vacc", "Vaccination", FALSE),
                         checkboxInput("tavg", "Average daily temperature", FALSE), width = 12, height = 480
                       )
                ),
                
                column(8,
                       #     valueBoxOutput("casesBox", width = 3),
                       #     valueBoxOutput("deathBox", width = 3),
                       #     valueBoxOutput("recovBox", width = 3),
                       #     valueBoxOutput("vaccinBox", width = 3),
                       box(title = textOutput("charttitle"), htmlOutput("chartsubtitle"),
                           plotOutput("plot1",  height = 350), width = 12, height = 480)
                )
              ),
              fluidRow(
                box(
                  checkboxGroupInput("restriction", "Restriction Measures:",
                                     choices = rest_names, selected = NULL, inline = TRUE), width = 12
                  #  prettyCheckboxGroup("restriction", "Restriction Measures:",
                  #                    choices = rest_names, inline = TRUE, shape = "curve"), width = 12
                )
              )
      ),
      
      #Second tab content
      tabItem(tabName = "pdp",
              h2("Partial Dependence Plots - ToDo")),
      
      # Content Bump Chart
      tabItem(tabName = "bc",
              box(plotOutput("plot_bc",  height = 350), width = 12, height = 480),
              box(
                checkboxGroupInput("bc_pred", "Predictors:",
                                   choices = pred_order[,1], selected = NULL, inline = TRUE), width = 12
              )
      ),
      
      # Content Data Source
      tabItem(tabName = "source",
              h2("Datasource - ToDO")
      
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Dynamic name for the chart
  titleText <- reactive({
    paste(input$country)
  })
  
  output$charttitle <- renderText({
    titleText()
  })
  
  # Infoboxes: Dynamic number of cases/deaths/recovered/vaccinations
  numberCases <- reactive({
    number_of_cases(input$country, input$dateinterval[1], input$dateinterval[2])
  })
  output$casesBox <- renderValueBox({ valueBox(tags$p(numberCases()[1], style = "font-size: 60%;"), tags$p("Infections", style = "font-size: 90%;")) })
  output$deathBox <- renderValueBox({ valueBox(tags$p(numberCases()[2], style = "font-size: 60%;"), tags$p("Deaths", style = "font-size: 90%;")) })
  output$recovBox <- renderValueBox({ valueBox(tags$p(numberCases()[3], style = "font-size: 60%;"), tags$p("Recovered", style = "font-size: 90%;")) })
  output$vaccinBox <- renderValueBox({ valueBox(tags$p(numberCases()[4], style = "font-size: 60%;"), tags$p("Vaccinations", style = "font-size: 90%;")) })
  
  
  # Subtitle
  subtitleText <- reactive({ 
    exp_subtitle(input$lead, plotData(), input$mc, input$dc, input$vacc, input$tavg)
  })
  output$chartsubtitle <- renderText({
    subtitleText()
  })
  
  # Applied restrictions per countries
  observeEvent(input$country,{
    updateCheckboxGroupInput(session, inputId = "restriction", selected = NULL, inline = TRUE)
    updateCheckboxGroupInput(session, inputId = "restriction", choices = sel_rest_country[[input$country]],  inline = TRUE)
  })
  
  # Data preparing from input
  plotData <- reactive({ 
    var <- input$country
    smoothed <- input$smoothed
    min_date <- input$dateinterval[1]
    max_date <- input$dateinterval[2]
    df <-  cbind(smooth_or_not(smoothed, var, min_date, max_date),
                 fb_smooth_or_not(smoothed, var, min_date, max_date, yLimit()[2]),
                 "vaccination" = vacc_smooth_or_not(smoothed, var, min_date, max_date, yLimit()[2])[,2])
    
    # Lead for the infections
    if(!is.na(input$lead))
    {
      lead_bad <- is.na(as.numeric(input$lead))
      if(!lead_bad){
        input_lead <- as.numeric(input$lead)
        if(!is.na(input$lead) & input$lead > 0 & input_lead < nrow(df)){
          df <- add_lead(dat = df, lead_to_add = input_lead)
        }
      }
    }
    
    df
  })
  
  yLimit <- reactive({
    var <- input$country
    smoothed <- input$smoothed
    temp <- input$tavg
    y_limits(y_limit_list[[var]], smoothed, temp)
  })
  
  plotRest <- reactive({
    rest <- input$restriction
    var <- input$country
    
    # Creating the coordinates for the rectangles displaying the restrictions
    rest_coord <- get_coord_for_country(country = var, rest_list = rest, max_y = yLimit()[2])
    rest_coord <- rest_coord[which(!is.na(rest_coord$x_min)),]
    
    rest_coord
    
  })
  
  restrictionPlotLabels <- reactive({
    # Restrictions
    rest_plot <- unique(plotRest()[,c("restriction", "y_min", "y_max")])
    rest_plot$y <- rest_plot$y_min + ((rest_plot$y_max - rest_plot$y_min)/2)
    rest_plot
  })
  
  # Reactive input Bump Chart
  bcData <- reactive({
    pred <- input$bc_pred
    bc_dat <- b_vis_long[which(b_vis_long$predictor %in% pred),]
    bc_dat
  })
  
  output$plot1 <- renderPlot({
    p <- exp_plot_base(plotData(), yLimit()[1], yLimit()[2], input$mc, input$dc, input$vacc)
    p <- exp_plot_add_temp(p, input$tavg, plotData())
    p <- exp_plot_add_fb_vacc(p, input$mc, input$dc, input$vacc, plotData())
    exp_display_plot(p, input$restriction, plotRest(), plotData(), restrictionPlotLabels(), input$mc, input$dc, input$tavg, input$vacc)
  })
  
  output$plot_bc <- renderPlot({
    bump_chart(bcData())
    })
}

shinyApp(ui, server)
