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
        font-size: 25px;
      }
    '))),
    tabItems(
      # First tab content
      tabItem(tabName = "exploratory",
              fluidRow(
                column(4,
                       box(
                         
                         tags$div(title = "Select a country", selectInput("country", "Country:",
                                     choices = names(country_list))),
                         #bsTooltip(id = "country", title = "Select a country", 
                        #           placement = "right", trigger = "hover"),
                         
                         tags$div(title = "Display moving average of continuous variables",
                                  checkboxInput("smoothed", "Smoothed", TRUE)),
                         
                        tags$div(title = "Add lead to the number of new cases",
                                 textInput("lead", "Lead:", value = "0", width = NULL, placeholder = NULL)),
                         
                        tags$div(title = "Set the date interval",
                                 sliderInput("dateinterval", "Date Range",
                                     min = min(country_list[[1]]$date),
                                     max = max(country_list[[1]]$date),
                                     value = c(min(country_list[[1]]$date), max(country_list[[1]]$date)))
                         ),
                         
                         tags$div(title = "Percentage of respondents that have reported use mask cover",
                                  checkboxInput("mc", "Mask Coverage", FALSE)),
                         tags$div(title = "Percentage of respondents that have reported had direct contact (longer than one minute) with people not staying with them in last 24 hours",
                                  checkboxInput("dc", "Direct Contact", FALSE)),
                         tags$div(title = "People vaccinated per 100 people in the total population of the country",
                                  checkboxInput("vacc", "Vaccination", FALSE)),
                         tags$div(title = "Average daily temperatures measured in the capital",
                                  checkboxInput("tavg", "Average daily temperature", FALSE)), width = 12, height = 480
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
                  #checkboxGroupInput("restriction", "Restriction Measures:",
                  #                   choices = rest_names, selected = NULL, inline = TRUE),
                  uiOutput("checkbox_group"), width = 12
                )
              )
      ),
      
      #Pdp tab content
      tabItem(tabName = "pdp",
              #h2("Partial Dependence Plots - ToDo")),
              fluidRow(
                column(4,
                       box(
                         
                         selectInput("country_pdp", "Country:",
                                     choices = names(pdp_all_c_all_pred)),
                         bsTooltip(id = "country_pdp", title = "Select a country", 
                                   placement = "left", trigger = "hover"),
                       ),
                       box(
                         
                         selectInput("predictor_pdp", "Predictor:",
                                     choices = NULL), #to be updated by each session based on values of the country_pdp
                         bsTooltip(id = "predictor_pdp", title = "Select a predictor", 
                                   placement = "left", trigger = "hover"),
                       )
                ),
                
                column(8,
                       box(title = textOutput("charttitle_pdp"), "Variable importance plot",
                           plotOutput("plot_pdp",  height = 350), width = 12, height = 480)
                )
              )),
      # Content Bump Chart
      tabItem(tabName = "bc",
              fluidRow(
                column(7,
                       box(
                         box(title = "Variable Importance Plot", "Ranking of predictors accross countries",
                             htmlOutput("bump_message", height = 20), plotOutput("plot_bc", height = 520),
                             height = 570, width = 12, solidHeader = TRUE),
                         box(
                           actionButton("reset_cy", "Check/Uncheck All Countries"),
                           actionButton("reset_pr", "Check/Uncheck All Predictors"),
                           height = 60, width = 12, solidHeader = TRUE), width = 12, height = 680),
                ),
                column(5,
                       bump_predictor_box(),
                       bump_country_box()
                )
              )
      ),
      # Content Data Source
      tabItem(tabName = "source",
              h1("Datasources- To link!"),
              h2("Facebook COVID-19 Symptom Survey"),
              h2("Temperature, National Centers for Environmental Information"),
              h2("Vaccination, Our Word in Data"),
              h2("COVID-19 Data Repository, Johns Hopkins University"),
              h2("Eurostat databases"),
              h2("Country response measures to COVID-19 by week and by country, European Centre for Disease Prevention and Control")
              
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
  
  output$checkbox_group <- renderUI({
    checkboxgroup_with_label(sel_rest_country[[input$country]], res_label_country[[input$country]]$res_text, res_label_country[[input$country]]$res_label)
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
    #updateCheckboxGroupInput(session, inputId = "restriction", selected = NULL, inline = TRUE)
    #updateCheckboxGroupInput(session, inputId = "restriction", choices = sel_rest_country[[input$country]],  inline = TRUE)
    update_checkboxgroup_with_label(session, sel_rest_country[[input$country]], res_label_country[[input$country]]$res_text)
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
    #rest <- input$restriction
    rest <- get_input_checkboxgroup_with_label(sel_rest_country[[input$country]], input)
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
  
  
  ## Pdp
  
  observeEvent(input$country_pdp,{
    updateSelectInput(session,'predictor_pdp',
                      choices=names(pdp_all_c_all_pred[[input$country_pdp]]))
  })
  
  # Dynamic name for the pdp chart
  titleText_pdp <- reactive({
    paste(input$country_pdp)
  })
  
  output$charttitle_pdp <- renderText({
    titleText_pdp()
  })
  
  selectedData <- reactive({
    pdp_all_c_all_pred[[input$country_pdp]][[input$predictor_pdp]]
  })
  
  output$plot_pdp <-renderPlot({
    
    ggplot(selectedData()) +
      geom_line(aes(x = pdp_all_c_all_pred[[input$country_pdp]][[input$predictor_pdp]][,1],
                    y = pdp_all_c_all_pred[[input$country_pdp]][[input$predictor_pdp]][,2]), size = 0.8) +
      xlab(input$predictor_pdp) +
      ylab("yhat") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=60, hjust=1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = unit(c(1,1,0,1), "lines")) 
  })
  
  
  ## Reactive input Bump Chart
  
  # Action Buttons
  observeEvent(input$reset_cy,{
    countr <- c(input$bc_country, input$bc_country2, input$bc_country3, input$bc_country4, input$bc_country5, input$bc_country6)
    reset_country(session, countr)
  })
  
  observeEvent(input$reset_pr,{
    pred <- c(input$bc_pred, input$bc_pred2)
    reset_predictor(session, pred)
  })
  
  bcData <- reactive({
    pred <- c(input$bc_pred, input$bc_pred2)
    countr <- c(input$bc_country, input$bc_country2, input$bc_country3, input$bc_country4, input$bc_country5, input$bc_country6)
    bc_dat <- NULL
    if(length(pred) == 0){
      bc_dat <- "no_pred"
    }
    if(length(countr) == 0){
      bc_dat <- c(bc_dat, "no_countr")
    }
    if(length(pred) > 0 & length(countr) > 0){
      bc_dat <- b_vis_long[which(b_vis_long$predictor %in% pred & b_vis_long$country %in% countr),]
    }
    bc_dat
  })
  
  ## Plots
  
  # Exploratory
  output$plot1 <- renderPlot({
    p <- exp_plot_base(plotData(), yLimit()[1], yLimit()[2], input$mc, input$dc, input$vacc)
    p <- exp_plot_add_temp(p, input$tavg, plotData())
    p <- exp_plot_add_fb_vacc(p, input$mc, input$dc, input$vacc, plotData())
    exp_display_plot(p, get_input_checkboxgroup_with_label(sel_rest_country[[input$country]], input), plotRest(), plotData(), restrictionPlotLabels(), input$mc, input$dc, input$tavg, input$vacc)
  })
  
  # Bump Chart
  bumpChart <- reactive({
    x_coord <- bc_pred_label(bcData())
    bump_chart(bcData(), x_coord)
  })
  output$plot_bc <- renderPlot({
    p <- bumpChart()
    if(is.ggplot(p)) p
  })
  output$bump_message <- renderText({
    p <- bumpChart()
    if(!is.ggplot(p)) p
  })
}

shinyApp(ui, server)
