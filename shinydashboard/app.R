# Content and functionality of the shiny dashboard application.
library(shinydashboard)
library(tidyverse)
library(RColorBrewer)
library(gridExtra)
library(lubridate)
library(reshape2)
library(shinyBS)
library(maps)
library(akima)
library(plotly)
library(shinybusy)

library(shiny)


source("Shiny_prep_and_functions.R")
source("Shiny_vis_functions.R")

##
# Shiny
#Path to User's Manual for the Documentation Tab
addResourcePath(prefix = 'pdfs', directoryPath = 'www/')


ui <- dashboardPage(
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar(
    collapsed = FALSE,
    sidebarMenu(
      menuItem("Exploratory", tabName = "exploratory", icon = icon("object-align-bottom", lib = "glyphicon")),
      menuItem("Partial Dependence", tabName = "pdp", icon=icon("object-align-bottom", lib = "glyphicon")),
      menuItem("3D Partial Dependence", tabName = "3d_pdp", icon=icon("object-align-bottom", lib = "glyphicon")),
      menuItem("Bump Chart", tabName = "bc", icon=icon("object-align-bottom", lib = "glyphicon")),
      menuItem("Country Characteristics", tabName = "rankcorr", icon=icon("object-align-bottom", lib = "glyphicon")),
      menuItem("Documentation", tabName = "source", icon = icon("th"))
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
                       box(title = textOutput("charttitle"), htmlOutput("chartsubtitle"),
                           plotOutput("plot1",  height = 350), width = 12, height = 480)
                )
              ),
              fluidRow(
                  #checkboxGroupInput("restriction", "Restriction Measures:",
                  #                   choices = rest_names, selected = NULL, inline = TRUE),
                  uiOutput("checkbox_group"), width = 12
              )
      ),
                
      tabItem(tabName = "pdp",
              fluidRow(
                 box(
                   column(6,
                     tags$div(title = "Select a country", selectInput("country_pdp", "Country:",
                                                                      choices = names(pdp_all_c_all_pred)))),
                   column(6,
                     tags$div(title = "Select a predictor", selectInput("predictor_pdp", "Predictor:", choices = NULL))),
                     width = 12),
                
                
                box(title = textOutput("charttitle_pdp"), "Partial Dependence Plot",
                           plotOutput("plot_pdp",  height = 450), width = 12, height = 580)
              )),
      
     # 3D PDP
     tabItem(tabName = "3d_pdp",
             fluidRow(
               box(
               column(4,
                      tags$div(title = "Select a country", selectInput("country_3d_pdp", "Country:",
                                                                       choices = names(pdp_all_c_all_pred)))),
               column(4,
                      tags$div(title = "Select a predictor for the X axis", selectInput("predictor_3d_pdp_1", "Predictor X:",
                                                                                        choices = NULL, selected = NULL))),
               column(4,
                      tags$div(title = "Select a predictor for the Y axis", selectInput("predictor_3d_pdp_2", "Predictor Y:",
                                                                                        choices = NULL, selected = NULL))),
               width = 12),
               
               
               box("3D Partial Dependence Plot", htmlOutput("diff_error"),
                   plotlyOutput("plot_3d_pdp",  height = 450), width = 12, height = 580)
             )),
      # Content Bump Chart
      tabItem(tabName = "bc",
              fluidRow(
                column(9,
                       box(
                         box(title = "Variable Importance Plot", "Ranking of predictors accross countries",
                             htmlOutput("bump_message", height = 20), plotOutput("plot_bc", height = 310),
                             width = 12, solidHeader = TRUE, height = 360),
                         box(
                           actionButton("reset_cy", "Check/Uncheck All Countries"),
                           actionButton("reset_pr", "Check/Uncheck All Predictors"),
                           height = 60, width = 12, solidHeader = TRUE), width = 12),
                       box(title = "Countries", uiOutput("checkbox_group_bc_country"), width = 12)
                ),
                column(3,
                       box(title = "Predictors", uiOutput("checkbox_group_bc"), width = 12, height = 750)
                       #box(title = "Countries", uiOutput("checkbox_group_bc_country"), width = 12)
                )
              ),
              #fluidRow(box(title = "Countries", uiOutput("checkbox_group_bc_country"), width = 12))
      ),
      # Content Variable Importance Rank Correlation Within Clusters
      tabItem(tabName = "rankcorr",
              fluidRow(
                box(title = "To what extent are the importance of predictors for new COVID infections determined by country characteristics?",
                    column(6,
                           p("Please select a cluster to check if its predictors for the number of new COVID cases are typical 
                             for countries with similar characteristics. 
                             You can see the strength of the rank correlation of the variable importances of the countries and the selected cluster 
                             based on country-level sociodemographic, medical factors and cultural participation below."),
                            p("Please check the Documentation Tab for further details."),
                           ),
                    column(6,
                           plotOutput("rank_eu", height = 200)
                           ), width = 12, height = 310),
                    box(
                      column(
                        tags$div(title = "Select a cluster to see how tipical the predictors for the number of new COVID cases are for the countries within", selectInput("cluste", "Select a cluster",
                                                                                                                                                                    choices = c("All Clusters", 1:7))),
                        tags$div(title = "Select the strength of rank correlation", selectInput("corr_strength", "Select the strength of the correlation",
                                                                                                choices = c("All", "Low", "Middle", "High"))),
                        width = 3),
                      column(
                        plotOutput("rank_cluster"), width = 9
                      ), width = 12), width = 12
                ),
      ),
      
     
      #Content Manual
      tabItem(tabName = "source",
              h1("Datasources"),
              h4(tags$div(tags$a(href = "https://covidmap.umd.edu/", "Facebook/UMD COVID-19 World Symptom Survey"))),
              h4(tags$div(tags$a(href ="https://github.com/RamiKrispin/coronavirus", "Johns Hopkins University CSSE COVID-19 Data Repository"))),
              h4(tags$div(tags$a(href ="https://ourworldindata.org/coronavirus", "Vaccination from Our World in Data"))),
              h4(tags$div(tags$a(href ="https://www.ecdc.europa.eu/en/publications-data/download-data-response-measures-covid-19", 
                     "Country response measures to COVID-19 by week and by country, European Centre for Disease Prevention and Control"))),
              h4(tags$div(tags$a(href ="https://www.ncdc.noaa.gov/", "Temperature, National Centers for Environmental Information"))), 
              h4(tags$div(tags$a(href ="https://ec.europa.eu/eurostat", "Eurostat databases on Population, Health expenditures and Cultural participation"))),
              
              # Documentation tab with linked datasources and downloadable manual
              h1("User's Manual"),
              h4("Please consult the Manual for details on the usage of the COVID-19 app, the methodology behind, the R codes and literaure references."),
              tags$iframe(style="height:400px; width:100%; scrolling=yes", 
                                 src=" pdfs/MDM Master Project - COVID - Manual.pdf")  ## use the prefix defined in addResourcePath
              
              
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
    checkboxgroup_with_label(sel_rest_country[[input$country]], res_label_country[[input$country]]$res_text,
                             res_label_country[[input$country]]$res_label, width = 2, height = 1)
  })
  
  output$checkbox_group_bc <- renderUI({
    checkboxgroup_with_label(bc_labels$predictor, bc_labels$d_text, bc_labels$d_label, width = 12, height = 1)
  })
  
  output$checkbox_group_bc_country <- renderUI({
    checkboxgroup_with_label(bc_country, countries, "", width = 2, height = 1, selected = TRUE)
  })
  
  # Infoboxes: Dynamic number of cases/deaths/recovered/vaccinations
  numberCases <- reactive({
    number_of_cases(input$country, input$dateinterval[1], input$dateinterval[2])
  })  
  
  # Subtitle
  subtitleText <- reactive({ 
    exp_subtitle(input$lead, plotData(), input$mc, input$dc, input$vacc, input$tavg)
  })
  output$chartsubtitle <- renderText({
    subtitleText()
  })
  
  # Applied restrictions per countries
  observeEvent(input$country,{
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
    rest <- get_input_checkboxgroup_with_label(sel_rest_country[[input$country]], input)
    var <- input$country
    
    # Creating the coordinates for the rectangles displaying the restrictions
    rest_coord <- get_coord_for_country(country = var, rest_list = rest, max_y = yLimit()[2])
    
    # X coordinates for selected restrictions measures are appropriate
    if(!is.Date(rest_coord$x_min) | !is.Date(rest_coord$x_max) | length(which(is.na(rest_coord$x_min))) > 0 | length(which(is.na(rest_coord$x_max))) > 0){
      cat("Error in get_coord_for_country (Shiny_prep_and_functions) - X coordinates are not date or missing for the selected restriction measure")
    }
    
    rest_coord <- rest_coord[which(!is.na(rest_coord$x_min)),]
    
    rest_coord
    
  })
  
  restrictionPlotLabels <- reactive({
    # Restrictions
    rest_plot <- unique(plotRest()[,c("restriction", "y_min", "y_max")])
    rest_plot$y <- rest_plot$y_min + ((rest_plot$y_max - rest_plot$y_min)/2)
    if("restriction" %in% colnames(rest_plot) & "res_id" %in% colnames(res_label_country[[input$country]])){
      rest_plot <- merge(rest_plot, res_label_country[[input$country]][,c("res_id", "res_text")], by.x = "restriction", by.y = "res_id", all.x = TRUE)
      rest_plot <- rest_plot[,-1]
      colnames(rest_plot)[4] <- "restriction"
      rest_plot
    }
  })
  
  
  ## Pdp
  
  observeEvent(input$country_pdp,{
  act_choices <- all_pred_table[which(all_pred_table$pred_id %in% names(pdp_all_c_all_pred[[input$country_pdp]])), "pred_text"]
    # Ordering predictors: average daily temperature, fb variables, restriction measures
    first_vars <- c("Average Daily Temperature", "COVID-like Illnes", "Mask Coverage")
    rest_choices <- act_choices[-which(act_choices %in% first_vars)]
    act_choices <- c(first_vars, rest_choices[order(rest_choices)])
    updateSelectInput(session,'predictor_pdp',
                      choices=act_choices)
  })
  
  # Dynamic name for the pdp chart
  titleText_pdp <- reactive({
    paste(input$country_pdp)
  })
  
  output$charttitle_pdp <- renderText({
    titleText_pdp()
  })
  
  selectedPredictorpdp <- reactive({
    all_pred_table[all_pred_table$pred_text == input$predictor_pdp, "pred_id"]
  })
  
  selectedData <- reactive({
    pdp_all_c_all_pred[[input$country_pdp]][[selectedPredictorpdp()]]
  })
  
  #get standardised predictor distribution for the rug
  rf_dat_rug <- rf_dat_fb[complete.cases(rf_dat_fb),]
  c_rf_dat_rug <- split(rf_dat_rug, rf_dat_rug$country)
  
  selectedRug <- reactive({
    c_rf_dat_rug[[input$country_pdp]]
  })
  
  output$plot_pdp <-renderPlot({
    
    act_dat <- selectedData()
    act_pred <- selectedPredictorpdp()
    act_rug <- selectedRug()
    act_country <- input$country_pdp
    
    # Labels on x axis
    act_min_max <- x_min_max[which(x_min_max$country == act_country & x_min_max$predictor == act_pred),]
    if(length(unique(act_dat[,1])) <= 2){
      x_range = c(min(act_dat[,1]), max(act_dat[,1]))
      x_vals = c(act_min_max$min, act_min_max$max)
    }else{
      x_range = c(min(act_dat[,1]), min(act_dat[,1]) + ((max(act_dat[,1]) - min(act_dat[,1]))/3),
                  min(act_dat[,1]) + (2*(max(act_dat[,1]) - min(act_dat[,1]))/3),  max(act_dat[,1]))
      x_vals = round(c(act_min_max$min, act_min_max$min + (act_min_max$max - act_min_max$min)/3,
                 act_min_max$min + 2* (act_min_max$max - act_min_max$min)/3, act_min_max$max), 2)
    }
    cat(x_range)
    cat(x_vals)
    
    ggplot(act_dat) +
      
      geom_line(aes(x = act_dat[,1],
                    y = act_dat[,2],
                    color = "darkred") , size = 0.8) +
      xlab(input$predictor_pdp) +
      #xlab(act_pred) +
      ylab("% of new cases") +
      geom_rug(data = act_rug, aes(x = act_rug[,act_pred], inherit.aes = F, alpha = 0.3, color="darkred")) + 
      theme_minimal() +
      scale_x_continuous(breaks = x_range,labels=x_vals) +
      theme(axis.text.x=element_text(angle=60, hjust=1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none",
            plot.margin = unit(c(1,1,0,1), "lines")
            ) 
  })
  
  ## 3D PDP
  output$diff_error <- renderText({
    ""
  })
  
  observeEvent(input$country_3d_pdp,{
    all_choices <- all_pred_table[which(all_pred_table$pred_id %in% names(pdp_all_c_all_pred[[input$country_3d_pdp]])), "pred_text"]
    # Ordering predictors: average daily temperature, fb variables, restriction measures
    first_vars <- c("Average Daily Temperature", "COVID-like Illnes", "Mask Coverage", "People Fully Vaccinated Per Hundred")
    rest_choices <- all_choices[-which(all_choices %in% first_vars)]
    act_choices <- c(first_vars, rest_choices[order(rest_choices)])
    updateSelectInput(session,'predictor_3d_pdp_1',
                      choices = act_choices)
    updateSelectInput(session,'predictor_3d_pdp_2',
                      choices = act_choices)
    output$diff_error <- renderText({
      ""
    })
  })
  
  observeEvent(input$predictor_3d_pdp_1,{
    if(input$predictor_3d_pdp_1 == input$predictor_3d_pdp_1)
    {
      output$diff_error <- renderText({
        "Please select two different predictors for X and Y axis"
      })
    }else{
      output$diff_error <- renderText({
        ""
      })
    }
  })
  
  observeEvent(input$predictor_3d_pdp_2,{
    if(input$predictor_3d_pdp_2 == input$predictor_3d_pdp_1)
    {
      output$diff_error <- renderText({
        "Please select two different predictors for X and Y axis"
      })
    }else{
      output$diff_error <- renderText({
        ""
      })
    }
  })
  
  
  selectedPredictor3dpdp_1 <- reactive({
    all_pred_table[all_pred_table$pred_text == input$predictor_3d_pdp_1, "pred_id"]
  })
  
  selectedPredictor3dpdp_2 <- reactive({
    all_pred_table[all_pred_table$pred_text == input$predictor_3d_pdp_2, "pred_id"]
  })
  
  output$plot_3d_pdp <-renderPlotly({
    
    act_country <- input$country_3d_pdp
    
    cat("Lets start with 3d")
    
    if(selectedPredictor3dpdp_1() != selectedPredictor3dpdp_2()){
      
      output$diff_error <- renderText({
        ""
      })
      
      show_modal_spinner()
      
      predx <- selectedPredictor3dpdp_1()
      predy <- selectedPredictor3dpdp_2()
      object <- pdp_pred_paired(rf_train[[act_country]], predx, predy)
      
      cat("Object calculated")
      
      dens <- akima::interp(y = object[,predx], x = object[,predy], z = object$yhat)
      
      p3 <- plot_ly(x = dens$x, 
                    y = dens$y, 
                    z = dens$z,
                    type = "surface")
      
      p3 <- p3 %>% layout(scene = list(xaxis = list(title = "X"),
                                       yaxis = list(title = "Y"),
                                       zaxis = list(title = "Partial Dependence")))
      
      
      remove_modal_spinner() 
      
      p3
      
    }

  })

  
  ## Reactive input Bump Chart
  
  # Action Buttons
  observeEvent(input$reset_cy,{
    countr <- get_input_checkboxgroup_with_label(bc_country, input)
    check_uncheck_checkboxgroup_with_label(session, bc_country, countries, countr)
  })
  
  observeEvent(input$reset_pr,{
    pred <- get_input_checkboxgroup_with_label(bc_labels$predictor, input)
    check_uncheck_checkboxgroup_with_label(session, bc_labels$predictor, bc_labels$d_text, pred)
  })
  
  bcData <- reactive({
    pred <- get_input_checkboxgroup_with_label(bc_labels$predictor, input)
    pred <- str_remove_all(pred, "^bc_")
    countr <- get_input_checkboxgroup_with_label(bc_country, input)
    countr <- str_remove_all(countr, "^bc_")
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
  
  # Downloadable manual
  
  
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
  
  # Rank correlation
  # eu 
  map
  output$rank_eu <- renderPlot({
    p <- eu_cluster_vis()
    p
  }, height = 250)
  
  # map of clusters
  output$rank_cluster <- renderPlot({
    p <- cluster_varimp_corr_vis(input$cluste, input$corr_strength)
    p
  }, height = 370)
  
}

shinyApp(ui, server)
