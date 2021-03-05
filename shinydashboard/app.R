library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("object-align-bottom", lib = "glyphicon")),
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
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Controls", status = "info",
                  
                  selectInput("country", "Variable:",
                              choices = unique(tdata$country)),
                  
                  sliderInput("dateinterval", "Date Range",
                              min = min(tdata$date),
                              max = max(tdata$date),
                              value = c(min(tdata$date), max(tdata$date))
                  ),
                  
                  checkboxInput("smoothed", "Smoothed", TRUE),
                  
                  textInput("lead", "Lead:", value = "0", width = NULL, placeholder = NULL),
                  
                  checkboxGroupInput("restriction", "Restriction:",
                                     choices = rest_names), width = 4
                ),
                  
                #uiOutput("box_test")
                box(title = textOutput("charttitle"), textOutput("chartsubtitle"), status = "info",
                    plotOutput("plot1"), width = 8)
              )
      ),
      
      # Second tab content
      tabItem(tabName = "source",
              h2("Datasource - ToDO")
      )
    )
  )
)

server <- function(input, output) {
  
  # Dynamic name for the chart
  titleText <- reactive({
    paste(input$country)
  })
  
  output$charttitle <- renderText({
    titleText()
  })
  
  #Subtitle
  subtitleText <- reactive({
    if(!is.na(input$lead) & input$lead != "")
    {
      lead_bad <- is.na(as.numeric(input$lead))
      if(!lead_bad){
        input_lead <- as.numeric(input$lead)
        if(input$lead > 0 & input_lead < nrow(df)){
          paste("Number of infections with", input$lead, "days lead", sep = " ")
        }else if(input$lead < 0){
          paste("Lead has to be positive")
        }else if(input$lead == 0){
          paste("Number of infections")
        }else{
          paste("Lead is too big")
        }
      }else{
        paste("Lead has to be numeric")
      }
    }else{
      paste("Number of infections")
    }
  })
  
  output$chartsubtitle <- renderText({
    subtitleText()
  })
  
  # Data preparing from input
  plotData <- reactive({ 
    var <- input$country
    smoothed <- input$smoothed
    min_date <- input$dateinterval[1]
    max_date <- input$dateinterval[2]
    df <- smooth_or_not(smoothed, var, min_date, max_date)
    
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
    if(smoothed == TRUE){
      max_limit <- y_limit_list[[var]][which(colnames(y_limit_list[[var]]) == "y_limit_smo")]
    }else{
      max_limit <- y_limit_list[[var]][which(colnames(y_limit_list[[var]]) == "y_limit_abs")]
    }
    max_limit
  })
  
  plotRest <- reactive({
    rest <- input$restriction
    var <- input$country
    
    # Creating the coordinates for the rectangles displaying the restrictions
    rest_coord <- get_coord_for_country(country = var, rest_list = rest, max_y = yLimit())
    
    rest_coord
    
  })
  
  restrictionPlotLabels <- reactive({
    # Restrictions
    rest_plot <- unique(plotRest()[,c("restriction", "y_min", "y_max")])
    rest_plot$y <- rest_plot$y_min + ((rest_plot$y_max - rest_plot$y_min)/2)
    rest_plot
  })
  
  output$plot1 <- renderPlot({
    
    p <- ggplot(plotData()) +
      geom_line(aes(x = date, y = cases), size = 0.8) +
      scale_fill_identity() + 
      xlab("Date") +
      ylab("Cases") +
      theme_minimal() +
      scale_y_continuous(limits = c(0, yLimit())) +
      theme(axis.text.x=element_text(angle=60, hjust=1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      coord_cartesian(xlim = c(min(plotData()$date), max(plotData()$date)), clip = 'off') +
      theme(plot.margin = unit(c(1,12,1,1), "lines"))
    
    if(length(input$restriction) > 0){
      p +
        geom_rect(data = plotRest(), aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = color), alpha = 0.3) +
        annotate("text", x = rep(max(plotData()$date) + 5, nrow(restrictionPlotLabels())), y = restrictionPlotLabels()$y, label = restrictionPlotLabels()$restriction, hjust = 0)
    }else{
      p
    }
  })
}

shinyApp(ui, server)