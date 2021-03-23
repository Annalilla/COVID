# Content and functionality of the shiny dashboard application.
# Contains functions to respond interactively to the queries in the shiny application.

# Interactive data visualization

library(shinydashboard)
library(tidyverse)
library(RColorBrewer)

##
# Data
tdata <- readRDS("dat/tdata.RDS")
rest_names <- readRDS("dat/rest_names.RDS")
y_limit_list <- readRDS("dat/y_limit_list.RDS")
country_list <- readRDS("dat/country_list.RDS")
rest_prev <- readRDS("dat/rest_prev.RDS")

##
# Functions
smooth_or_not <- function(to_smooth, cvar, min_date, max_date){
  df <- country_list[[cvar]][which((country_list[[cvar]]$date >= min_date) & (country_list[[cvar]]$date <= max_date)),]
  if(to_smooth == TRUE){
    df <- df[,c(which(colnames(df) %in% c("date", "smoothed_cases")))]
  }else{
    df <- df[,c(which(colnames(df) %in% c("date", "cases_new")))]
  }
  colnames(df) <- c("date", "cases")
  return(df)
}

ci_from_se <- function(var, se){
  res <- cbind((var - (qnorm(0.995) * se)), (var + (qnorm(0.995) * se)))
  return(res)
}

fb_smooth_or_not <- function(to_smooth, cvar, min_date, max_date, max_y){
  df <- country_list[[cvar]][which((country_list[[cvar]]$date >= min_date) & (country_list[[cvar]]$date <= max_date)),]
  if(to_smooth == TRUE){
    df <- cbind(df[,c("fb_data.smoothed_cli", "fb_data.smoothed_mc", "fb_data.smoothed_dc")],
                ci_from_se(df$fb_data.smoothed_cli, df$fb_data.smoothed_cli_se),
                ci_from_se(df$fb_data.smoothed_mc, df$fb_data.smoothed_mc_se),
                ci_from_se(df$fb_data.smoothed_dc, df$fb_data.smoothed_dc_se))
  }else{
    df <- cbind(df[,c(which(colnames(df) %in% c("fb_data.percent_cli", "fb_data.percent_mc", "fb_data.percent_dc")))],
                ci_from_se(df$fb_data.percent_cli, df$fb_data.cli_se),
                ci_from_se(df$fb_data.percent_mc, df$fb_data.mc_se),
                ci_from_se(df$fb_data.percent_dc, df$fb_data.dc_se))
  }
  colnames(df) <- c("cli", "mc", "dc", "cli_ci_l", "cli_ci_u", "mc_ci_l", "mc_ci_u", "dc_ci_l", "dc_ci_u")
  df <- apply(df, 2, function(x) x * max_y)
  return(df)
}

number_of_cases <- function(cvar, min_date, max_date){
  no_cases <- sum(country_list[[cvar]][which((country_list[[cvar]]$date >= min_date) & (country_list[[cvar]]$date <= max_date)), "cases_new"], na.rm = TRUE)
  no_deaths <- sum(country_list[[cvar]][which((country_list[[cvar]]$date >= min_date) & (country_list[[cvar]]$date <= max_date)), "deaths_new"], na.rm = TRUE)
  no_recovered <- sum(country_list[[cvar]][which((country_list[[cvar]]$date >= min_date) & (country_list[[cvar]]$date <= max_date)), "recovered_new"], na.rm = TRUE)
  number_of_cases <- c(no_cases, no_deaths, no_recovered)
}

add_lead <- function(dat, lead_to_add){
  dat$cases <- lead(dat$cases, lead_to_add)
  dat <- dat[1:(nrow(dat) - lead_to_add),]
  return(dat)
}

get_x_for_rest <- function(rest, data){
  k <- data
  colnames(k) <- c(colnames(k)[1:(ncol(k)/2)], paste(colnames(k)[1:(ncol(k)/2)], "prev", sep = "_"))
  k$x_min <- as.Date(NA)
  k$x_max <- as.Date(NA)
  
  rest_prev <- paste(rest, "prev", sep = "_")
  k <- k[, which(colnames(k) %in% c("date", rest, "date_prev", rest_prev, "x_min", "x_max"))]
  k$x_min[which(k[,which(colnames(k) == rest_prev)] == 0 & k[,which(colnames(k) == rest)] == 1)] <-
    k$date[which(k[,which(colnames(k) == rest_prev)] == 0 & k[,which(colnames(k) == rest)] == 1)]
  k$x_max[which(k[,which(colnames(k) == rest_prev)] == 1 & k[,which(colnames(k) == rest)] == 0)] <-
    k$date_prev[which(k[,which(colnames(k) == rest_prev)] == 1 & k[,which(colnames(k) == rest)] == 0)]
  if(k[nrow(k),which(colnames(k) == rest)] == 1){
    k$x_max[nrow(k)] <- k$date[nrow(k)]
  }
  k_coord <- data.frame("x_min" = k$x_min[which(!is.na(k$x_min))], "x_max" = k$x_max[which(!is.na(k$x_max))])
  k_coord <- cbind("restriction" = rep(rest, nrow(k_coord)), k_coord)
  k_coord
}

get_x_for_country <- function(country){
  act_all_rest <- sel_rest_country[[which(names(sel_rest_country) == country)]]
  act_x <- lapply(act_all_rest, function(x) get_x_for_rest(x, rest_prev[[which(names(rest_prev) == country)]]))
  names(act_x) <- act_all_rest
  act_x
}

get_coord_for_country <- function(country, rest_list, max_y){
  
  # X coordinates
  act_x <- lapply(rest_list, function(x) get_x_for_rest(x, rest_prev[[which(names(rest_prev) == country)]]))
  names(act_x) <- rest_list
  
  # Y coordinates
  coord <- seq(0, max_y, len = length(rest_list) + 1)
  if(length(rest_list) == 1){
    coord <- as.data.frame(cbind(rest_list, t(coord)))
  }else{
    coord <- c(coord[1], rep(coord[2:(length(coord) - 1)], each = 2), coord[length(coord)])
    coord <- t(do.call(rbind, (split(coord, c(1,length(rest_list))))))
    coord <- as.data.frame(cbind(rest_list, coord))
  }
  colnames(coord) <- c("restriction", "y_min", "y_max")
  coord[, c("y_min", "y_max")] <- lapply(coord[, c("y_min", "y_max")], as.numeric)
  
  # All coordinates into a dataframe
  rest_coord <- lapply(rest_list, function(x) act_x[[x]] <- cbind(act_x[[x]], coord[coord$restriction == x, c("y_min", "y_max")], row.names = NULL))
  names(rest_coord) <- rest_list
  rest_coord <- do.call(rbind, rest_coord)
  
  # Colorlist
  colors <- brewer.pal(n = 8, name = 'Pastel2')
  col_list <- cbind(rest_list, colors[1:length(rest_list)])
  colnames(col_list) <- c("restriction", "color")
  rest_coord <- merge(rest_coord, col_list, by = "restriction", all.x = TRUE)
  
  rest_coord
}

##
# Shiny

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
                column(4,
                  box(
                    title = "Controls", status = "info",
                    
                    selectInput("country", "Variable:",
                                choices = unique(tdata$country)),
                    
                    sliderInput("dateinterval", "Date Range",
                                min = min(tdata$date),
                                max = max(tdata$date),
                                value = c(min(tdata$date), max(tdata$date))
                    ),
                    
                    checkboxInput("cli", "CLI", FALSE),
                    
                    checkboxInput("mc", "MC", FALSE),
                    
                    checkboxInput("dc", "DC", FALSE),
                    
                    checkboxInput("smoothed", "Smoothed", TRUE),
                    
                    textInput("lead", "Lead:", value = "0", width = NULL, placeholder = NULL),
                    
                    checkboxGroupInput("restriction", "Restriction:",
                                       choices = rest_names), width = 12
                  )
                ),
                
                column(8,
                       infoBox("Infections", textOutput("no_cases"), fill = TRUE),
                       infoBox("Deaths", textOutput("no_deaths"), fill = TRUE),
                       infoBox("Recovered", textOutput("no_recovered"), fill = TRUE),
                       box(title = textOutput("charttitle"), textOutput("chartsubtitle"), status = "info",
                           plotOutput("plot1"), width = 12)
                       )
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
  
  # Dynamic number of cases/deaths/recovred
  numberCases <- reactive({
    number_of_cases(input$country,input$dateinterval[1], input$dateinterval[2])
  })
  
  output$no_cases <- renderText({
    numberCases()[1]
  })
  
  output$no_deaths <- renderText({
    numberCases()[2]
  })
  
  output$no_recovered <- renderText({
    numberCases()[3]
  })
  
  
  #Subtitle
  subtitleText <- reactive({
    if(!is.na(input$lead) & input$lead != "")
    {
      lead_bad <- is.na(as.numeric(input$lead))
      if(!lead_bad){
        input_lead <- as.numeric(input$lead)
        if(input_lead > 0 & input_lead < nrow(plotData())){
          paste("Number of infections with", input$lead, "days lead", sep = " ")
        }else if(input_lead < 0){
          paste("Lead has to be positive")
        }else if(input_lead == 0){
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
    df <-  cbind(smooth_or_not(smoothed, var, min_date, max_date), fb_smooth_or_not(smoothed, var, min_date, max_date, yLimit()))
    
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
    
    if(input$cli == TRUE)
    {
      p <- p +
        geom_line(aes(x = date, y = cli_ci_l), size = 0.8, color = brewer.pal(n = 3, name = 'Dark2')[1], alpha=0.7) +
        geom_line(aes(x = date, y = cli_ci_u), size = 0.8, color = brewer.pal(n = 3, name = 'Dark2')[1], alpha=0.7) +
        geom_line(aes(x = date, y = cli), size = 0.8, color = brewer.pal(n = 3, name = 'Dark2')[1]) +
        geom_ribbon(aes(x = date, ymin = cli_ci_l, ymax = cli_ci_u), fill = brewer.pal(n = 3, name = 'Dark2')[1], alpha=0.3)
    }
    if(input$mc == TRUE)
    {
      p <- p +
        geom_line(aes(x = date, y = mc_ci_l), size = 0.8, color = brewer.pal(n = 3, name = 'Dark2')[2], alpha=0.7) +
        geom_line(aes(x = date, y = mc_ci_u), size = 0.8, color = brewer.pal(n = 3, name = 'Dark2')[2], alpha=0.7) +
        geom_line(aes(x = date, y = mc), size = 0.8, color = brewer.pal(n = 3, name = 'Dark2')[2]) +
        geom_ribbon(aes(x = date, ymin = mc_ci_l, ymax = mc_ci_u), fill = brewer.pal(n = 3, name = 'Dark2')[2], alpha=0.3)
    }
    if(input$dc == TRUE)
    {
      p <- p +
        geom_line(aes(x = date, y = dc_ci_l), size = 0.8, color = brewer.pal(n = 3, name = 'Dark2')[3], alpha=0.7) +
        geom_line(aes(x = date, y = dc_ci_u), size = 0.8, color = brewer.pal(n = 3, name = 'Dark2')[3], alpha=0.7) +
        geom_line(aes(x = date, y = dc), size = 0.8, color = brewer.pal(n = 3, name = 'Dark2')[3]) +
        geom_ribbon(aes(x = date, ymin = dc_ci_l, ymax = dc_ci_u), fill = brewer.pal(n = 3, name = 'Dark2')[3], alpha=0.3)
    }
    
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
