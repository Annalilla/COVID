# Contains functions to respond interactively to the queries in the shiny application.

exp_subtitle <- function(ilead, max_lead){
  if(!is.na(ilead) & ilead != "")
  {
    lead_bad <- is.na(as.numeric(ilead))
    if(!lead_bad){
      input_lead <- as.numeric(ilead)
      if(input_lead > 0 & input_lead < nrow(max_lead)){
        subt <- paste("Number of infections with", ilead, "days lead", sep = " ")
      }else if(input_lead < 0){
        subt <- paste("Lead has to be positive")
      }else if(input_lead == 0){
        subt <- paste("Number of infections")
      }else{
        subt <- paste("Lead is too big")
      }
    }else{
      subt <- paste("Lead has to be numeric")
    }
  }else{
    subt <- paste("Number of infections")
  }
  return(subt)
}

exp_plot_base <- function(dat, y_min, y_max, mc, dc, vacc){
  p <- ggplot(dat) +
        geom_line(aes(x = date, y = cases), size = 0.8) +
        xlab("Date") +
        ylab("Cases") +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=60, hjust=1),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.margin = unit(c(1,1,0,1), "lines")) +
        coord_cartesian(xlim = c(min(dat$date), max(dat$date)), clip = 'off')
  # Second Y axis labels
  if(mc == TRUE | dc == TRUE | (vacc == TRUE & length(which(dat$vaccination > 0)) > 0)){
  varnames <- NULL
  if(mc == TRUE) varnames <- c(varnames, "Mask Coverage")
  if(dc == TRUE) varnames <- c(varnames, "Direct Contact")
  if((vacc == TRUE & length(which(dat$vaccination > 0)) > 0)) varnames <- c(varnames, "Vaccination")
  axis_title <- paste(paste(varnames, collapse = " & "), "%", sep = " ")
  p <- p +
    scale_y_continuous(limits = c(y_min, y_max),
                       sec.axis = sec_axis(~./y_max, name = axis_title,
                                           labels = function(x) { paste0(round(x * 100, 0), "%")}))
  }
  else{
    p <- p +
      scale_y_continuous(limits = c(y_min, y_max))
  }
  return(p)
}

exp_plot_add_temp <- function(plot, temp, dat){
  if(temp == TRUE){ 
    x_dist <- as.numeric(round((max(dat$date, na.rm = TRUE) - min(dat$date, na.rm = TRUE))/25, 0))
    tcol <- brewer.pal(n = 3, name = 'Set1')[1]
    multi <- max(dat$cases, na.rm = TRUE)/max(dat$tavg, na.rm = TRUE)
    labpx1 <- (dat[which.max(dat$tavg), "date"] + x_dist)
    labpy1 <- (multi * dat[1, "tavg"] + multi)
    labt1 <- paste(round(dat[1, "tavg"], 1), "Â°C", sep = "")
    pointx1 <- dat[1, "date"]
    pointy1 <- (multi * dat[1, "tavg"])
    labpx <- (dat[which.max(dat$tavg), "date"] + x_dist)
    labpy <- (multi * dat[which.max(dat$tavg), "tavg"] + multi)
    labt <- paste(round(dat[which.max(dat$tavg), "tavg"], 1), "°C", sep = "")
    pointx <- dat[which.max(dat$tavg), "date"]
    pointy <- (multi * dat[which.max(dat$tavg), "tavg"])
    labpxmin <- (dat[which.min(dat$tavg), "date"] + x_dist)
    labpymin <- (multi * dat[which.min(dat$tavg), "tavg"])
    labtmin <- paste(round(dat[which.min(dat$tavg), "tavg"], 1), "°C", sep = "")
    pointxmin <- dat[which.min(dat$tavg), "date"]
    pointymin <- (multi * dat[which.min(dat$tavg), "tavg"])
    
    plot <- plot +
      geom_line(aes(x = date, y = (multi * tavg)), color = tcol, size = 0.8, alpha=0.7) +
      geom_point(aes(x = pointx, y = pointy), color = tcol, size = 2, alpha = 0.7) +
      geom_point(aes(x = pointxmin, y = pointymin), color = tcol, size = 2, alpha = 0.7) +
      annotate("text", x = labpxmin, y = labpymin, label = labtmin, color = tcol) +
      annotate("text", x = labpx, y = labpy, label = labt, color = tcol)
  }
  return(plot)
}

exp_plot_add_fb_vacc <- function(plot, mc, dc, vacc, dat){
  if(mc == TRUE | dc == TRUE | (vacc == TRUE & length(which(dat$vaccination > 0)) > 0)){
    select <- NULL
    varnames <- NULL
    if(mc == TRUE){
      select <- c(select, "mc")
      varnames <- c(varnames, "Mask Coverage")
    }
    if(dc == TRUE){
      select <- c(select, "dc")
      varnames <- c(varnames, "Direct Contact")
    }
    if(vacc == TRUE & length(which(dat$vaccination > 0)) > 0){
      select <- c(select, "vaccination")
      varnames <- c(varnames, "Vaccination")
    }
    dat_l <- dat[,c("date", colnames(dat)[which(colnames(dat) %in% select)])]
    colnames(dat_l) <- c("date", varnames)
    df <- melt(dat_l, id.vars = 1)
    plot <- plot + 
      geom_line(data = df, aes(x = date, y = value, colour = variable), size = 0.8) +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      scale_color_brewer(palette = "Dark2")
    
    if(mc == TRUE)
    {
      plot <- plot +
        geom_ribbon(data = dat, aes(x = date, ymin = mc_ci_l, ymax = mc_ci_u), fill ="grey", alpha=0.3)
    }
    if(dc == TRUE)
    {
      plot <- plot +
        geom_ribbon(data = dat, aes(x = date, ymin = dc_ci_l, ymax = dc_ci_u), fill = "grey", alpha=0.3)
    }
  }
  return(plot)
}

exp_display_plot <- function(plot, rest, plotrest, plotdata, restlabels, mc, dc, temp, vacc){
  if(length(rest) > 0){
    # x axis for restriction labels
    if(mc == FALSE & dc == FALSE & vacc == FALSE & temp == FALSE) {rest_x <- rep(max(plotdata$date) + 5, nrow(restlabels))}
    else{rest_x <- rep(max(plotdata$date) + 70, nrow(restlabels))} 
    if(is.Date(plotrest$x_min) & is.Date(plotrest$x_max)){
      plotrest$x_max[which(plotrest$x_max > max(plotdata$date, na.rm = TRUE))] <- max(plotdata$date, na.rm = TRUE)
      plotrest$x_min[which(plotrest$x_min < min(plotdata$date, na.rm = TRUE))] <- min(plotdata$date, na.rm = TRUE)
      plotrest$x_min[which(plotrest$x_min > max(plotdata$date, na.rm = TRUE))] <- NA
      plotrest$x_max[which(plotrest$x_min > max(plotdata$date, na.rm = TRUE))] <- NA
      plotrest$x_min[which(plotrest$x_max < min(plotdata$date, na.rm = TRUE))] <- NA
      plotrest$x_max[which(plotrest$x_max < min(plotdata$date, na.rm = TRUE))] <- NA
      # Adding restrictions to plot
      plot <- plot +
        theme(plot.margin = unit(c(1,13,0,1), "lines")) +
        geom_rect(data = plotrest, aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = color), show.legend = FALSE, alpha = 0.4) +
        annotate("text", x = rest_x, y = restlabels$y, label = restlabels$restriction, hjust = 0)
    }
  }
  plot
}

