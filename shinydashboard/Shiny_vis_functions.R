# Contains functions to respond interactively to the queries in the shiny application.

exp_subtitle <- function(ilead, dat, mc, dc, vacc, tavg){
  if(!is.na(ilead) & ilead != "")
  {
    lead_bad <- is.na(as.numeric(ilead))
    if(!lead_bad){
      input_lead <- as.numeric(ilead)
      if(input_lead > 0 & input_lead < nrow(dat)){
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
  subsubt <- NULL
  if(!all(isFALSE(c(mc, dc, vacc, tavg)))){
    if(mc == TRUE){ if(all(is.na(dat$mc))) subsubt <- c(subsubt, "Mask Coverage")}
    if(dc == TRUE){ if(all(is.na(dat$dc))) subsubt <- c(subsubt, "Direct Contact")}
    if(vacc == TRUE){ if(all(is.na(dat$vaccination))) subsubt <- c(subsubt, "Vaccination")}
    if(tavg == TRUE){ if(all(is.na(dat$tavg))) subsubt <- c(subsubt, "Average daily temperature")}
  }
  if(length(subsubt) > 0){
    subsubt <- paste("No data available for", paste(subsubt, collapse = ", "))
    subt <- paste(subt, paste('<p style="color:red">', subsubt, '</p>', sep = ""))
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
  if((mc == TRUE & !all(is.na(dat$mc))) | (dc == TRUE & !all(is.na(dat$dc))) | (vacc == TRUE & length(which(dat$vaccination > 0)) > 0)){
  varnames <- NULL
  if(mc == TRUE & !all(is.na(dat$mc))) varnames <- c(varnames, "Mask Coverage")
  if(dc == TRUE & !all(is.na(dat$dc))) varnames <- c(varnames, "Direct Contact")
  if((vacc == TRUE & length(which(dat$vaccination > 0)) > 0)) varnames <- c(varnames, "Vaccination")
  axis_title <- paste(paste(varnames, collapse = " & "), "%", sep = " ")
  p <- p +
    scale_y_continuous(limits = c(y_min, y_max),
                       sec.axis = sec_axis(~./y_max, name = axis_title,
                                           labels = function(x) {
                                             unlist(lapply(x, function(y){
                                               if(!is.na(y) & y >= 0){
                                                 paste0(round(y * 100, 0), "%")
                                               }else{
                                                 " "
                                               }
                                             }))
                                           }))
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
    labt <- paste(round(dat[which.max(dat$tavg), "tavg"], 1), "Â°C", sep = "")
    pointx <- dat[which.max(dat$tavg), "date"]
    pointy <- (multi * dat[which.max(dat$tavg), "tavg"])
    labpxmin <- (dat[which.min(dat$tavg), "date"] + x_dist)
    labpymin <- (multi * dat[which.min(dat$tavg), "tavg"])
    labtmin <- paste(round(dat[which.min(dat$tavg), "tavg"], 1), "Â°C", sep = "")
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
  if((mc == TRUE & !all(is.na(dat$mc))) | (dc == TRUE & !all(is.na(dat$dc))) | (vacc == TRUE & length(which(dat$vaccination > 0)) > 0)){
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
    
    if(mc == TRUE & !all(is.na(dat$mc)))
    {
      plot <- plot +
        geom_ribbon(data = dat, aes(x = date, ymin = mc_ci_l, ymax = mc_ci_u), fill ="grey", alpha=0.3)
    }
    if(dc == TRUE & !all(is.na(dat$dc)))
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
    x_length <- as.numeric(difftime(max(plotdata$date), min(plotdata$date), units = "days"))
    x_to_add <- 0
    if((mc == FALSE | all(is.na(plotdata$mc))) & (dc == FALSE | all(is.na(plotdata$dc))) & (vacc == FALSE | length(which(plotdata$vaccination == 0)) > 0)) {
      if(x_length > 20){
        x_to_add <- round(as.numeric(difftime(max(plotdata$date), min(plotdata$date), units = "days"))/15 + 1, 0)
      }else if(x_length > 8){
        x_to_add <- 1
      }
    }else{
      if(x_length > 20){
        x_to_add <- rest_x <- round(as.numeric(difftime(max(plotdata$date), min(plotdata$date), units = "days"))/6 + 1, 0)
      }else if(x_length > 8){
        x_to_add <- 2
      }else if(x_length > 4){
        x_to_add <- 1
      }else{
        x_to_add <- 0.3
      }
    } 
    if(!is.null(nrow(restlabels))){
      rest_x <- rep(max(plotdata$date) + x_to_add, nrow(restlabels))
    }
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
  
  
bump_chart <- function(vis_dat, x_coord){
  if(!is.vector(vis_dat)){
    if(nrow(vis_dat) > 0 & !all(is.na(vis_dat$ranking))){
      first_country <- unique(vis_dat$country)[1]
      last_country <- unique(vis_dat$country)[length(unique(vis_dat$country))]
      
      ggplot(data = vis_dat, aes(x = country, y = ranking, group = pred_text)) +
        geom_line(aes(color = pred_text, alpha = 0.5), size = 2) +
        geom_point(aes(color = pred_text, alpha = 0.5), size = 4) +
        geom_point(color = "#FFFFFF", size = 1) +
        scale_y_reverse() +
        scale_x_discrete(breaks = unique(vis_dat$country)) +
        geom_text(data = x_coord[[1]],
                  aes(label = pred_text, x = 0) , hjust = 1, fontface = "bold", color = "#888888", size = 4) +
        #geom_text(data = x_coord[[2]],
        #          aes(label = pred_text, x = (length(unique(vis_dat$country))) + 1), hjust = 0, fontface = "bold",
        #          color = "#888888", size = 4) +
        labs(x = "Countries",
             y = "Rank") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=60, hjust=1, size = 14),
              axis.text.y = element_text(size = 14),
              panel.grid.major.x = element_blank(),
              legend.position = "none",
              plot.margin = unit(c(1,3,1,1), "lines")) +
        coord_cartesian(xlim = c(-(round(length(unique(vis_dat$country))/3, 0)),length(unique(vis_dat$country))), clip = 'off')
    }else{
      mess <- "Not available for the selected country"
      mess <- paste(paste('<p style="color:red">', mess, '</p>', sep = ""))
      mess
    }
  }else{
    mess <- NULL
    if("no_pred" %in% vis_dat){
      mess <- c(mess, "predictor")
    }
    if("no_countr" %in% vis_dat){
      mess <- c(mess, "country")
    }
    mess <- paste(mess, collapse = " and a " )
    mess <- paste("Please select a ", mess, sep = "")
    mess <- paste(paste('<p style="color:red">', mess, '</p>', sep = ""))
  }
}
  
  
## Cluster Variable importance correlation
# eu map with clusters colored by cluster
eu_cluster_vis <- function(){
  p <- ggplot(eu_clusters_map, aes(long, lat)) +
    geom_polygon(aes(group = group, fill = cluster), color = "white") +
    geom_text(aes(label = region), data = region_lab,  size = 3, hjust = 0.5) +
    scale_fill_manual(values=c("#F8766D", "#B79F00", "#00BA38", "#38CBCE", "#68A0FE", "#F564E3", "#b87ba9")) +
    coord_fixed() +
    theme(plot.margin = unit(c(1,1,0,1), "lines")) +
    theme_void()
  return(p)
}

# Map of countries in the selected cluster
cluster_varimp_corr_vis <- function(clus, corr_strength){
  # Countries and varimp in the selected cluster
  if(clus == "All Clusters"){
    corr_sel <- rank_corr_all
    sel_country <- corr_sel$country
  }else{
    sel_country <- rank_corr_res[[as.numeric(clus)]]$country
    corr_sel <- rank_corr_res[[as.numeric(clus)]]
  }
  colnames(corr_sel)[which(colnames(corr_sel)  == "country")] <- "region"
  # Replace "Czechia" with "Czech Republic" to match with the maps
  corr_sel[which(corr_sel$region == "Czechia"), "region"] <- "Czech Republic"
  
  # Showing only countries with the selected correlation
  if(corr_strength != "All"){
    corr_strength <- tolower(corr_strength)
    corr_sel[which(corr_sel$corr_cut != corr_strength), "correlation"] <- NA
  }
  
  act_eu_clusters_map <- left_join(corr_sel, eu_map, by = "region")
  
  region_lab_cluster <- act_eu_clusters_map %>%
    group_by(region) %>%
    dplyr::summarise(long = mean(long), lat = mean(lat))
  
  p <- ggplot(act_eu_clusters_map, aes(long, lat)) +
    geom_polygon(aes(group = group, fill = correlation), color = "white") +
    geom_text(aes(label = region), data = region_lab_cluster,  size = 3, hjust = 0.5) +
    scale_fill_continuous(type = "viridis",
                          na.value="#e0e0e0") +
    #scale_fill_gradient2(low = muted("green"),
    #                     mid = "white",
    #                     high = muted("blue"),
    #                     midpoint = 0.3455,
    #                     na.value="#e0e0e0") +
    coord_fixed() +
    theme_void()
  return(p)
}
