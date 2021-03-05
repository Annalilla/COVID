# Contains functions to respond interactively to the queries in the shiny application.
# Prepares selected variables and time intervals for the visualization.

library(RColorBrewer)

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
