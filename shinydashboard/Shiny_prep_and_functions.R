# Reading the data prepared by Shiny_data_prep.R
# Functions to process the data by the users queries

country_list <- readRDS("dat/country_list.RDS")
rest_names <- readRDS("dat/rest_names.RDS")
y_limit_list <- readRDS("dat/y_limit_list.RDS")
rest_prev <- readRDS("dat/rest_prev.RDS")
sel_rest_country <- readRDS("dat/sel_rest_country.RDS")
b_vis_long <- readRDS("dat/pred_imp_ranking.RDS")
pred_order <- readRDS("dat/pred_order.RDS")

# Functions
smooth_or_not <- function(to_smooth, cvar, min_date, max_date){
  df <- country_list[[cvar]][which((country_list[[cvar]]$date >= min_date) & (country_list[[cvar]]$date <= max_date)),]
  if(to_smooth == TRUE){
    df <- df[,c(which(colnames(df) %in% c("date", "smoothed_cases", "smoothed_tavg")))]
  }else{
    df <- df[,c(which(colnames(df) %in% c("date", "cases_new", "tavg")))]
  }
  colnames(df) <- c("date", "cases", "tavg")
  return(df)
}

y_limits <- function(lim_list, to_smooth, temp){
  # Maximum
  if(to_smooth == TRUE){
    max_limit <- lim_list[which(colnames(lim_list) == "y_limit_smo")]
  }else{
    max_limit <- lim_list[which(colnames(lim_list) == "y_limit_abs")]
  }
  # Minimum
  if(temp == TRUE){
    if(to_smooth == TRUE){
      min_limit <- min(0, lim_list[which(colnames(lim_list) == "y_min_smo_temp")])
    }else{
      min_limit <- min(0, lim_list[which(colnames(lim_list) == "y_min_abs_temp")])
    }
  }
  else{
    min_limit <- 0
  }
  limits <- c(min_limit, max_limit)
  return(limits)
}

ci_from_se <- function(var, se){
  res <- cbind((var - (qnorm(0.995) * se)), (var + (qnorm(0.995) * se)))
  return(res)
}

fb_smooth_or_not <- function(to_smooth, cvar, min_date, max_date, max_y){
  df <- country_list[[cvar]][which((country_list[[cvar]]$date >= min_date) & (country_list[[cvar]]$date <= max_date)),]
  if(to_smooth == TRUE){
    df <- cbind(df[,c("smoothed_fb_cli", "smoothed_fb_mc", "smoothed_fb_dc")],
                ci_from_se(df$fb_data.smoothed_cli, df$smoothed_fb_cli_se),
                ci_from_se(df$fb_data.smoothed_mc, df$smoothed_fb_mc_se),
                ci_from_se(df$fb_data.smoothed_dc, df$smoothed_fb_dc_se))
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

# Vaccination: people vaccinated or people fully vaccinated?
vacc_smooth_or_not <- function(to_smooth, cvar, min_date, max_date, max_y){
  df <- country_list[[cvar]][which((country_list[[cvar]]$date >= min_date) & (country_list[[cvar]]$date <= max_date)),]
  if(to_smooth == TRUE){
    df <- df[,c("date", "smoothed_people_vaccinated_per_hundred")]
  }else{
    df <- df[,c("date", "people_vaccinated_per_hundred")]
  }
  colnames(df) <- c("date", "vaccination")
  df$vaccination <- df$vaccination * max_y/100
  # Setting NAs before the first nonnull value (no vaccination before)
  if(length(which(df$vaccination > 0)) > 0){
    df$vaccination[1:(which(df$vaccination > 0)[1] - 1)] <- NA
  }else{
    df$vaccination <- rep(NA, nrow(df))
  }
  return(df)
}

number_of_cases <- function(cvar, min_date, max_date){
  tmp <- country_list[[cvar]][which((country_list[[cvar]]$date >= min_date) & (country_list[[cvar]]$date <= max_date)),]
  no_cases <- sum(tmp$cases_new, na.rm = TRUE)
  no_deaths <- sum(tmp$deaths_new, na.rm = TRUE)
  no_recovered <- sum(tmp$recovered_new, na.rm = TRUE)
  no_vacc <- sum(tmp$new_vaccinations, na.rm = TRUE)
  number_of_cases <- c(no_cases, no_deaths, no_recovered, no_vacc)
}

add_lead <- function(dat, lead_to_add){
  dat$cases <- lead(dat$cases, lead_to_add)
  dat <- dat[1:(nrow(dat) - lead_to_add),]
  return(dat)
}

get_x_for_rest <- function(rest, data){
  k <- data
  if(rest %in% colnames(k)){
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
  }else{
    k_coord <- cbind("restriction" = NA, "x_min" = NA, "x_max" = NA)
  }
  return(k_coord)
}


get_coord_for_country <- function(country, rest_list, max_y){
  dat <- rest_prev[[which(names(rest_prev) == country)]]
  # X coordinates
  act_x <- lapply(rest_list, function(x) get_x_for_rest(x, dat))
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

## Bump Chart
# Coordinates for labelling the predictors
bc_pred_label <- function(vis_dat){
  if(!is.vector(vis_dat))
  {
    x_coord <- list()
    pred_list <- split(vis_dat, vis_dat$predictor)
    x1 <- unlist(lapply(pred_list, function(x) {
      y <- x[which(!is.na(x$ranking)),]
      y$ranking[1]
    }
    ))
    x1 <- as.data.frame(cbind("predictor" = names(x1), "ranking" = x1))
    x1$ranking <- as.numeric(x1$ranking)
    x_coord[[1]] <- x1
    x2 <- unlist(lapply(pred_list, function(x){
      y <- x[which(!is.na(x$ranking)),]
      y$ranking[length(y$ranking)]
    }
    ))
    x2 <- as.data.frame(cbind("predictor" = names(x2), "ranking" = x2))
    x2$ranking <- as.numeric(x2$ranking)
    x_coord[[2]] <- x2
    return(x_coord)
  }else return(NULL)
}
