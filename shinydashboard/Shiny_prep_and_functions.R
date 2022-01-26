# Reading the data prepared by Shiny_data_prep.R
# Functions to process the data by the users queries
country_list <- readRDS("dat/country_list.RDS")
rest_names <- readRDS("dat/rest_names.RDS")
y_limit_list <- readRDS("dat/y_limit_list.RDS")
rest_prev <- readRDS("dat/rest_prev.RDS")
sel_rest_country <- readRDS("dat/sel_rest_country.RDS")
b_vis_long <- readRDS("dat/pred_imp_ranking.RDS")
pred_order <- readRDS("dat/pred_order.RDS")
#country_res <- readRDS("dat/country_res.RDS")
pdp_all_c_all_pred <- readRDS("dat/pdp_all_c_all_pred.RDS")
res_label_country <- readRDS("dat/res_label_country.RDS")
rank_corr_res <- readRDS("dat/rank_corr_res.RDS")
clust_dat <- readRDS("dat/clust_dat.RDS")
c_rf_dat_fb <- readRDS("dat/c_rf_dat_fb.RDS")
rf_dat_fb <- readRDS("dat/rf_dat_fb.RDS")
pdp_all_c_all_pred <- readRDS("dat/pdp_all_c_all_pred.RDS")
bc_country <- readRDS("dat/bc_country.RDS")
all_pred_table <- readRDS("dat/all_pred_table.RDS")
pred_table <- readRDS("dat/pred_table.RDS")
rest_table <- readRDS("dat/rest_table.RDS")
eu_clusters_map <- readRDS("dat/eu_clusters_map.RDS")
region_lab <- readRDS("dat/region_lab.RDS")
eu_map <- readRDS("dat/eu_map.RDS")
x_min_max <- readRDS("dat/x_min_max.RDS")
rf_train <- readRDS("dat/rf_train.RDS")
pdp_3d_country <- readRDS("dat/pdp_3d_country.RDS")
selectable_ctr <- readRDS("dat/selectable_3d_country.RDS")

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
                ci_from_se(df$smoothed_fb_cli, df$smoothed_fb_cli_se),
                ci_from_se(df$smoothed_fb_mc, df$smoothed_fb_mc_se),
                ci_from_se(df$smoothed_fb_dc, df$smoothed_fb_dc_se))
  }else{
    df <- cbind(df[,c(which(colnames(df) %in% c("fb_data.pct_covid_cli", "fb_data.percent_mc", "fb_data.percent_dc")))],
                #ci_from_se(df$fb_data.percent_cli, df$fb_data.cli_se),
                ci_from_se(df$fb_data.pct_covid_cli, df$fb_data.covid_se_cli),
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
    k[1,rest_prev] <- 0
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
    cat("Restriction not found")
    k_coord <- as.data.frame(cbind("restriction" = NA, "x_min" = NA, "x_max" = NA))
    k_coord$x_min <- as.Date(k_coord$x_min)
    k_coord$x_max <- as.Date(k_coord$x_max)
  }
  min_ind <- which(colnames(k_coord) == "x_min")
  max_ind <- which(colnames(k_coord) == "x_max")
  k_coord[,min_ind] <- as.Date(k_coord[,min_ind])
  k_coord[,max_ind] <- as.Date(k_coord[,max_ind])
  cat(c(rest, ": min date: ", k_coord$x_min, ", max date: ", k_coord$x_max))
  if((!is.Date(k_coord$x_min) | !is.Date(k_coord$x_max)) | (length(which(is.na(k_coord$x_min))) > 0 | length(which(is.na(k_coord$x_max))))){
    cat("Error in get_x_for_rest (Shiny_prep_and_functions.R) - x coordinates of selected restriction is na or not date")
  }
  return(k_coord)
}

get_coord_for_country <- function(country, rest_list, max_y){
  if(all(!is.na(unlist(rest_list)))){
    dat <- rest_prev[[which(names(rest_prev) == country)]]
    # X coordinates
    act_x <- lapply(rest_list, function(x) get_x_for_rest(x, dat))
    rownums <- lapply(act_x, function(x) nrow(x))
    if(all(!is.null(unlist(rownums)))){
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
      
      # x coordinates to date
      rest_coord$x_min <- as.Date(rest_coord$x_min)
      rest_coord$x_max <- as.Date(rest_coord$x_max)
      
      # Colorlist
      colors <- rep(brewer.pal(n = 8, name = 'Pastel2'), ceiling(length(rest_list)/8))
      col_list <- cbind(rest_list, colors[1:length(rest_list)])
      colnames(col_list) <- c("restriction", "color")
      rest_coord <- merge(rest_coord, col_list, by = "restriction", all.x = TRUE)
      
      # X coordinates for selected restrictions measures are appropriate
      if(!is.Date(rest_coord$x_min) | !is.Date(rest_coord$x_max) | length(which(is.na(rest_coord$x_min))) > 0 | length(which(is.na(rest_coord$x_max))) > 0){
        cat("Error in get_coord_for_country (Shiny_prep_and_functions) - X coordinates are not date or missing for the selected restriction measure")
      }
      
      return(rest_coord)
    }else(cat("Error in get_coord_for_country(Shiny_prep_and_functions) - No dates to display the selected restrictions"))
  }
  else(return(NULL))
}

## Bump Chart
# Coordinates for labelling the predictors
bc_pred_label <- function(vis_dat){
  if(!is.vector(vis_dat))
  {
    x_coord <- list()
    pred_list <- split(vis_dat, vis_dat$predictor)
    x1 <-lapply(pred_list, function(x) {
      x <- x[order(x$country),]
      y <- x[which(!is.na(x$ranking)), c("ranking", "predictor", "pred_text")]
      y[1,]
    })
    x1 <- do.call(rbind, x1)
    x1$ranking <- as.numeric(x1$ranking)
    x_coord[[1]] <- x1
    x2 <- lapply(pred_list, function(x){
      x <- x[order(x$country),]
      y <- x[which(!is.na(x$ranking)), c("ranking", "predictor", "pred_text")]
      y[length(y$ranking),]
    })
    x2 <- do.call(rbind, x2)
    x2$ranking <- as.numeric(x2$ranking)
    x_coord[[2]] <- x2
    return(x_coord)
  }else return(NULL)
}

# Action Buttons
reset_country <- function(session, sel_c){
  col1 <- unique(b_vis_long$country)[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)]
  col2 <- unique(b_vis_long$country)[c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)]
  col3 <- unique(b_vis_long$country)[c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)]
  col4 <- unique(b_vis_long$country)[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)]
  col5 <- unique(b_vis_long$country)[c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)]
  col6 <- unique(b_vis_long$country)[c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)]
  
  if(length(sel_c) > 0)
  {
    updateCheckboxGroupInput(session, inputId = "bc_country", choices = col1, selected = NULL, inline = FALSE)
    updateCheckboxGroupInput(session, inputId = "bc_country2", choices = col2, selected = NULL, inline = FALSE)
    updateCheckboxGroupInput(session, inputId = "bc_country3", choices = col3, selected = NULL, inline = FALSE)
    updateCheckboxGroupInput(session, inputId = "bc_country4", choices = col4, selected = NULL, inline = FALSE)
    updateCheckboxGroupInput(session, inputId = "bc_country5", choices = col5, selected = NULL, inline = FALSE)
    updateCheckboxGroupInput(session, inputId = "bc_country6", choices = col6, selected = NULL, inline = FALSE)
  }
  if(length(sel_c) == 0)
  {
    updateCheckboxGroupInput(session, inputId = "bc_country", choices = col1, selected = col1, inline = FALSE)
    updateCheckboxGroupInput(session, inputId = "bc_country2", choices = col2, selected = col2, inline = FALSE)
    updateCheckboxGroupInput(session, inputId = "bc_country3", choices = col3, selected = col3, inline = FALSE)
    updateCheckboxGroupInput(session, inputId = "bc_country4", choices = col4, selected = col4, inline = FALSE)
    updateCheckboxGroupInput(session, inputId = "bc_country5", choices = col5, selected = col5, inline = FALSE)
    updateCheckboxGroupInput(session, inputId = "bc_country6", choices = col6, selected = col6, inline = FALSE)
  }
}

reset_predictor <- function(session, sel_p){
  col1 <- pred_order$predictor[1:30][c(TRUE, FALSE)]
  col2 <- pred_order$predictor[1:30][c(FALSE, TRUE)]
  
  if(length(sel_p) > 0)
  {
    updateCheckboxGroupInput(session, inputId = "bc_pred", choices = col1, selected = NULL, inline = FALSE)
    updateCheckboxGroupInput(session, inputId = "bc_pred2", choices = col2, selected = NULL, inline = FALSE)
  }
  if(length(sel_p) == 0)
  {
    updateCheckboxGroupInput(session, inputId = "bc_pred", choices = col1, selected = col1, inline = FALSE)
    updateCheckboxGroupInput(session, inputId = "bc_pred2", choices = col2, selected = col2, inline = FALSE)
  }
}

bump_country_box <- function(){
  col1 <- unique(b_vis_long$country)[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)]
  col2 <- unique(b_vis_long$country)[c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)]
  col3 <- unique(b_vis_long$country)[c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)]
  col4 <- unique(b_vis_long$country)[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)]
  col5 <- unique(b_vis_long$country)[c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)]
  col6 <- unique(b_vis_long$country)[c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)]
  box(title = "Countries:",
      column(2,
             checkboxGroupInput("bc_country", "", choices = col1, selected = col1, inline = FALSE)
      ),
      column(2,
             checkboxGroupInput("bc_country2", "", choices = col2, selected = col2, inline = FALSE)
      ),
      column(2,
             checkboxGroupInput("bc_country3", "", choices = col3, selected = col3, inline = FALSE)
      ),
      column(2,
             checkboxGroupInput("bc_country4", "", choices = col4, selected = col4, inline = FALSE)
      ),
      column(2,
             checkboxGroupInput("bc_country5", "", choices = col5, selected = col5, inline = FALSE)
      ),
      column(2,
             checkboxGroupInput("bc_country6", "", choices = col6, selected = col6, inline = FALSE)
      ), width = 12, height = 200
  )
}

bump_predictor_box <- function(){
  col1 <- pred_order$predictor[1:30][c(TRUE, FALSE)]
  col2 <- pred_order$predictor[1:30][c(FALSE, TRUE)]
  box(title = "Predictors:", style = "margin-bottom: 0px;",
      column(6, style = "margin-top: 0px;",
             checkboxGroupInput("bc_pred", "", choices = col1, selected = NULL, inline = FALSE)
      ),
      column(6, style = "margin-top: 0px;",
             checkboxGroupInput("bc_pred2", "", choices = col2, selected = NULL, inline = FALSE)
      ), width = 12, height = 460
  )
}

# Tooltips and labels for Bump Chart
bc_labels <- merge(pred_order[1:15,], rest_table, by.x = "predictor", by.y = "res_id", all.x = TRUE)
bc_labels <- merge(bc_labels, pred_table, by.x = "predictor", by.y = "pred_id", all.x = TRUE)
bc_labels <- bc_labels %>%
  mutate(d_text = coalesce(res_text, pred_text)) %>%
  mutate(d_label = coalesce(res_label, pred_label)) %>%
  dplyr::select(predictor, d_text, d_label)
bc_labels$d_label[which(is.na(bc_labels$d_label))] <- bc_labels$predictor[which(is.na(bc_labels$d_label))]
bc_labels <- bc_labels[order(match(bc_labels$predictor, pred_order$predictor[1:20])),]
bc_labels$predictor <-  paste("bc", bc_labels$predictor, sep = "_")
                           
# countries to display on bump chart
countries <- unique(b_vis_long$country)[order(unique(b_vis_long$country))]

#                      
# Display Checkboxgroups with label
checkboxgroup_with_label <- function(names, labels, tooltips, width, height, selected = FALSE){
  cgroup <- list()
  for(i in 1:length(names)){
    cgroup[[i]] <- column(tags$div(title = tooltips[i],
                                   checkboxInput(names[i], labels[i], selected)),
                          width = width, height = height, background = NULL)
  }
  cgroup
}

get_input_checkboxgroup_with_label <- function(names, input){
  sel_input <- c()
  for(i in 1:length(names))
  {
      if(length(input[[names[i]]] > 0)){
        sel_input[i] <- input[[names[i]]]
      }
  }
  
  return(names[sel_input == TRUE])
}

update_checkboxgroup_with_label <- function(session, names, labels){
  for(i in 1:length(names))
  {
    updateCheckboxInput(session = session, names[i], labels[i], FALSE)
  }
}

check_uncheck_checkboxgroup_with_label <- function(session, names, labels, to_check){
  for(i in 1:length(names))
  {
    if(length(to_check) > 0){
      updateCheckboxInput(session = session, names[i], labels[i], FALSE)
    }
    else{
      updateCheckboxInput(session = session, names[i], labels[i], TRUE)
    }
  }
}


# Cut rank correlation of variable importance into 3 categories
names(rank_corr_res) <- c(1:7)
rank_corr_all <- do.call(rbind, rank_corr_res)
rank_corr_all$groups <- str_replace_all(rownames(rank_corr_all), "\\.\\d$", "")
# Dealing with Ireland separately (alone in the cluster -> corr is 1)
ire_corr <- rank_corr_all[which(rank_corr_all$country == "Ireland"),]
rank_corr_all <- rank_corr_all[-which(rank_corr_all$country == "Ireland"),]
rank_corr_all$corr_cut <- cut(rank_corr_all$correlation, 3, labels = c("low", "middle", "high"))
# Adding back Ireland
rank_corr_all <- rbind(rank_corr_all, c(ire_corr, "corr_cut" = "high"))
rank_corr_all <- rank_corr_all[order(rank_corr_all$groups),]
rank_corr_res <- split(rank_corr_all, rank_corr_all$groups)
