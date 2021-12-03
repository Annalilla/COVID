# Prepares the data for the dashboard to enable fast visualisation and to improve interactivity.

library(zoo)
library(tidyverse)
library(data.table)
library(caret)
library(pdp)
library(readr)
library(stringr)
library(dplyr)
library(caret)
library(plotly)

source("helpers/Restriction_labels.R")

# Smooth variables as new variable in tdata: cases, tavg
country_list <- split(tdata, tdata$country)
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_cases" = rollapply(x$cases_new, 7, mean, na.rm = TRUE, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_tavg" = rollapply(x$tavg, 7, mean, na.rm = TRUE, fill = NA)))
#country_list <- lapply(country_list, function(x) cbind(x, "smoothed_fb_cli" = rollapply(x$fb_data.percent_cli, 7, mean, na.rm = TRUE, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_fb_cli" = rollapply(x$fb_data.pct_covid_cli, 7, mean, na.rm = TRUE, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_fb_mc" = rollapply(x$fb_data.percent_mc, 7, mean, na.rm = TRUE, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_fb_dc" = rollapply(x$fb_data.percent_dc, 7, mean, na.rm = TRUE, fill = NA)))
#country_list <- lapply(country_list, function(x) cbind(x, "smoothed_fb_cli_se" = rollapply(x$fb_data.cli_se, 7, mean, na.rm = TRUE, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_fb_cli_se" = rollapply(x$fb_data.covid_se_cli, 7, mean, na.rm = TRUE, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_fb_mc_se" = rollapply(x$fb_data.mc_se, 7, mean, na.rm = TRUE, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_fb_dc_se" = rollapply(x$fb_data.dc_se, 7, mean, na.rm = TRUE, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_people_vaccinated_per_hundred" = rollapply(x$people_vaccinated_per_hundred, 7, mean, na.rm = TRUE, fill = NA)))
saveRDS(country_list, "shinydashboard/dat/country_list.RDS")

# Maximum limit of y (list of maximum limits for absolute number and smoothed number of cases for all countries)
multi <- 1.05
y_limit_list <- lapply(country_list, function(x)
  cbind("y_limit_abs" = floor(multi * max(x$cases_new, na.rm = TRUE)),
        "y_limit_smo" = floor(multi * max(x$smoothed_cases, na.rm = TRUE)),
        "y_min_abs_temp" = floor(multi * min(x$tavg, na.rm = TRUE) * (max(x$cases, na.rm = TRUE)/max(x$tavg, na.rm = TRUE))),
        "y_min_smo_temp" = floor(multi * min(x$smoothed_tavg, na.rm = TRUE) * (max(x$cases, na.rm = TRUE)/max(x$tavg, na.rm = TRUE)))))
saveRDS(y_limit_list, "shinydashboard/dat/y_limit_list.RDS")

# Restrictions 

# Restriction variables are binary
rest_names <- colnames(tdata[, unlist(lapply(tdata, function(x) all(na.omit(x) %in% c(0, 1))))])
saveRDS(rest_names, "shinydashboard/dat/rest_names.RDS")

# Selecting applied restrictions in all countries
rest_list <- lapply(country_list, function(x) x %>% dplyr::select("date", rest_names))
rest_list <- lapply(rest_list, function(x) cbind("date" = x$date, as.data.frame(apply(x[,-which(colnames(x) == "date")], 2, as.numeric))))
rest_list <- lapply(rest_list, function(x) cbind("date" = x$date, x[,-which(colnames(x) == "date")][, which(colSums(x[,-which(colnames(x) == "date")], na.rm = TRUE) > 0)]))
sel_rest_country <- lapply(rest_list, function(x) colnames(x[,-which(colnames(x) == "date")]))
sel_rest_country <- lapply(sel_rest_country, function(x) x[order(x)])
saveRDS(sel_rest_country, "shinydashboard/dat/sel_rest_country.RDS")
                           
# Restriction measures and tooltips for countries
# rest_table created by helpers/restriction_labels.R
res_label_country <- lapply(sel_rest_country, function(x){
  x <- as.data.frame(x)
  colnames(x) <- "res_id"
  y <- merge(x, rest_table, by = "res_id", all.x = TRUE)
  # If label or tooltip is missing the id will be used
  y$res_text[which(is.na(y$res_text))] <- y$res_id[which(is.na(y$res_text))]
  y$res_label[which(is.na(y$res_label))] <- y$res_id[which(is.na(y$res_text))]
  y
})
saveRDS(res_label_country, "shinydashboard/dat/res_label_country.RDS")

#Deleting missing values from the beginning
rest_list <- lapply(rest_list, function(x) x[-which(apply(x[, -which(colnames(x) == "date")], 1, function(y) all(is.na(y)))),])

# Creating variables to calculate the x coordinates later for the visualization
rest_prev <- lapply(rest_list, function(x) cbind(x, rbind(rep(NA, ncol(x)), x[1:(nrow(x) - 1),])))
saveRDS(rest_prev, "shinydashboard/dat/rest_prev.RDS")


##Partial Dependece plots

#Creates input for the Partial Dependence Plots of the Random Forest train object for all countries (with FB data)


c_rf_dat_fb <- readRDS("shinydashboard/dat/c_rf_dat_fb.RDS")

##Functions for Shiny pdp data preparation

#Creates the train object per countries, then the pdp input for one country, all predictors

#Creates the train object

rf_train <- function(country_dat){
  country <- country_dat$country[1]
  rf_dat_t <- country_dat[complete.cases(country_dat),]
  
  # Train and test set
  #set.seed(9985)
  #to_train <- createDataPartition(rf_dat_t$cases_new_cum,
  #                                p = .8,
  #                                list = FALSE,
  #                                times = 1)
  
  #rf_train <- rf_dat_t[to_train,]
  #rf_test <- rf_dat_t[-to_train,]
  
  # RF
  ctrl <- trainControl(method = "timeslice",
                       initialWindow = 28,
                       horizon = 5,
                       fixedWindow = TRUE)
  
  grid <- expand.grid(mtry = c(round(sqrt(ncol(rf_dat_t))),
                               round(log(ncol(rf_dat_t)))))
  
  rf <- caret:: train(as.numeric(cases_new) ~ .,
              data = rf_dat_t[,-which(colnames(rf_dat_t) %in% c( "cases_new_cum", "date", "last_day", "last_week", "country"))],
              method = "rf",
              trControl = ctrl,
              tuneGrid = grid)
  
  
  return(rf)
}


#Function for Pdp input for one country, all of its predictors
pdp_all_pred <- function(countr){
  res_AUT <- list()
  for (i in seq_along(preds[[countr]])){
    res_AUT[[i]]<-  pdp::partial(rf_train[[countr]], pred.var = preds[[countr]][[i]])
  }
  return(res_AUT)
}



#Creates the RF train objects from the RF data
rf_train <- lapply(c_rf_dat_fb, function(x) rf_train(x))
saveRDS(rf_train, "shinydashboard/dat/rf_train.RDS")

#Get the predictors applied by countries
preds <- lapply(rf_train, function(x){ x <- predictors(x)
})

countries <- names(rf_train)

#Create Pdp input (yhats) for all countries, all of its predictors
pdp_all_c_all_pred <- lapply(countries, function(x) pdp_all_pred(x))

names(pdp_all_c_all_pred) <- countries


#Name the predictors within countries

preds[[1]] -> names(pdp_all_c_all_pred[[1]])
preds[[2]] -> names(pdp_all_c_all_pred[[2]])
preds[[3]] -> names(pdp_all_c_all_pred[[3]])
preds[[4]] -> names(pdp_all_c_all_pred[[4]])
preds[[5]] -> names(pdp_all_c_all_pred[[5]])
preds[[6]] -> names(pdp_all_c_all_pred[[6]])
preds[[7]] -> names(pdp_all_c_all_pred[[7]])
preds[[8]] -> names(pdp_all_c_all_pred[[8]])
preds[[9]] -> names(pdp_all_c_all_pred[[9]])
preds[[10]] -> names(pdp_all_c_all_pred[[10]])
preds[[11]] -> names(pdp_all_c_all_pred[[11]])
preds[[12]] -> names(pdp_all_c_all_pred[[12]])
preds[[13]] -> names(pdp_all_c_all_pred[[13]])
preds[[14]] -> names(pdp_all_c_all_pred[[14]])
preds[[15]] -> names(pdp_all_c_all_pred[[15]])
preds[[16]] -> names(pdp_all_c_all_pred[[16]])
preds[[17]] -> names(pdp_all_c_all_pred[[17]])
preds[[18]] -> names(pdp_all_c_all_pred[[18]])
preds[[19]] -> names(pdp_all_c_all_pred[[19]])
preds[[20]] -> names(pdp_all_c_all_pred[[20]])
preds[[21]] -> names(pdp_all_c_all_pred[[21]])

saveRDS(pdp_all_c_all_pred, "shinydashboard/dat/pdp_all_c_all_pred.RDS")




# Data preparetion for Random Forest Visualization
# Bump Chart: https://www.r-bloggers.com/2018/04/bump-chart/
# country_res: result of random forest

## Order of predictors and ranking of predictors within countries
b_dat <- lapply(country_res, function(x){
  x <- varImp(x)
  x$predictor <- rownames(x)
  x <- x[order(x$Overall, decreasing = TRUE),]
  x$ranking <- c(1:nrow(x))
  x$ranking[x$Overall == 0] <- NA
  x <- x[order(x$predictor),]
  x <- data.frame("ranking" = x$ranking,  row.names = x$predictor)
  x
})


# Ranking of predictors within countries
b_vis <- lapply(b_dat, function(x){
  r <- data.frame("ranking" = x$ranking, row.names = rownames(x))
  r
})
b_vis <- do.call("cbind", b_vis)
colnames(b_vis) <- names(b_dat)
b_vis <- b_vis[rowSums(is.na(b_vis)) != ncol(b_vis), ]

# Order of predictors (sorting predictors according to reciprocal of their ranks by their feature importance)
b_vis_r <- 1/b_vis
b_vis_r$ord <- rowSums(b_vis_r, na.rm = TRUE)
b_vis <- b_vis[order(b_vis_r$ord, decreasing = TRUE),]

# Removing last_day and last_week from bump chart
b_vis <- b_vis[-which(row.names(b_vis) %in% c("last_day", "last_week")),]

pred_order <- as.data.frame(cbind("predictor" = rownames(b_vis), "order" = 1:nrow(b_vis)))
saveRDS(pred_order, "shinydashboard/dat/pred_order.RDS")

# Creating long data for bump chart
b_vis$predictor <- rownames(b_vis)
b_vis_long <- as.data.frame(melt(setDT(b_vis), id.vars = c("predictor"), variable.name = "country"))
colnames(b_vis_long) <- c("predictor", "country", "ranking")


# Merging with prediction order
b_vis_long <- merge(b_vis_long, pred_order, by = "predictor", all.x = TRUE)
b_vis_long$order <- as.numeric(b_vis_long$order)
b_vis_long <- b_vis_long[order(b_vis_long$country, b_vis_long$order),]
b_vis_long <- merge(b_vis_long, all_pred_table[,c("pred_id", "pred_text")], by.x = "predictor", by.y = "pred_id")

saveRDS(b_vis_long, "shinydashboard/dat/pred_imp_ranking.RDS")

# Number of top predictors per countries (30 top predictors from pred_order)
top_pred_c <- b_vis_long[-which(is.na(b_vis_long$ranking)),]
n_top <- as.data.frame(top_pred_c[which(top_pred_c$predictor %in% pred_order$predictor[1:30]),] %>%
  group_by(country) %>%
  dplyr::summarise(n = n()))
summary(n_top)
                             
                             ## Reverse scaling the x axis for rug of pdp

# Min-max values for all variables in all countries
x_min_max <- list()
x_min_max <- lapply(country_list, function(x){
  lapply(x, function(y) if(is.numeric(y) & !allMissing(y)){
      c(min(y, na.rm = TRUE), max(y, na.rm = TRUE))
    }else{
      c(0,0)
    })
})
x_min_max <- lapply(x_min_max, function(x) do.call("rbind", x))
x_min_max <- lapply(names(x_min_max), function(x){
  x_min_max[[x]] <- cbind(as.data.frame(x_min_max[[x]]), "country" = x, "predictor" = row.names(x_min_max[[x]]))
})
x_min_max <- do.call("rbind", x_min_max)
colnames(x_min_max)[1:2] <- c("min", "max")
saveRDS(x_min_max, "shinydashboard/dat/x_min_max.RDS")


#
## Data preparation for 3D partial dependence
all_country <- names(country_res)
                    
##Function to calculate the object for the 3D Partial Dependence Plot
pdp_pred_paired <- function(countr, pred1, pred2){
  res <-  pdp::partial(countr, pred.var = c(pred1, pred2))
  return(res)
}

# Get selectable predictors for left and right vars
# Using rank of predictors calculated for the bump chart: b_dat
# 10 top predictors will be selectable per country
# From vaccination only kept one -> People Vaccinated Per Hundred will be removed
get_selectable <- function(country, no_top_predictors)
{
  all_pred <- b_dat[[country]]
  all_pred <- all_pred[-which(rownames(all_pred) == "people_vaccinated_per_hundred")]
  all_pred <- cbind("pred" = rownames(all_pred), all_pred)
  # Remove variables that are not in the training data
  all_pred <- all_pred[-which(all_pred$pred %nin% colnames(rf_train[[country]]$trainingData)),]
  sel_pred <- all_pred[order(all_pred$ranking),]
  sel_pred <- merge(sel_pred, all_pred_table, by.x = "pred", by.y = "pred_id")
  sel_pred <- sel_pred[order(sel_pred$ranking),]
  all_choices <- sel_pred$pred_text
  des_left <- c("Average Daily Temperature", "COVID-like Illnes", "Mask Coverage", "People Fully Vaccinated Per Hundred")
  left_vars <- des_left[which(des_left %in% sel_pred$pred_text)]
  right_vars <- c(left_vars, all_choices[-which(all_choices %in% left_vars)])[1:no_top_predictors]
  
  des_left_id = c("tavg", "fb_data.pct_covid_cli", "fb_data.percent_mc", "people_fully_vaccinated_per_hundred")
  left_vars_id <- des_left_id[which(des_left_id %in% sel_pred$pred)]
  right_vars_id = c(c(left_vars_id, sel_pred$pred[-which(sel_pred$pred %in% left_vars_id)])[1:no_top_predictors])
  return(as.data.frame(cbind("left_vars" = left_vars, "right_vars" = right_vars, "left_vars_id" = left_vars_id,
                             "right_vars_id" = right_vars_id)))  
}

selectable_ctr <- list()
selectable_ctr <- lapply(all_country, function(x){
  get_selectable(x, 10)
})
names(selectable_ctr) <- all_country

# Function to create pdp object
pdp_3d_object <- function(pred_id1, pred_id2, country){
  predx <- pred_id1
  predy <- pred_id2
  object <- pdp_pred_paired(rf_train[[country]], predx, predy)
  
  cat("Object calculated")
  
  #dens <- akima::interp(y = object[,predx], x = object[,predy], z = object$yhat)
  
  #return(dens)
  return(object)
}

# Function to calculate 3d pdp object for all predictor combinations for a country
ctr_pdp_3d_object <- function(country)
{
  left_vars <- selectable_ctr[[country]]$left_vars
  right_vars <- selectable_ctr[[country]]$right_vars
  left_vars_id <- selectable_ctr[[country]]$left_vars_id
  right_vars_id <- selectable_ctr[[country]]$right_vars_id
  
  pred_table <- expand.grid(left_vars_id, right_vars_id)
  pred_table <- pred_table[!duplicated(t(apply(pred_table, 1, sort))),]
  pred_table <- pred_table[-which(as.character(pred_table$Var1) == as.character(pred_table$Var2)),]
  
  combi_names <- paste(pred_table$Var1, pred_table$Var2, sep = "*")
  
  ctr_3d <- list()
  ctr_3d <- lapply(combi_names, function(x){
    preds <- strsplit(x, split = "\\*")
    pred1 = preds[[1]][1]
    pred2 = preds[[1]][2]
    return(pdp_3d_object(pred1, pred2, country))
  })
  names(ctr_3d) <- combi_names
  
  return(ctr_3d)
}

pdp_3d_country <- list()
pdp_3d_country <- lapply(all_country, function(x){
  ctr_pdp_3d_object(x)
})

names(pdp_3d_country) <- all_country

saveRDS(pdp_3d_country, "shinydashboard/dat/pdp_3d_country.RDS")
saveRDS(selectable_ctr, "shinydashboard/dat/selectable_3d_country.RDS")

