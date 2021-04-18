# Prepares the data for the dashboard to enable fast visualisation and to improve interactivity.

library(zoo)
library(tidyverse)
library(data.table)

dir.create("shinydashboard", showWarnings = FALSE)
dir.create("shinydashboard/dat", showWarnings = FALSE)

# Smooth variables as new variable in tdata: cases, tavg
country_list <- split(tdata, tdata$country)
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_cases" = rollmean(x$cases_new, 7, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_tavg" = rollmean(x$tavg, 7, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_fb_cli" = rollmean(x$fb_data.percent_cli, 7, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_fb_mc" = rollmean(x$fb_data.percent_mc, 7, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_fb_dc" = rollmean(x$fb_data.percent_dc, 7, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_fb_cli_se" = rollmean(x$fb_data.cli_se, 7, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_fb_mc_se" = rollmean(x$fb_data.mc_se, 7, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_fb_dc_se" = rollmean(x$fb_data.dc_se, 7, fill = NA)))
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_people_vaccinated_per_hundred" = rollmean(x$people_vaccinated_per_hundred, 7, fill = NA)))
saveRDS(country_list, "shinydashboard/dat/country_list.RDS")

# Maximum limit of y (list of maximum limits for absolute number and smoothed number of cases for all countries)
multi <- 1.05
y_limit_list <- lapply(country_list, function(x)
  cbind("y_limit_abs" = multi * max(x$cases_new, na.rm = TRUE),
        "y_limit_smo" = multi * max(x$smoothed_cases, na.rm = TRUE),
        "y_min_abs_temp" = multi * min(x$tavg, na.rm = TRUE) * (max(x$cases, na.rm = TRUE)/max(x$tavg, na.rm = TRUE)),
        "y_min_smo_temp" = multi * min(x$smoothed_tavg, na.rm = TRUE) * (max(x$cases, na.rm = TRUE)/max(x$tavg, na.rm = TRUE))))
saveRDS(y_limit_list, "shinydashboard/dat/y_limit_list.RDS")

# Restrictions 

# Restriction variables are binary
rest_names <- colnames(tdata[, unlist(lapply(tdata, function(x) all(na.omit(x) %in% c(0, 1))))])
saveRDS(rest_names, "shinydashboard/dat/rest_names.RDS")

# Selecting applied restrictions in all countries
rest_list <- lapply(country_list, function(x) select(x, c("date", rest_names)))
rest_list <- lapply(rest_list, function(x) cbind("date" = x$date, as.data.frame(apply(x[,-which(colnames(x) == "date")], 2, as.numeric))))
rest_list <- lapply(rest_list, function(x) cbind("date" = x$date, x[,-which(colnames(x) == "date")][, which(colSums(x[,-which(colnames(x) == "date")], na.rm = TRUE) > 0)]))
sel_rest_country <- lapply(rest_list, function(x) colnames(x[,-which(colnames(x) == "date")]))
saveRDS(sel_rest_country, "shinydashboard/dat/sel_rest_country.RDS")

#Deleting missing values from the beginning
rest_list <- lapply(rest_list, function(x) x[-which(apply(x[, -which(colnames(x) == "date")], 1, function(y) all(is.na(y)))),])

# Creating variables to calculate the x coordinates later for the visualization
rest_prev <- lapply(rest_list, function(x) cbind(x, rbind(rep(NA, ncol(x)), x[1:(nrow(x) - 1),])))
saveRDS(rest_prev, "shinydashboard/dat/rest_prev.RDS")


##Pdp

#Creates the RF train objects and inputs (x and y axis) for pdps per countries for one indicator (tavg)
pdp_input <- lapply(c_rf_dat_fb, function(x) pdp_input(x))
# : preprocessed data, countries removed without fb data, split by country ready for rf

saveRDS(pdp_input, "shinydashboard/dat/pdp_input.RDS")


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

# Order of predictors (sorting predictors according to number of countries, where predictor is 1. 2. or 3. most important)
b_vis$no_1 <- apply(b_vis, 1, function(x) length(which(x == 1)))
b_vis$no_2 <- apply(b_vis, 1, function(x) length(which(x == 2)))
b_vis$no_3 <- apply(b_vis, 1, function(x) length(which(x == 3)))
b_vis$no_4 <- apply(b_vis, 1, function(x) length(which(x == 4)))
b_vis$no_5 <- apply(b_vis, 1, function(x) length(which(x == 5)))

b_vis <- b_vis[order(b_vis$no_1, b_vis$no_2, b_vis$no_3, b_vis$no_4, b_vis$no_5, decreasing = TRUE),]

pred_order <- as.data.frame(cbind("predictor" = rownames(b_vis), "order" = 1:nrow(b_vis)))
saveRDS(pred_order, "shinydashboard/dat/pred_order.RDS")

b_vis <- b_vis[,-which(grepl("no_", colnames(b_vis)))]

# Creating long data for bump chart
b_vis$predictor <- rownames(b_vis)
b_vis_long <- as.data.frame(melt(setDT(b_vis), id.vars = c("predictor"), variable.name = "country"))
colnames(b_vis_long) <- c("predictor", "country", "ranking")

# Merging with prediction order
b_vis_long <- merge(b_vis_long, pred_order, by = "predictor", all.x = TRUE)
b_vis_long$order <- as.numeric(b_vis_long$order)
b_vis_long <- b_vis_long[order(b_vis_long$country, b_vis_long$order),]
saveRDS(b_vis_long, "shinydashboard/dat/pred_imp_ranking.RDS")

# Number of top predictors per countries (30 top predictors from pred_order)
top_pred_c <- b_vis_long[-which(is.na(b_vis_long$ranking)),]
n_top <- as.data.frame(top_pred_c[which(top_pred_c$predictor %in% pred_order$predictor[1:30]),] %>%
  group_by(country) %>%
  summarise(n = n()))
summary(n_top)

