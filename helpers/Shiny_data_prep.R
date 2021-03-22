# Prepares the data for the dashboard to enable fast visualisation and to improve interactivity.

dir.create("shinydashboard/dat", showWarnings = FALSE)

# tdata
saveRDS(tdata, "shinydashboard/dat/tdata.RDS")

# Smooth cases (as new variable in tdata)
country_list <- split(tdata, tdata$country)
country_list <- lapply(country_list, function(x) cbind(x, "smoothed_cases" = rollmean(x$cases_new, 7, fill = NA)))
saveRDS(country_list, "shinydashboard/dat/country_list.RDS")

# Maximum limit of y (list of maximum limits for absolute number and smoothed number of cases for all countries)
multi <- 1.05
y_limit_list <- lapply(country_list, function(x) cbind("y_limit_abs" = multi * max(x$cases_new, na.rm = TRUE), "y_limit_smo" = multi * max(x$smoothed_cases, na.rm = TRUE)))
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

#Deleting missing values from the beginning
rest_list <- lapply(rest_list, function(x) x[-which(apply(x[, -which(colnames(x) == "date")], 1, function(y) all(is.na(y)))),])

# Creating the x coordinates for the rectangles to display restrictions
rest_prev <- lapply(rest_list, function(x) cbind(x, rbind(rep(NA, ncol(x)), x[1:(nrow(x) - 1),])))
saveRDS(rest_prev, "shinydashboard/dat/rest_prev.RDS")

