library(tidyverse)
library(eurostat)
library(coronavirus)
library(lubridate)
library(rnoaa)
library(jsonlite)
library(httr)
library(data.table)

source("functions/Data_preparation_functions.R")
source("functions/Data_cleansing_functions.R")
source("data_collection/Save_data.R")

old_tdata <- get_data("tdata")

# Functions to replace missing values with the new values
replace_old <- function(new_data, vars_to_drop, vars_to_merge = NA, vars_to_merge_x = NA, vars_to_merge_y = NA)
{
  if(nrow(new_data) > 0){
    vars_to_change <- colnames(new_data)
    vars_to_change <- vars_to_change[-(which(vars_to_change %in% vars_to_drop))]
    if(is.na(vars_to_merge)[1]){
      new <- merge(tdata, new_data, by.x = vars_to_merge_x, by.y = vars_to_merge_y, all.x = TRUE)
    }else{
      new <- merge(tdata, new_data, by = vars_to_merge, all.x = TRUE)
    }
    for(i in 1:length(vars_to_change)){
      new <- coalesce_vars(vars_to_change[i], new)
    }
    return(new)
  }else{
    return(tdata)
  }
}

coalesce_vars <- function(varname, data){
  varname_x <- paste(varname, "x", sep = ".")
  varname_y <- paste(varname, "y", sep = ".")
  if(is.numeric(data[,which(colnames(data) == varname_x)])){
    data[,which(colnames(data) == varname_y)] <- as.numeric(data[,which(colnames(data) == varname_y)])
  }
  act <- coalesce(data[,which(colnames(data) == varname_x)], data[,which(colnames(data) == varname_y)])
  act_dat <- cbind(data, act)
  colnames(act_dat)[ncol(act_dat)] <- varname
  act_dat <- act_dat[, -c(which(colnames(act_dat) %in% c(varname_x, varname_y)))]
  return(act_dat)
}


# Complete old dataset with new date
new_rows <- as.data.frame(matrix(nrow = (length(unique(old_tdata$country)) * (as.numeric(as.Date(maxdate) - as.Date(max(old_tdata$date))))),
                                         ncol = ncol(old_tdata)))
colnames(new_rows) <- colnames(old_tdata)
new_date <- as.character(seq(as.Date(max(old_tdata$date)) + 1, as.Date(maxdate), by="days"))
new_rows$date <- rep(new_date, length(unique(old_tdata$country)))
new_rows$country <- rep(unique(old_tdata$country), each = length(new_date))

tdata <- rbind(old_tdata, new_rows)

# Adding year and week number
tdata$year[which(is.na(tdata$year))] <- str_extract(tdata$date[which(is.na(tdata$year))], "^\\d+")
tdata$week[which(is.na(tdata$week))] <- strftime(tdata$date[which(is.na(tdata$week))], format = "%V")

# Adding country_codes
tdata <- tdata %>%
  merge(capitals[, c("country", "country_code")], by ="country", all.x = TRUE) %>%
  mutate(country_code.x = country_code.y) %>%
  subset(select = -c(country_code.y))
colnames(tdata)[colnames(tdata) == "country_code.x"] <- "country_code"

# Changing variable types
source("helpers/Change_variable_types.R")

#
# Response measurements
data_untill <- old_tdata[!(is.na(old_tdata$AdaptationOfWorkplace)),] %>%
  group_by(country) %>%
  summarise(maxdate = max(date))
mindate_r <- as.Date(min(data_untill$maxdate)) + 1

url_response <- "https://www.ecdc.europa.eu/sites/default/files/documents/data_response_graphs_0.csv"
response <- fread(url_response)

new_response <- prepare_response(response, rangefrom = mindate_r)
new_response <- subset(new_response, select = -c(year, week))

# Replace NAs with new values
tdata <- replace_old(new_response, vars_to_drop = c("Country", "date"),
                        vars_to_merge = NA, vars_to_merge_x = c("country", "date"), vars_to_merge_y = c("Country", "date"))

#
# Testing
data_untill <- old_tdata[!(is.na(old_tdata$testing_new_cases)),] %>%
  group_by(country) %>%
  summarise(maxdate = max(date))
mindate_t <- as.Date(min(data_untill$maxdate)) + 1
minweek_t <-  strftime(mindate_t, format = "%V")

url_testing <- "https://opendata.ecdc.europa.eu/covid19/testing/csv"
testing <- fread(url_testing)

new_testing <- prepare_testing(testing, rangefrom = minweek_t)
new_testing <- clean_testing(new_testing)
new_testing <- subset(new_testing, select = -c(country_code))

# Replace NAs with new values
tdata <- replace_old(new_testing, vars_to_drop = c("country", "year", "week"),
                     vars_to_merge = c("country", "year", "week"))

#
# Covid cases
data_untill <- old_tdata[!(is.na(old_tdata$confirmed)),] %>%
  group_by(country) %>%
  summarise(maxdate = max(date))
mindate_c <- as.Date(min(data_untill$maxdate)) + 1

covid <- coronavirus %>%
  group_by(country, date, type) %>%
  summarise(cases = sum(cases)) %>%
  filter(country %in% capitals$country & date >= mindate_c)

covid <- prepare_covid(covid)

# Replace NAs with new values
tdata <- replace_old(covid, vars_to_drop = c("country", "date"),
                     vars_to_merge = c("country", "date"))


#
#Weather
data_untill <- old_tdata[!(is.na(old_tdata$tavg)),] %>%
  group_by(country) %>%
  summarise(maxdate = max(date))
mindate_w <- as.character(as.Date(min(data_untill$maxdate)) + 1)

weather <- list()

for(i in 1:nrow(capitals)){
  act_res <- ghcnd_search(stationid = capitals$Station[i], date_min = mindate_w, date_max = maxdate, var = "tavg", refresh = TRUE)[[1]]
  weather[[capitals$country_code[i]]] <- act_res
}

tempavg <- prepare_weather(weather)
tempavg <- clean_weather(tempavg)

# Replace NAs with new values
tdata <- replace_old(tempavg, vars_to_drop = c("country_code", "date"),
                     vars_to_merge = c("country_code", "date"))

# FB
data_untill <- old_tdata[!(is.na(old_tdata$fb_data.iso_code)),] %>%
  group_by(country) %>%
  summarise(maxdate = max(date))
mindate_fb <- as.character(as.Date(min(data_untill$maxdate)) + 1)
rangefrom <- str_replace_all(as.character(mindate_fb), "-", "")
rangeto <- str_replace_all(as.character(maxdate), "-", "")
type_mark <- c("cli", "mc", "dc")
merge_vars <- c("data.country", "data.iso_code", "data.gid_0", "data.survey_date",  "status")
indi <- c("covid", "mask", "contact")
fb <- data.frame()
for(i in unique(capitals$country_fb)){
  act_signal <- data.frame()
  for(j in indi)
  {
    # Daily
    path_daily <- paste("https://covidmap.umd.edu/api/resources?indicator=", j, "&type=daily&country=", i, "&daterange=", rangefrom, "-",
                        rangeto, sep = "")
    request_daily <- GET(url = path_daily)
    fb_response_daily <- content(request_daily, as = "text", encoding = "UTF-8")
    fb_content_daily <- jsonlite::fromJSON(fb_response_daily, flatten = TRUE)
    
    # Smoothed
    path_smoothed <- paste("https://covidmap.umd.edu/api/resources?indicator=", j, "&type=smoothed&country=", i, "&daterange=", rangefrom, "-",
                           rangeto, sep = "")
    request_smoothed <- GET(url = path_smoothed)
    fb_response_smoothed <- content(request_smoothed, as = "text", encoding = "UTF-8")
    fb_content_smoothed <- jsonlite::fromJSON(fb_response_smoothed, flatten = TRUE)
    
    # If non of the data is available
    if(length(fb_content_daily$data) == 0 & length(fb_content_smoothed$data) == 0){
      fb_content <- data.frame()
    }
    
    # If smoothed data available
    if(length(fb_content_smoothed$data) > 0){
      fb_content_smoothed <- data.frame(fb_content_smoothed)
      #Mark variable names with smoothed (if not yet marked)
      marked_smoothed <- unique(c(which(colnames(fb_content_smoothed) %in% merge_vars),
                                  which(grepl("smoothed", colnames(fb_content_smoothed)))))
      colnames(fb_content_smoothed)[-marked_smoothed] <-
        paste(colnames(fb_content_smoothed)[-marked_smoothed], "smoothed", sep ="_")
      
      # If smoothed and daily data available
      if(length(fb_content_daily$data) > 0){
        fb_content_daily <- data.frame(fb_content_daily)
        fb_content <- merge(fb_content_daily, fb_content_smoothed,
                            by = merge_vars, all = TRUE)
      }else{
        # If only smoothed data available
        fb_content <- fb_content_smoothed
      }
    }else if(length(fb_content_daily$data) > 0){
      # If only daily data available
      fb_content <- data.frame(fb_content_daily)
    }
    
    if(nrow(fb_content) > 0){
      # Mark variables with indicators (if not yet marked)
      marked_indi <- unique(c(which(colnames(fb_content) %in% merge_vars),
                              which(grepl(type_mark[match(j, indi)], colnames(fb_content)))))
      colnames(fb_content)[-marked_indi] <-
        paste(colnames(fb_content)[-marked_indi], type_mark[match(j, indi)], sep ="_")
      
      if(nrow(act_signal) == 0){
        act_signal <- fb_content
      }else{
        act_signal <- merge(act_signal, fb_content,
                            by = merge_vars, all = TRUE)
      }
    }
  }
  if(nrow(act_signal) > 0){
    fb <- rbind(fb, act_signal)
  }
}
fb <- prepare_fb(fb)

# Replace NAs with new values
tdata <- replace_old(fb, vars_to_drop = c("country_code", "date"),
                     vars_to_merge = c("country_code", "date"))

# Vaccination
url_vaccin <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
vaccination <- fread(url_vaccin)

vaccination <- prepare_vaccination(vaccination)

# Replace NAs with new values
tdata <- replace_old(vaccination, vars_to_drop = c("country", "date"),
                     vars_to_merge = c("country", "date"))

tdata <- tdata[names(old_tdata)]
tdata <- tdata[order(tdata$country, tdata$date),]

# Save data local or to google sheets
save_data(tdata, datname = "tdata", dirname = "data", archieve = TRUE)
save_data_online(tdata, datname = "tdata", archieve = TRUE)
