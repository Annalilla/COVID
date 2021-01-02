library(tidyverse)
library(eurostat)
library(coronavirus)
library(lubridate)
library(rnoaa)
library(jsonlite)
library(httr)

source("functions/Data_preparation_functions.R")
source("data_collection/Save_data.R")

old_tdata <- read.csv("data/tdata.csv")

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
new_testing <- subset(new_testing, select = -c(country_code))

# Replace NAs with new values
tdata <- replace_old(new_testing, vars_to_drop = c("country", "year", "week"),
                     vars_to_merge = c("country", "year", "week"))

#
# Covid cases
data_untill <- old_tdata[!(is.na(old_tdata$cases_new)),] %>%
  group_by(country) %>%
  summarise(maxdate = max(date))
mindate_c <- as.Date(min(data_untill$maxdate)) + 1

covid <- refresh_coronavirus_jhu()%>%
  group_by(location, date, data_type) %>%
  summarise(cases = sum(value)) %>%
  filter(location %in% capitals$country & date >= mindate_c)

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
indi <- c("covid", "mask", "contact")
fb <- data.frame()
for(i in unique(capitals$country_fb)){
  act_signal <- data.frame()
  for(j in indi)
  {
    path <- paste("https://covidmap.umd.edu/api/resources?indicator=", j, "&type=daily&country=", i, "&daterange=", rangefrom, "-",
                  rangeto, sep = "")
    request <- GET(url = path)
    fb_response <- content(request, as = "text", encoding = "UTF-8")
    fb_content <- jsonlite::fromJSON(fb_response, flatten = TRUE)
    if(length(fb_content[[1]]) > 0){
      if(nrow(act_signal) == 0){
        act_signal <- data.frame(fb_content)
      }else{
        act_signal <- cbind(act_signal, data.frame(fb_content))
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

tdata <- tdata[names(old_tdata)]
tdata <- tdata[order(tdata$country, tdata$date),]

save_data(tdata, datname = "tdata", dirname = "data", archieve = FALSE)
