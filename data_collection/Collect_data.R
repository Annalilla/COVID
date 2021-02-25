# Downloads data from all sources listed in the documentation

library(tidyverse)
library(eurostat)
library(coronavirus)
library(data.table)
library(httr)
library(dplyr)
library(rnoaa)
library(jsonlite)
library(rvest)

# eurostat
# Access to eurostat
# check_access_to_data()

eu_countries

pop_eurostat <- get_eurostat("demo_pjangroup", time_format = "num")

#As physicians data is available for only 6 countries, we do not use them.
#physicians_eurostat <- get_eurostat("hlth_rs_spec", time_format = "num")

health_expenditures_eurostat <- get_eurostat("hlth_sha11_hc", time_format = "num")

cultural_participation_eurostat <- get_eurostat("ilc_scp03", time_format = "num")

# Relabel
#physicians_eurostat$med_spec <- label_eurostat(physicians_eurostat)$med_spec
health_expenditures_eurostat$icha11_hc <- label_eurostat(health_expenditures_eurostat)$icha11_hc
cultural_participation_eurostat$acl00 <- label_eurostat(cultural_participation_eurostat)$acl00

# Last year available
pop_eurostat <- subset(pop_eurostat, time == 2019) # 2019
#physicians_eurostat <- subset(physicians_eurostat, time == 2019) # 2019
health_expenditures_eurostat <- subset(health_expenditures_eurostat, time == 2018) # 2018
cultural_participation_eurostat <- subset(cultural_participation_eurostat, time == 2015) # 2015

#
# Response measures and testing
response_page <- read_html("https://www.ecdc.europa.eu/en/publications-data/download-data-response-measures-covid-19")
url_response <- response_page %>% html_nodes(xpath = "//*[contains(text(), 'Download data on country response measures')]") %>% html_attr('href')
url_testing <- "https://opendata.ecdc.europa.eu/covid19/testing/csv"

response <- fread(url_response)
testing <- fread(url_testing)

# Covid cases
covid <- refresh_coronavirus_jhu()%>%
  group_by(location, date, data_type) %>%
  summarise(cases = sum(value)) %>%
  filter(location %in% capitals$country)

#data("coronavirus")
#covid <- coronavirus %>%
#    group_by(country, date, type) %>%
#    summarise(cases = sum(cases)) %>%
#    filter(country %in% capitals$country)

# Weather
weather <- list()

for(i in 1:nrow(capitals)){
  act_res <- ghcnd_search(stationid = capitals$Station[i], date_min = "2019-12-31", date_max = maxdate, var = "tavg", refresh = TRUE)[[1]]
  weather[[capitals$country_code[i]]] <- act_res
}

# FB
rangeto <- str_replace_all(as.character(maxdate), "-", "")
indi <- c("covid", "mask", "contact")
type_mark <- c("cli", "mc", "dc")
merge_vars <- c("data.country", "data.iso_code", "data.gid_0", "data.survey_date",  "status")
fb <- data.frame()
for(i in unique(capitals$country_fb)){
  act_signal <- data.frame()
  for(j in indi)
  {
    # Daily
    path_daily <- paste("https://covidmap.umd.edu/api/resources?indicator=", j, "&type=daily&country=", i, "&daterange=20200227-",
                  rangeto, sep = "")
    request_daily <- GET(url = path_daily)
    fb_response_daily <- content(request_daily, as = "text", encoding = "UTF-8")
    fb_content_daily <- jsonlite::fromJSON(fb_response_daily, flatten = TRUE)
    
    # Smoothed
    path_smoothed <- paste("https://covidmap.umd.edu/api/resources?indicator=", j, "&type=smoothed&country=", i, "&daterange=20200227-",
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

####*****##########* fb variable test

path <- "https://covidmap.umd.edu/api/resources?indicator=contact&type=daily&country=Austria&daterange=20200227-20210120"
request <- GET(url = path)
fb_response <- content(request, as = "text", encoding = "UTF-8")
fb_content <- jsonlite::fromJSON(fb_response, flatten = TRUE)
colnames(fb_content$data)
# colnames with data.
#colnames(data.frame(fb_content))

####*####*****######

# Some countries are missing:
#capitals$country_fb[which(!(str_replace_all(capitals$country_fb, "_", " ") %in% unique(fb$data.country)))]

# MIT
# Available only for countries:
available <- c("US", "BR", "MX", "IN", "GB", "JP", "DE", "IT", "AR", "ID", "CO", "TR", "FR", "EG", "VN", "PH", "PK", "BD", "RO", "PL", "TH", "MY", "NG")
capitals$country_code_iso2[which(capitals$country_code_iso2 %in% available)]

# Vaccination
# https://github.com/owid/covid-19-data/tree/master/public/data
url_vaccin <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
vaccination <- fread(url_vaccin)
