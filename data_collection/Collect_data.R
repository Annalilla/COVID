library(tidyverse)
library(eurostat)
library(coronavirus)
library(data.table)
library(httr)
library(dplyr)
library(rnoaa)
library(jsonlite)

# eurostat
# Access to eurostat
# check_access_to_data()

eu_countries

pop_eurostat <- get_eurostat("demo_pjangroup", time_format = "num")
physicians_eurostat <- get_eurostat("hlth_rs_spec", time_format = "num")
health_expenditures_eurostat <- get_eurostat("hlth_sha11_hc", time_format = "num")
cultural_participation_eurostat <- get_eurostat("ilc_scp03", time_format = "num")

# Relabel
physicians_eurostat$med_spec <- label_eurostat(physicians_eurostat)$med_spec
health_expenditures_eurostat$icha11_hc <- label_eurostat(health_expenditures_eurostat)$icha11_hc
cultural_participation_eurostat$acl00 <- label_eurostat(cultural_participation_eurostat)$acl00

# Last year available
pop_eurostat <- subset(pop_eurostat, time == 2019) # 2019
physicians_eurostat <- subset(physicians_eurostat, time == 2019) # 2019
health_expenditures_eurostat <- subset(health_expenditures_eurostat, time == 2018) # 2018
cultural_participation_eurostat <- subset(cultural_participation_eurostat, time == 2015) #

#
# Response measures and testing
url_response <- "https://www.ecdc.europa.eu/sites/default/files/documents/data_response_graphs_0.csv"
url_testing <- "https://opendata.ecdc.europa.eu/covid19/testing/csv"

response <- fread(url_response)
testing <- fread(url_testing)

# Covid cases
# Post to API
covid <- refresh_coronavirus_jhu()%>%
  group_by(location, date, data_type) %>%
  summarise(cases = sum(value)) %>%
  filter(location %in% capitals$country)

# Weather
weather <- list()

for(i in 1:nrow(capitals)){
  act_res <- ghcnd_search(stationid = capitals$Station[i], date_min = "2019-12-31", date_max = maxdate, var = "tavg", refresh = TRUE)[[1]]
  weather[[capitals$country_code[i]]] <- act_res
}

# FB
rangeto <- str_replace_all(as.character(maxdate), "-", "")
indi <- c("covid", "mask", "contact")
fb <- data.frame()
for(i in unique(capitals$country_fb)){
  act_signal <- data.frame()
  for(j in indi)
  {
    path <- paste("https://covidmap.umd.edu/api/resources?indicator=", j, "&type=daily&country=", i, "&daterange=20200227-",
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

# Some countries are missing:
#capitals$country_fb[which(!(str_replace_all(capitals$country_fb, "_", " ") %in% unique(fb$data.country)))]

# MIT
# Available only for countries:
available <- c("US", "BR", "MX", "IN", "GB", "JP", "DE", "IT", "AR", "ID", "CO", "TR", "FR", "EG", "VN", "PH", "PK", "BD", "RO", "PL", "TH", "MY", "NG")
capitals$country_code_iso2[which(capitals$country_code_iso2 %in% available)]

