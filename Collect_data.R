library(tidyverse)
library(eurostat)
library(jsonlite)
library(data.table)
library(httr)
library(dplyr)
library(ggplot2)
library(rnoaa)

setwd("C:/Stuff/MDM/Master/R")

capitals <- data.frame("country" = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Chechia", "Denmark", "Estonia", "Finland", "France",
                                     "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
                                     "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden"),
                       "country_code" = c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU", "IE", "IT", "LVA",
                                          "LT", "LU", "MT", "NL", "POL", "PT", "RO", "SK", "SI", "ES", "SE"),
                       "Capital" = c("Vienna", "Brussels", "Sofia", "Zagreb", "Nicosia", "Prague", "Copenhagen", "Tallinn", "Helsinki", "Paris",
                                     "Berlin", "Athens", "Budapest", "Dublin", "Rome", "Riga", "Vilnius", "Luxembourg", "Valletta", "Amsterdam", "Warsaw",
                                     "Lisbon", "Bucharest", "Bratislava", "Ljubljana", "Madrid", "Stockholm"),
                       "Code" = c("AU000006", "BE000002", "BU000002", "HR000002", "CY000002", "EZ000006", "DA000003", "EN000001", "FI000001", "FR000018",
                                  "GM000001", "GR000001", "HU000002", "EI000002", "IT000015", "LG000001", "LH000001", "LU000001", "IT000012", "NL000002", "PL000042",
                                  "PO000006", "RO000009", "LO000001", "SI000001", "SP000006", "SW000009"),
                       "Longitudes" = c("16.3725042", "4.351697", "23.3221789", "15.977048", "33.364726", "14.4212535", "12.5700724", "24.7453688", "24.9425769",
                                        "2.3514616", "13.3888599", "23.7283052", "19.0404707", "-6.2602732", "12.4829321", "-77.87633792300464", "25.2829111",
                                        "6.129799", "14.5136759", "4.8936041", "21.0067249", "-9.1365919", "26.1027202", "17.1093063", "14.5068602", "-3.7035825",
                                        "18.0710935"),
                       "Latitudes" = c("48.2083537", "50.8465573", "42.6978634", "45.813177", "35.1739302", "50.0874654", "55.6867243", "59.4372155", "60.1674098",
                                       "48.8566969", "52.5170365", "37.9839412", "47.4983815", "53.3497645", "41.8933203", "43.07720535", "54.6870458",
                                       "49.6112768", "35.8989818", "52.3727598", "52.2319581", "38.7077507", "44.4361414", "48.1516988", "46.0499803", "40.4167047",
                                       "59.3251172"),
                       "Station" = c("AU000005901", "BE000006447", "BUM00015614", "HR000142360", "CY000176090", "EZM00011520", "DA000032020", "EN000026038", "FIE00142101", "FR000007150",
                                     "GME00121150", "GR000016716", "HUM00012843", "EI000003969", "IT000016239", "USW00014768", "LH000026730", "LU000006590", "MT000016597", "NLM00006260",
                                     "PLM00012375", "PO000008535", "ROE00108889", "AU000005901", "SIM00014015", "SP000003195", "SWM00002589"))

# eurostat
# Access to eurostat
# check_access_to_data()

eu_countries

pop_eurostat <- get_eurostat("demo_pjangroup", time_format = "num")
physicians_eurostat <- get_eurostat("hlth_rs_spec", time_format = "num")
health_expenditures_eurostat <- get_eurostat("hlth_sha11_hc", time_format = "num")

# Relabel
physicians_eurostat$med_spec <- label_eurostat(physicians_eurostat)$med_spec
health_expenditures_eurostat$icha11_hc <- label_eurostat(health_expenditures_eurostat)$icha11_hc

# Last year available
pop_eurostat <- subset(pop_eurostat, time == max(pop_eurostat$time))
physicians_eurostat <- subset(physicians_eurostat, time == max(physicians_eurostat$time))
health_expenditures_eurostat <- subset(health_expenditures_eurostat, time == max(health_expenditures_eurostat$time))


# Response measures and testing
url_response <- "https://www.ecdc.europa.eu/sites/default/files/documents/data_response_graphs_0.csv"
url_testing <- "https://opendata.ecdc.europa.eu/covid19/testing/csv"

response <- fread(url_response)
testing <- fread(url_testing)

# Covid cases
# Post to API
payload <- list(code = "ALL")
response <- httr::POST(url = "https://api.statworx.com/covid",
                       body = toJSON(payload, auto_unbox = TRUE), encode = "json")

# Convert to data frame
content <- rawToChar(response$content)
covid <- data.frame(fromJSON(content))

test <- subset(covid, covid$code %in% pop_eurostat$geo)
length(table(test$code)) # Should be 27

# Weather
library(rnoaa)

maxdate <- Sys.Date() - 1

weather <- list()

for(i in 1:nrow(capitals)){
  act_res <- ghcnd_search(stationid = capitals$Station[i], date_min = "2019-12-31",date_max = maxdate, var = "tavg", refresh = TRUE)[[1]]
  weather[[capitals$country_code[i]]] <- act_res
}
