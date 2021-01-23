library(lubridate)

source("functions/Data_preparation_functions.R")
source("functions/Data_cleansing_functions.R")

### Country characteristics

#
# pop_eurostat

total <- pop_eurostat[pop_eurostat$age == "TOTAL" & pop_eurostat$sex == "T",] %>%
  group_by(geo, time) %>%
  summarise(values = sum(values))
total$variable <- "Total"

sex <- pop_eurostat[pop_eurostat$age == "TOTAL",] %>%
  group_by(geo, sex, time) %>%
  summarise(values = sum(values))
colnames(sex)[colnames(sex) == "sex"] <- "variable"
sex <- sex[,c("geo", "time", "values", "variable")]
sex <- filter(sex, variable %in% c("F", "M"))

# Age groups

# Values of agecategories "unknown" if sex is "total"  always 0 in eu countries 
#pop_eurostat[which(pop_eurostat$age == "UNK" & pop_eurostat$sex == "T"
#                   & pop_eurostat$values != 0 & pop_eurostat$geo %in% capitals$country_code_iso2),]

age <- pop_eurostat[pop_eurostat$sex == "T" &
                      pop_eurostat$age %in% c("Y10-14", "Y15-19", "Y20-24", "Y25-29", "Y30-34", "Y35-39",
                                              "Y40-44", "Y45-49", "Y5-9", "Y50-54", "Y55-59", "Y60-64","Y65-69",
                                              "Y70-74", "Y75-79", "Y80-84", "Y_GE75", "Y_GE80", "Y_GE85", "Y_LT5"),] %>%
  group_by(geo, age, time) %>%
  summarise(values = sum(values))
colnames(age)[colnames(age) == "age"] <- "variable"
age <- age[,c("geo", "time", "values", "variable")]

#
# physicians_eurostat
physicians <- physicians_eurostat %>%
  group_by(geo, time) %>%
  summarise(values = sum(values))
physicians$variable <- "physicians"

#
# health_expenditures_eurostat
health_exp <- health_expenditures_eurostat %>%
  group_by(geo, time) %>%
  summarise(values = sum(values))
health_exp$variable <- "health_expenditures"

#
# cultural_participation_eurostat (Percentage of 16 years and older, under 30, above 75 who didn't attend on any cultural event in the last 12 months)
cult_part <- cultural_participation_eurostat %>%
  filter(sex == "T" & isced11 == "TOTAL" &
           acl00 == "Cultural activities (cinema, live performances or cultural sites)" & frequenc == "NM12") %>%
  subset(select = c(geo, time, values, age))
cult_part$age <- paste("cult", cult_part$age, sep = "_")
colnames(cult_part)[colnames(cult_part) == "age"] <- "variable"

pop <- rbind(total, sex, age, physicians, health_exp, cult_part)
pop <- subset(pop, select = -time)

country_char <- spread(pop, key = variable, value = values)
country_char <- country_char[country_char$geo %in% capitals$country_code_iso2,]

### Time vars

#
# Weather
tempavg <- prepare_weather(weather)
tempavg <- clean_weather(tempavg)

#
# Response measurements
response <- prepare_response(response, rangefrom = NA)

#
# Testing
testing <- prepare_testing(testing, rangefrom = NA)
testing <- clean_testing(testing)

#
# Merging testing and response
response_testing <- merge(testing, response, by.x = c("country", "year", "week"),
                          by.y = c("Country", "year", "week"), all.y = TRUE)
response_testing <- subset(response_testing, select = -c(country_code))
response_testing <- merge(response_testing, capitals[,c("country", "country_code")],
                         by = "country", all.x = TRUE)

#
# Merging with temperature
resp_test_temp <- merge(response_testing, tempavg, by = c("country_code", "date"), all = TRUE)
# Adding year, week number and country
resp_test_temp$year[which(is.na(resp_test_temp$year))] <- str_extract(resp_test_temp$date[which(is.na(resp_test_temp$year))], "^\\d+")
resp_test_temp$week[which(is.na(resp_test_temp$week))] <- strftime(resp_test_temp$date[which(is.na(resp_test_temp$week))], format = "%V")

resp_test_temp <- resp_test_temp %>%
  merge(capitals[, c("country", "country_code")], by ="country_code", all.x = TRUE) %>%
  mutate(country.x = country.y) %>%
  subset(select = -c(country.y))
colnames(resp_test_temp)[colnames(resp_test_temp) == "country.x"] <- "country"

# Fb
fb <- prepare_fb(fb)

# Merging with fb data
tdata <- merge(resp_test_temp, fb, by = c("country_code", "date"), all = TRUE)

# Covid
covid <- prepare_covid(covid)

# Merging with covid
tdata <- merge(covid, tdata, by = c("country", "date"), all = TRUE)

# Vaccination
vaccination <- prepare_vaccination(vaccination)

# Merging with vaccination
tdata <- merge(tdata, vaccination, by = c("country", "date"), all.x = TRUE)


# Year and week numeric
tdata$year <- as.numeric(tdata$year)
tdata$week <- as.numeric(tdata$week)
