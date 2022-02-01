# Formats the data and merges it into two databases

library(lubridate)
library(stringr)
library(reshape2)

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
#As physicians data is available for only 6 countries, we do not use them.
#physicians <- physicians_eurostat %>%
#  group_by(geo, time) %>%
#  summarise(values = sum(values))
#physicians$variable <- "physicians"

#
# health_expenditures_eurostat
# Categories which sums up the total health care expenditure
tot_h_sum <- c("Curative care", "Rehabilitative care", "Long-term care (health)", "Ancillary services (non-specified by function)",
               "Medical goods (non-specified by function)", "Preventive care", "Governance and health system and financing administration",
               "Other health care services unknown")
health_expenditures_eurostat <- health_expenditures_eurostat[which((health_expenditures_eurostat$icha11_hc %in% tot_h_sum) &
                                                               health_expenditures_eurostat$unit == "MIO_EUR"),]
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

# pop <- rbind(total, sex, age, physicians, health_exp, cult_part)
pop <- rbind(total, sex, age, health_exp, cult_part)
pop <- subset(pop, select = -time)

country_char <- spread(pop, key = variable, value = values)
country_char <- country_char[country_char$geo %in% capitals$country_code_iso2,]
country_char <- as.data.frame(apply(country_char, 2, as.character))

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

# Dominant variant
variant <- prepare_variant(variant)

# Merging reponse and testing with variant
resp_test_temp <- merge(response_testing, variant, by = c("country", "year", "week"), all.x = TRUE)

# Set variants to 0 before first detection
# Source of detection date: https://www.ecdc.europa.eu/en/covid-19/variants-concern
min_dates <- as.data.frame(matrix(ncol = 4, nrow = 1))
colnames(min_dates) <- c("B.1.1.7", "P.1", "B.1.617.2", "B.1.1.529")
min_dates$B.1.1.7 <- "2020-09.01"
min_dates$P.1 <- "2020-12-01"
min_dates$B.1.617.2 <- "2020-12-01"
min_dates$B.1.1.529 <- "2021-11-01"

resp_test_temp <- as.data.frame(resp_test_temp)
new_var_cols <- lapply(colnames(min_dates), function(x){
  act_col <- resp_test_temp[, c(which(colnames(resp_test_temp) == "date"), which(grepl(x, colnames(resp_test_temp))))]
  act_min_date <- min_dates[,which(colnames(min_dates) == x)]
  act_col[which(act_col$date < act_min_date & act_col[,2] > 0), 2] <- 0
  return(act_col)
})
new_var_cols <- do.call("cbind", new_var_cols)
new_var_cols <- new_var_cols[,-which(colnames(new_var_cols) == "date")]

resp_test_temp <- cbind(resp_test_temp[,-which(colnames(resp_test_temp) %in% colnames(new_var_cols))],
                        new_var_cols)


#
# Merging with temperature
resp_test_temp <- merge(resp_test_temp, tempavg, by = c("country_code", "date"), all = TRUE)
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

## Missing values in variants
# Replace variant other with 100%, other variants with 0 for the beginning of the time periods of each country
variant_vars <- which(grepl("percent_variant", colnames(tdata)))
variant_vars_spec <- variant_vars[-which(variant_vars == which(colnames(tdata) == "percent_variant.Other"))]
tdata[,variant_vars_spec] <- lapply(tdata[,variant_vars_spec], function(x){
  x[which(is.na(x) & tdata$date < "2021.01.01")] <- 0
  return(x)
})
tdata[,"percent_variant.Other"][which(is.na(tdata[,"percent_variant.Other"]) & tdata$date < "2021.01.01")] <- 100

# Remove last rows, where variants are missing <- they will be removed later
# missing_vari <- which(apply(tdata[,variant_vars], 1, function(x) sum(which(is.na(x)))) > 0)

#max_date <- min(tdata[missing_vari, "date"])

#tdata <- tdata[tdata$date < max_date,]

# Merging with vaccination
tdata <- merge(tdata, vaccination, by = c("country", "date"), all.x = TRUE)

tdata <- tdata[,-which(colnames(tdata) == "percent_variant.P.1")]


# Year and week numeric
tdata$year <- as.numeric(tdata$year)
tdata$week <- as.numeric(tdata$week)
