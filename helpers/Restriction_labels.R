# Contains the restriction labels and their descriptions to be displayed in the dashboard instead of their abbreviation.

res_id <- c("AdaptationOfWorkplace", "BanOnAllEvents", "ClosDaycare", 
           "ClosDaycarePartial", "ClosHigh", "ClosHighPartial", 
           "ClosPrim", "ClosPrimPartial", "ClosPubAny", 
           "ClosPubAnyPartial", "ClosSec", "ClosSecPartial", 
           "EntertainmentVenues", "EntertainmentVenuesPartial", "GymsSportsCentres", 
           "HotelsAccommodation", "IndoorOver100", "IndoorOver1000", 
           "MasksMandatoryAllSpaces", "MasksMandatoryClosedSpaces", "MasksMandatoryClosedSpacesPartial", 
           "MasksVoluntaryClosedSpacesPartial", "MassGather50", "MassGatherAll", 
           "NonEssentialShops", "NonEssentialShopsPartial", "OutdoorOver1000", 
           "OutdoorOver50", "OutdoorOver500", "PlaceOfWorship", 
           "PlaceOfWorshipPartial", "PrivateGatheringRestrictions", "RegionalStayHomeOrder", 
           "RestaurantsCafes", "SocialCircle", "StayHomeGen", 
           "StayHomeOrder", "StayHomeOrderPartial", "Teleworking", 
           "BanOnAllEventsPartial", "GymsSportsCentresPartial", "IndoorOver500", 
           "MasksMandatoryAllSpacesPartial", "MasksVoluntaryAllSpaces", "MassGather50Partial", 
           "MassGatherAllPartial", "PrivateGatheringRestrictionsPartial", "RestaurantsCafesPartial", 
           "StayHomeGenPartial", "TeleworkingPartial", "AdaptationOfWorkplacePartial",  "ClosureOfPublicTransport",
           "ClosureOfPublicTransportPartial", "IndoorOver50", "StayHomeRiskG", "HotelsAccommodationPartial", "OutdoorOver100",
           "WorkplaceClosures", "WorkplaceClosuresPartial", "MasksVoluntaryAllSpacesPartial","MasksVoluntaryClosedSpaces",
           "StayHomeRiskGPartial", "RegionalStayHomeOrderPartial", "SocialCirclePartial",
           "QuarantineForInternationalTravellers", "QuarantineForInternationalTravellersPartial")
          
res_text <- c("Adaptation Of Workplace", "Ban On All Events", "Close Daycare", 
              "Close Daycare Partial", "Close High", "Close High Partial", 
              "Close Primary", "Close Primary Partial", "Close Pubs Any Kind", 
              "Close Pubs Any Kind Partial", "Close Secondary", "Close Secondary Partial", 
              "Entertainment Venues", "Entertainment Venues Partial", "Gyms/Sports Centres", 
              "Hotels Accommodation", "Indoor Over 100", "Indoor Over 1000", 
              "Masks Mandatory All", "Masks Mandatory Closed", "Masks Mandatory Closed Partial", 
              "Masks Voluntary Closed Partial", "Mass Gather 50", "Mass Gather All", 
              "Non-Essential Shops", "Non-Essential Shops Partial", "Outdoor Over 1000", 
              "Outdoor Over 50", "Outdoor Over 500", "Place Of Worship", 
              "Place Of Worship Partial", "Private Gathering Restrictions", "Regional Stay Home Order", 
              "Restaurants Cafes", "Social Circle", "Stay Home General", 
              "Stay Home Order", "Stay Home Order Partial", "Teleworking", 
              "Ban On All Events Partial", "Gyms Sports Centres Partial", "Indoor Over 500", 
              "Masks Mandatory All Partial", "Masks Voluntary All", "Mass Gather 50 Partial", 
              "Mass Gather All Partial", "Private Gathering Partial", "Restaurants Cafes Partial", 
              "Stay Home General Partial", "Teleworking Partial", "Adaptation Of Workplace Partial",  "Closure Of Public Transport",
              "Closure Of Public Transport Partial", "Indoor Over 50", "Stay Home Risk Groups", "Hotels Accommodation Partial", "Outdoor Over 100",
              "Workplace Closures", "Workplace Closures Partial", "Masks Voluntary All Partial","Masks Voluntary Closed",
              "Stay Home Risk Groups Partial", "Regional Stay Home Order Partial", "Social Circle Partial",
              "Quarantine for International Travellers", "Quarantine for International Travellers Partial")

res_label <- c("Adaptation of workplaces", "Limit all indoor/outdoor mass/public gatherings", "Closure of educational institutions: daycare or nursery", 
               "Closure of educational institutions: daycare or nursery -partially relaxed measure", "Closure of educational institutions: higher education", "Closure of educational institutions: higher education -partially relaxed measure", 
               "Closure of educational institutions: primary schools", "Closure of educational institutions: primary schools -partially relaxed measure", "Closure of public spaces of any kind (including restaurants, entertainment venues, non-essential shops, partial or full closure of public transport, gyms and sport centers, etc)", "Closure of public spaces of any kind (including restaurants, entertainment venues, non-essential shops, partial or full closure of public transport, gyms and sport centers etc) -partially relaxed measure", "Closure of educational institutions: secondary schools", "Closure of educational institutions: secondary schools -partially relaxed measure", 
               "Closure of entertainment venues", "Closure of entertainment venues-partially relaxed measure", "Closure of gyms/sports centres", 
               "Closure of hotels/accommodation services", "Interventions are in place to limit indoor mass/public gatherings of over 100participants", "Interventions are in place to limit indoor mass/public gatherings of over 1000participants", 
               "Protective mask use in all public spaces on mandatory basis (enforced by law)", "Protective mask use in closed public spaces/transport on mandatory basis (enforced by law)", "Protective mask use in closed public spaces/transport on mandatory basis (enforced by law)-partially relaxed measure", 
               "Protective mask use in closed public spaces/transport on voluntary basis (general recommendation not enforced)", "Mass Gather 50", "Interventions are in place to limit mass/public gatherings (any interventions on mass gatherings up to 1000 participants included)", 
               "Closures of non-essential shops", "Closures of non-essential shops -partially relaxed measure", "Interventions are in place to limit outdoor mass/public gatherings of over 1000participants", 
               "Interventions are in place to limit outdoor mass/public gatherings of over 50participants", "Interventions are in place to limit outdoor mass/public gatherings of over 500participants", "Closure of places of worship", 
               "Closure of places of worship-partially relaxed measure", "Restrictions on private gatherings", "Regional stay-at-home orders for the general population at least in one region(these are enforced and also referred to as 'lockdown')", 
               "Closure of restaurants and cafes/bars", "Social circle/bubble to limit social contacts e.g. to limited number of households", "Stay-at-home recommendations for the general population (which are voluntary or not enforced)", 
               "Stay-at-home orders for the general population (these are enforced and also referred to as 'lockdown')", "Stay-at-home orders for the general population (these are enforced and also referred to as 'lockdown') -partially relaxed measure", "Teleworking recommendation", 
               "Interventions are in place to limit all indoor/outdoor mass/public gatherings-partially relaxed measure", "Closure of gyms/sports centres-partially relaxed measure", "Interventions are in place to limit indoor mass/public gatherings of over 500participants", 
               "Protective mask use in all public spaces on mandatory basis (enforced by law)-partially relaxed measure", "Protective mask use in all public spaces on voluntary basis (general recommendation not enforced)", "Mass Gather 50 Partial", 
               "Interventions are in place to limit mass/public gatherings (any interventions on mass gatherings up to 1000 participants included)-partially relaxed measure", "Restrictions on private gatherings-partially relaxed measure", "Closure of restaurants and cafes/bars-partially relaxed measure", 
               "Stay-at-home recommendations for the general population (which are voluntary or not enforced) -partially relaxed measure", "Teleworking recommendation or workplace closures -partially relaxed measure", "Adaptation of workplaces (e.g. to reduce risk of transmission)-partially relaxed measure",  "Closure of public transport",
               "Closure of public transport-partially relaxed measure", "Interventions are in place to limit indoor mass/public gatherings of over 50participants", "Stay-at-home recommendations for risk groups or vulnerable populations (such as the elderly, people with underlying health conditions, physically disabled people, etc.)", "Closure of hotels/accommodation services-partially relaxed measure", "Interventions are in place to limit outdoor mass/public gatherings of over 100participants",
               "Closures of workplaces", "Closures of workplaces-partially relaxed measure", "Protective mask use in all public spaces on voluntary basis (general recommendation not enforced)-partially relaxed measure","Protective mask use in closed public spaces/transport on voluntary basis (general recommendation not enforced)",
               "Stay-at-home recommendations for risk groups or vulnerable populations (such as the elderly, people with underlying health conditions, physically disabled people, etc.) -partially relaxed measure", "Regional stay-at-home orders for the general population at least in one region (these are enforced and also referred to as 'lockdown')-partially relaxed measure", "Social circle/bubble to limit social contacts e.g. to limited number of households-partially relaxed measure",
               "Quarantine for International Travellers", "Quarantine for International Travellers Partial")
rest_table <- as.data.frame(cbind(res_id, res_text, res_label))
saveRDS(rest_table, "shinydashboard/dat/rest_table.RDS")

# Labels for other predictors
pred_id <- c("cases_new_lead", "deaths_new", "fb_data.percent_dc", "fb_data.percent_mc", "new_vaccinations",
             "people_fully_vaccinated_per_hundred", "people_vaccinated_per_hundred", "recovered_new", "tavg",
             "total_vaccinations_per_hundred", "fb_data.pct_covid_cli", "fb_data.sample_size_smoothed_dc",
             "fb_data.sample_size_smoothed_mc", "fb_data.smoothed_pct_covid_cli",
             "last_day", "last_week", "people_fully_vaccinated", "new_vaccinations_smoothed_per_million", "new_vaccinations_smoothed",
             "total_vaccinations", "people_vaccinated")
pred_text <- c("New cases with lead", "Deaths", "Direct Contact", "Mask Coverage", "New Vaccinations", "People Fully Vaccinated Per Hundred",
               "People Vaccinated Per Hundred", "Recovered", "Average Daily Temperature", "Total Vaccinations Per Hundred",
               "COVID-like Illnes", "Sample Size Direct Contact", "Sample Size Mask Cover",
               "COVID-like Illnes Smoothed", "New cases on the last day", "New cases on the last week",
               "People Fully Vaccinated", "New Vaccinations per Million Smoothed", "New Vaccinations Smoothed",
               "Total Vaccinations", "People Vaccinated")
pred_label <- c("Number of reported new cases with 14 days lead", "Number of reported new deaths",
                "Percentage of respondents that have reported had direct contact (longer than one minute) with people not staying with them in last 24 hours",
                "Percentage of respondents that have reported use mask cover", "Number of new vaccinations",
                "People fully vaccinated per hundred", "People vaccinated per hundred", "Number of recovered",
                "Average daily temperatures measured in the capital", "Total vaccinations per hundred",
                "Percentage of respondents that have reported COVID-like symptoms", "Sample size direct contact (smoothed)",
                "Sample size mask cover (smoothed)", "Percentage of respondents that have reported COVID-like symptoms (smoothed)",
                "Number of reported new cases on the last day", "Number of reported new cases on the last week",
                "People fully vaccinated", "Number of new vaccinations per Million (smoothed)", "Number of new vaccinations (smoothed)",
                "Total number of vaccinations", "Number of people vaccinated")
pred_table <- as.data.frame(cbind(pred_id, pred_text, pred_label))
saveRDS(pred_table, "shinydashboard/dat/pred_table.RDS")

to_bind_rest_table <- rest_table
colnames(to_bind_rest_table) <- c("pred_id", "pred_text", "pred_label")
all_pred_table <- rbind(pred_table, to_bind_rest_table)
saveRDS(all_pred_table, "shinydashboard/dat/all_pred_table.RDS")


bc_country <- paste("bc", c("Austria", "Belgium", "Bulgaria", "Croatia", "Czechia", "Denmark", "Finland", "France", "Germany",
                "Greece", "Hungary", "Ireland", "Italy", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
                "Slovenia", "Spain", "Sweden"), sep = "_")
saveRDS(bc_country, "shinydashboard/dat/bc_country.RDS")

