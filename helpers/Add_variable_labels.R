library(Hmisc)

population_labels <- c(`Y_LT5` = "Less than 5 years", `Y5-9` = "From 5 to 9 years", `Y10-14` = "From 10 to 14 years",
                       `Y15-19` = "From 15 to 19 years", `Y20-24` = "From 20 to 24 years", `Y25-29` = "From 25 to 29 years",
                       `Y30-34` = "From 30 to 34 years", `Y35-39` = "From 35 to 39 years", `Y40-44` = "From 40 to 44 years",
                       `Y45-49` = "From 45 to 49 years", `Y50-54` = "From 50 to 54 years", `Y55-59` = "From 55 to 59 years",
                       `Y60-64` = "From 60 to 64 years", `Y65-69` = "From 65 to 69 years", `Y70-74` = "From 70 to 74 years",
                       `Y75-79` = "From 75 to 79 years", `Y_GE75` = "75 years or over", `Y80-84` = "From 80 to 84 years",
                       `Y_GE80` = "80 years or over", `Y_GE85` = "85 years or over",
                       T = "Total", M = "Males", F = "Females")

response_labels <- c(AdaptationOfWorkplace = "Adaptationof workplaces(e.g. to reduce risk of transmission)",
                     AdaptationOfWorkplacePartial = "Adaptation of workplaces (e.g. to reduce risk of transmission)-partially relaxed measure",
                     BanOnAllEvents = "Interventions are in place to limit all indoor/outdoor mass/public gatherings",
                     BanOnAllEventsPartial = "Interventions are in place to limit all indoor/outdoor mass/public gatherings-partially relaxed measure",
                     ClosDaycare = "Closure of educational institutions - daycare or nursery",
                     ClosDaycarePartial = "Closure of educational institutions - daycare or nursery -partially relaxed measure",
                     ClosHigh = "Closure of educational institutions- higher education.",
                     ClosHighPartial = "Closure of educational institutions - higher education -partially relaxed measure",
                     ClosPrim = "Closure of educational institutions - primary schools.",
                     ClosPrimPartial = "Closure of educational institutions - primary schools -partially relaxed measure",
                     ClosPubAny = "Closure of public spaces of any kind (including restaurants, entertainment venues, non-essential shops, partial or full closure of public transport, gyms and sport centers, etc).",
                     ClosPubAnyPartial = "Closure of public spaces of any kind (including restaurants, entertainment venues, non-essential shops, partial orfull closure of public transport, gyms and sport centers etc) -partially relaxed measure",
                     ClosSec = "Closure of educational institutions - secondary schools.",
                     ClosSecPartial = "Closure of educational institutions - secondary schools -partially relaxed measure",
                     ClosureOfPublicTransport = "Closure of public transport",
                     ClosureOfPublicTransportPartial = "Closure of public transport-partially relaxed measure",
                     EntertainmentVenues = "Closure of entertainment venues",
                     EntertainmentVenuesPartial = "Closure of entertainment venues-partially relaxed measure",
                     GymsSportsCentres = "Closure of gyms/sports centres",
                     GymsSportsCentresPartial = "Closure of gyms/sports centres-partially relaxed measure",
                     HotelsAccommodation = "Closure of hotels/accommodation services",
                     HotelsAccommodationPartial = "Closure of hotels/accommodation services-partially relaxed measure",
                     IndoorOver100 = "Interventions are in place to limit indoor mass/public gatherings of over 100participants",
                     IndoorOver1000 = "Interventions are in place to limit indoor mass/public gatherings of over 1000participants",
                     IndoorOver1000Partial = "Interventions are in place to limit indoor mass/public gatherings of over 1000participants-partially relaxed measure",
                     IndoorOver100Partial = "Interventions are in place to limit indoor mass/public gatherings of over 100participants-partially relaxed measure",
                     IndoorOver50 = "Interventions are in place to limit indoor mass/public gatherings of over 50participants",
                     IndoorOver500 = "Interventions are in place to limit indoor mass/public gatherings of over 500participants",
                     IndoorOver500Partial = "Interventions are in place to limit indoor mass/public gatherings of over 500participants-partially relaxed measure",
                     IndoorOver50Partial = "Interventions are in place to limit indoor mass/public gatherings of over 50participants-partially relaxed measure",
                     MasksMandatoryAllSpaces = "Protective mask use in all public spaces on mandatory basis (enforced by law)",
                     MasksMandatoryAllSpacesPartial = "Protective mask use in all public spaces on mandatory basis (enforced by law)-partially relaxed measure",
                     MasksMandatoryClosedSpaces = "Protective mask use in closedpublic spaces/transport on mandatory basis (enforced by law)",
                     MasksMandatoryClosedSpacesPartial = "Protective mask use in closed public spaces/transport on mandatory basis (enforced by law)-partially relaxed measure",
                     MasksVoluntaryAllSpaces = "Protective mask use in all public spaceson voluntary basis (general recommendation not enforced)",
                     MasksVoluntaryAllSpacesPartial = "Protective mask use in all public spaces on voluntary basis (general recommendation not enforced)-partially relaxed measure",
                     MasksVoluntaryClosedSpaces = "Protective mask use in closed public spaces/transport on voluntary basis (general recommendation not enforced)",
                     MasksVoluntaryClosedSpacesPartial = "Protective mask use in closed public spaces/transport on voluntary basis (general recommendation not enforced)-partially relaxed measure",
                     MassGatherAll = "Interventions are in place to limit mass/public gatherings (any interventions on mass gatherings up to 1000 participants included)",
                     MassGatherAllPartial = "Interventions are in place to limit mass/public gatherings (any interventions on mass gatherings up to 1000 participants included)-partially relaxed measure",
                     NonEssentialShops = "Closures of non-essential shops",
                     NonEssentialShopsPartial = "Closures of non-essential shops -partially relaxed measure",
                     OutdoorOver100 = "Interventions are in place to limit outdoor mass/public gatherings of over 100participants",
                     OutdoorOver1000 = "Interventions are in place to limit outdoor mass/public gatherings of over 1000participants",
                     OutdoorOver1000Partial = "Interventions are in place to limit outdoor mass/public gatherings of over 1000participants-partially relaxed measure",
                     OutdoorOver100Partial = "Interventions are in place to limit outdoor mass/public gatherings of over 100participants-partially relaxed measure",
                     OutdoorOver50 = "Interventions are in place to limit outdoor mass/public gatherings of over 50participants",
                     OutdoorOver500 = "Interventions are in place to limit outdoormass/public gatherings of over 500participants",
                     OutdoorOver500Partial = "Interventions are in place to limit outdoor mass/public gatherings of over 500participants-partially relaxed measure",
                     OutdoorOver50Partial = "Interventions are in place to limit outdoor mass/public gatherings of over 50participants-partially relaxed measure",
                     PlaceOfWorship = "Closure of places of worship",
                     PlaceOfWorshipPartial = "Closure of places of worship-partially relaxed measure",
                     PrivateGatheringRestrictions = "Restrictions on private gatherings",
                     PrivateGatheringRestrictionsPartial = "Restrictions on private gatherings-partially relaxed measure",
                     RegionalStayHomeOrder = "Regional stay-at-home orders for the general populationat least in one region(these are enforced and also referred to as 'lockdown')",
                     RegionalStayHomeOrderPartial = "Regional stay-at-home orders for the general population at least in one region (these are enforced and also referred to as 'lockdown')-partially relaxed measure",
                     RestaurantsCafes = "Closure of restaurants and cafes/bars",
                     RestaurantsCafesPartial = "Closure of restaurants and cafes/bars-partially relaxed measure",
                     SocialCircle = "Social circle/bubble to limit social contacts e.g. to limited number of households",
                     SocialCirclePartial = "Social circle/bubble to limit social contacts e.g. to limited number of households-partially relaxed measure",
                     StayHomeGen = "Stay-at-home recommendations for the general population (which are voluntary or not enforced)",
                     StayHomeGenPartial = "Stay-at-home recommendations for the general population (which are voluntary or not enforced) -partially relaxed measure",
                     StayHomeOrder = "Stay-at-home orders for the general population (these are enforced and also referred to as 'lockdown')",
                     StayHomeOrderPartial = "Stay-at-home orders for the general population (these are enforced and also referred to as 'lockdown') -partially relaxed measure",
                     StayHomeRiskG = "Stay-at-home recommendations for risk groups or vulnerable populations (such as the elderly, people with underlying health conditions, physically disabled people, etc.)",
                     StayHomeRiskGPartial = "Stay-at-home recommendations for risk groups or vulnerable populations (such as the elderly, people with underlying health conditions, physically disabled people, etc.) -partially relaxed measure",
                     Teleworking = "Teleworking recommendation",
                     TeleworkingPartial = "Teleworking recommendation or workplace closures -partially relaxed measure",
                     WorkplaceClosures = "Closures of workplaces",
                     WorkplaceClosuresPartial = "Closures of workplaces-partially relaxed measure")

testing_labels <- c(testing_new_cases = "Number of new confirmed cases", tests_done = "Number of tests done",
                    testing_rate = "Testing rate per 100 000 population", testing_positivity_rate = "Weekly test positivity (%)")

weather_labels <- c(tavg = "Average daily temperature")

vaccination_labels <- c(total_vaccinations = "total number of doses administered", new_vaccinations = "daily change in the total number of doses administered",
                        new_vaccinations_smoothed = "new doses administered per day (7-day smoothed)",
                        total_vaccinations_per_hundred = "people vaccinated per 100 people in the total population of the country",
                        new_vaccinations_smoothed_per_million = "daily vaccinations per 1,000,000 people in the total population of the country")

covid_labels <- c(cases_new = "Confirmed daily new cases ", 
                  death_new = "Daily number of deaths ",
                  recovered_new = "Daily number of the recovered ")

fb_labels <- c(fb_data.iso_code = " ISO country codes",
               fb_data.percent_cli ="weighted percentage of respondents that have reported Covid Like Illness",
               fb_data.cli_se ="standard error of percent_cli",
               fb_data.percent_cli_unw ="unweighted percentage of respondents that have reported CLI",
               fb_data.cli_se_unw = " standard error of percent_cli_unw",
               fb_data.sample_size_cli =" sample size for calculating CLI",
               fb_data.smoothed_cli="smoothed percentage of respondents that have reported Covid Like Illness",
               fb_data.smoothed_cli_se="standard error of smoothed percent_cli",
               fb_data.sample_size_smoothed_cli="sample size for calculating smoothed CLI",
               fb_data.percent_mc ="weighted percentage of respondents that have reported using a mask",
               fb_data.mc_se ="standard error of percent_mc" ,
               fb_data.percent_mc_unw ="unweighted percentage of respondents that have reported use mask cover" ,
               fb_data.mc_se_unw ="standard error of percent_mc_unw" ,
               fb_data.sample_size_mc =" sample size for calculating mask coverage" ,
               fb_data.smoothed_mc= "smoothed percentage of respondents that have reported use mask cover",
               fb_data.smoothed_mc_se= "standard error of smoothed percent_mc",
               fb_data.sample_size_mc_smoothed="sample size for calculating smoothed mc", 
               fb_data.percent_dc ="weighted percentage of respondents that have reported had direct contact 
                                    (longer than one minute) with people not staying with them in last 24 hours",
               fb_data.mc_se_dc ="?" ,
               fb_data.percent_dc_unw ="unweighted percentage of respondents that have reported use have direct contact 
                                      with people not staying with them", 
               fb_data.dc_se_unw ="standard error of percent_dc_unw" ,
               fb_data.sample_size_dc ="sample size for calculating direct contact", 
               fb_data.smoothed_dc ="smoothed percentage of respondents that have reported direct contact",
               fb_data.smoothed_dc_se ="standard error of smoothed percent_dc",
               fb_data.sample_size_dc_smoothed = "sample size for calculating smoothed dc")

#
# country_char
# All labels
all_labels_c <- c(population_labels)

# Adding labels
label(country_char) <- as.list(all_labels_c[match(names(country_char), names(all_labels_c))])


#
# tdata

# All labels
all_labels <- c(response_labels, testing_labels, weather_labels, vaccination_labels, covid_labels, fb_labels)


#
# Adding labels
label(tdata) <- as.list(all_labels[match(names(tdata), names(all_labels))])



