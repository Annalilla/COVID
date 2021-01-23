library(Hmisc)

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

# All labels
all_labels <- c(response_labels, testing_labels, weather_labels, vaccination_labels)

#
# Adding labels
label(tdata) <- as.list(all_labels[match(names(tdata), names(all_labels))])
label(tdata) = as.list(var.labels[match(names(data), names(var.labels))])



