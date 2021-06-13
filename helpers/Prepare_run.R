# Prepares the data collection and analysis
# Must be run before any other codes

setwd("C:/Stuff/MDM/Master/R")

dir.create("data", showWarnings = FALSE)

#maxdate <- Sys.Date() - 1
maxdate <- "2021-05-19"

capitals <- data.frame("country" = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France",
                                     "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
                                     "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden"),
                       "country_fb" = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech_Republic", "Denmark", "Estonia", "Finland", "France",
                                        "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
                                        "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden"),
                       "country_code" = c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU", "IE", "IT", "LVA",
                                          "LT", "LU", "MT", "NL", "POL", "PT", "RO", "SK", "SI", "ES", "SE"),
                       "country_code_iso" = c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL",
                                              "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"),
                       "country_code_iso2" = c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "EL", "HU", "IE", "IT", "LV",
                                               "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE"),
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
                                     "GMM00010385", "GR000016716", "HUM00012843", "EI000003969", "IT000016239", "USW00014768", "LH000026730", "LU000006590", "MT000016597", "NLM00006260",
                                     "PLM00012375", "PO000008535", "ROE00108889", "AU000005901", "SIM00014015", "SP000003195", "SWM00002589"))

