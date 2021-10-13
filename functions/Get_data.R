# Contains a function to load the databases form a local or online location

get_data <- function(datname, dirname = NA, local = FALSE)
{
  if(local == TRUE){
    fileurl <- paste(dirname, "/", datname, ".csv", sep = "")
    if(file.exists(fileurl)){
      read.csv(fileurl)
    }else{
      cat("File not Found")
    }
  }else if(local == FALSE){
    # File for authentication available
    if(file.exists(".secrets")){
      all_files <- list.files(".secrets")
      if(length(which(grepl(".+@gmail.com$", all_files))) > 0){
        if(!("googlesheets4" %in% (.packages()))) library(googlesheets4)
        if(!gs4_has_token()){
          # google sheets authentication
          gs4_auth(
            cache = ".secrets",
            email = "covid.data.storage@gmail.com"
          )
        }
        
        covid_sheet <- "https://docs.google.com/spreadsheets/d/1K6jzpSKJHVQBVyRfAtVhkOcgepGEnrTVK-Ifnw85_vM/edit#gid=0"
        if(datname %in% sheet_names(covid_sheet)){
          range_speedread(covid_sheet, sheet = datname)
        }
      }else{
          cat("Read data from googlesheets was unsuccesfull - File for authentication not available")
      }
    }else{
      cat("Read data from googlesheets was unsuccesfull - File for authentication not available")
    }
  }
}
