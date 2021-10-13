# Contains two functions (save database to local or to online location) to save the databases

save_data <- function(data, datname, dirname = NA, archieve = FALSE)
{
  filename <- paste(getwd(), "/", dirname, "/", datname, ".csv", sep = "")
  attach <- str_replace_all(str_extract_all(Sys.time(), ".+ \\d+:\\d+:\\d+"), "[ :]", "_")
  if(archieve == TRUE)
  {
    archived_file <- paste(getwd(), "/", dirname, "/", datname, "_", attach, ".csv", sep = "")
    if(file.exists(filename)) file.rename(filename, archived_file)
  }
  write.csv(data, filename, row.names = FALSE)
}

save_data_online <- function(data, datname, archieve = FALSE)
{
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
      if(archieve == TRUE)
      {
        attach <- str_replace_all(str_extract_all(Sys.time(), ".+ \\d+:\\d+:\\d+"), "[ :]", "_")
        if(datname %in% sheet_names(covid_sheet)){
          sheet_rename(covid_sheet, sheet = datname, new_name = paste(datname, attach, sep = "_"))
        }
      }
      sheet_write(data, ss = covid_sheet, sheet = datname)
    }else{
      cat("Save data to googlesheets was unsuccesfull - File for authentication not available")
    }
  }else{
    cat("Save data to googlesheets was unsuccesfull - .secrets dir not found")
  }
}
