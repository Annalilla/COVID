save_data <- function(data, datname, dirname = NA, archieve = FALSE)
{
  filename <- paste(getwd(), "/", dirname, "/", datname, ".csv", sep = "")
  if(archieve == TRUE)
  {
    archived_file <- paste(getwd(), "/", dirname, "/", datname, "_", Sys.Date(), ".csv", sep = "")
    if(file.exists(filename)) file.rename(filename, archived_file)
  }
  write.csv(data, filename, row.names = FALSE)
}