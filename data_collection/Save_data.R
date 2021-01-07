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
