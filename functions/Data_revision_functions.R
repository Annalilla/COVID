show_number_of_differences <- function(){
  no_diff <- lapply(differences, length)
  return(no_diff)
}

show_first_differences <- function(n){
  first_diff <- lapply(differences, head, n = n)
  find_different_records(first_diff)
}

show_last_differences <- function(n){
  last_diff <- lapply(differences, tail, n = n)
  find_different_records(last_diff)
}

show_all_differences_in_one_var <- function(varname){
  act_diff <- differences[[which(names(differences) == varname)]]
  diffs_in_record <- cbind(to_compare[act_diff, c(which(colnames(to_compare) %in% c("country", "date", varname)))],
                           old_tdata[act_diff, which(colnames(old_tdata) == varname)])
  colnames(diffs_in_record)[ncol(diffs_in_record)] <- paste("old value of ", varname, sep = "")
  return(diffs_in_record)
}

show_new_vars <- function(){
  colnames(to_compare)[-which(colnames(to_compare) %in% colnames(old_tdata))]
}

# Extract differences
find_different_records <- function(diffs){
  colnum <- unlist(lapply(seq_along(diffs), function(i){which(colnames(to_compare) == names(diffs)[i])}))
  res <- lapply(seq_along(colnum), function(x) differences_in_record(nx = x, listx = diffs, colnumx = colnum))
  return(res)
}

differences_in_record <- function(nx, listx, colnumx){
  diffs_in_record <- cbind(to_compare[listx[[nx]], c(which(colnames(to_compare) %in% c("country", "date")), colnumx[[nx]])],
                           old_tdata[listx[[nx]], colnumx[[nx]]])
  colnames(diffs_in_record)[ncol(diffs_in_record)] <- paste("old value of ", colnames(to_compare)[colnumx[[nx]]], sep = "")
  return(diffs_in_record)
}

# Update old records
update_var <- function(varname, records = "all"){
  old_dat <- old_tdata
  if(records[1] == "all"){
    act_diff <- differences[[which(names(differences) == varname)]]
    old_dat[act_diff, which(colnames(old_dat) == varname)] <- tdata[act_diff, which(colnames(tdata) == varname)]
  }else{
    old_dat[records, which(colnames(old_dat) == varname)] <- tdata[records, which(colnames(tdata) == varname)]
  }
  return(old_dat)
}

# Add new variable
add_new_var <- function(varname){
  old_tdata <- cbind(old_tdata, to_compare[, which(colnames(to_compare) == varname)])
  colnames(old_tdata)[ncol(old_tdata)] <- varname
}


