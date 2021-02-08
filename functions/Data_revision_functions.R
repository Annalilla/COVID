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
  diffs_in_record <- cbind(common_vars[act_diff, c(which(colnames(common_vars) %in% c("country", "date", varname)))],
                           old_tdata_common[act_diff, which(colnames(old_tdata_common) == varname)])
  colnames(diffs_in_record)[ncol(diffs_in_record)] <- paste("old value of ", varname, sep = "")
  return(diffs_in_record)
}

show_new_vars <- function(){
  colnames(to_compare)[-which(colnames(to_compare) %in% colnames(old_tdata))]
}

show_extinct_vars <- function(){
  colnames(old_tdata)[-which(colnames(old_tdata) %in% colnames(to_compare))]
}

# Extract differences
find_different_records <- function(diffs){
  colnum_common_vars <- unlist(lapply(seq_along(diffs), function(i){which(colnames(common_vars) == names(diffs)[i])}))
  colnum_old_tdata <- unlist(lapply(seq_along(diffs), function(i){which(colnames(old_tdata_common) == names(diffs)[i])}))
  
  res <- lapply(seq_along(colnum_common_vars), function(x) differences_in_record(nx = x, listx = diffs, colnum_commonx = colnum_common_vars,
                                                                     colnum_oldx = colnum_old_tdata))
  return(res)
}

differences_in_record <- function(nx, listx, colnum_commonx, colnum_oldx){
  diffs_in_record <- cbind(common_vars[listx[[nx]], c(which(colnames(common_vars) %in% c("country", "date")), colnum_commonx[[nx]])],
                           old_tdata_common[listx[[nx]], colnum_oldx[[nx]]])
  colnames(diffs_in_record)[ncol(diffs_in_record)] <- paste("old value of ", colnames(common_vars)[colnum_commonx[[nx]]], sep = "")
  return(diffs_in_record)
}

# Update old records
update_var <- function(varname, records = "all"){
  old_dat <- old_tdata_common
  if(records[1] == "all"){
    act_diff <- differences[[which(names(differences) == varname)]]
    old_dat[act_diff, which(colnames(old_dat) == varname)] <- common_vars[act_diff, which(colnames(common_vars) == varname)]
  }else{
    old_dat[records, which(colnames(old_dat) == varname)] <- common_vars[records, which(colnames(common_vars) == varname)]
  }
  return(old_dat)
}

# Add new variable
add_new_var <- function(varname){
  old_tdata <- cbind(old_tdata, to_compare[, which(colnames(to_compare) == varname)])
  colnames(old_tdata)[ncol(old_tdata)] <- varname
  return(old_tdata)
}
