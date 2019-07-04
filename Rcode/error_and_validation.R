# So far very lite, I should've coded better

validate_wines <- function(allWine){
  if (!file.exists(file_error_data)){
    message("no data on variance found, calculating")
    error_frame                    <- leave_oneout_regression(allWine)
    error_frame <- merge(leave_one_out_neg_log(allWine)[, c("Varnummer", "log_pred")], error_frame, by = "Varnummer")
    write.table(error_frame, file_error_data, sep = "\t")
  }
}
