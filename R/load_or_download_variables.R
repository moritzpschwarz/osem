load_or_download_variables <- function(module_order, inputdata_directory, save_to_disk = FALSE) {

  # check the inputdata_directory for any files

  # open files and check if variables available

  # if not, download via eurostat package

  # return finalised data

  if (save_to_disk) {
    write_csv(return_df, file = paste0(inputdata_directory, "/eurostat_download.csv"))
  }

  return(return_df)
}
