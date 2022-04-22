run_module <- function(specification, data){

  raw_data <- identify_correct_data(specification, data)

  clean_df <- clean_data(raw_data)

  estimated_module <- estimate_module(clean_df)

  done <- add_to_original_data(clean_df, estimated_module)

  return(done)



}
