data <- imf.data::load_datasets("PCTOT")
imports <- data$get_series(freq="M",ref_area = "DE",indicator = "m",type="R_RW_IX")
columns <- colnames(imports) # columns[1] = TIME_PERIOD, columns[2] = 'the unique identifier that represents the values of the data row'
value_colname <- columns[2]
#add na_item (model_varname)
imports <- imports %>% dplyr::mutate(na_item = "import_price_index")
#convert the value column into numeric
imports <- imports %>%
  dplyr::mutate(VALUE = as.numeric(.data[[value_colname]]))

#drop old uniquely identified value column
imports <- imports %>%
  dplyr::select(.,-dplyr::all_of(value_colname))

#rename REF_DATE to time
imports <- imports %>% dplyr::rename("time" = "TIME_PERIOD")

# rename VALUE to values
imports <- imports %>% dplyr::rename("values" = "VALUE")

