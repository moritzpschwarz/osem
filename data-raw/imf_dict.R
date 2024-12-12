
dict_imf <- tibble::tribble(
  ~model_varname,            ~full_name, ~database, ~dataset_id, ~freq, ~ref_area, ~commodity, ~unit_measure,
  "WORLD_OIL", "World Oil Price USD",     "imf",      "PCPS",   "M",     "W00", "POILAPSP",         "USD"
)


dict_imf <- as.data.frame(dict_imf)

usethis::use_data(x = dict_imf, overwrite = TRUE)

