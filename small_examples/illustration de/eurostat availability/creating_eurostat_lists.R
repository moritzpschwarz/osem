dictionary %>%
  filter(database == "eurostat") %>%
  distinct(dataset_id) %>%
  pull(dataset_id)-> eurostat_databases

overall_codes <- tibble()
overall_labels <- tibble()
for(i in eurostat_databases){
  print(i)
  int_data <- get_eurostat(i)

  dimensions_codes <- setdiff(names(int_data), c("values", "TIME_PERIOD"))

  int_data %>%
    drop_na(values) %>%
    summarise(n = n(),
              min_time = min(TIME_PERIOD),
              max_time = max(TIME_PERIOD), .by = all_of(dimensions_codes)) %>%
    mutate(dataset_id = i) %>%
    bind_rows(overall_codes, .) -> overall_codes


  int_labels <- label_eurostat(int_data, fix_duplicated = TRUE)

  dimensions_labels <- setdiff(names(int_labels), c("values", "TIME_PERIOD"))

  int_labels %>%
    drop_na(values) %>%
    summarise(n = n(),
              min_time = min(TIME_PERIOD),
              max_time = max(TIME_PERIOD), .by = all_of(dimensions_labels)) %>%
    mutate(dataset_id = i) %>%
    bind_rows(overall_labels, .) -> overall_labels

}

write_csv(overall_codes, "eurostat_availability_codes.csv")
write_csv(overall_labels, "eurostat_availability_labels.csv")


write_csv(overall_codes, "eurostat_availability_codes.csv")
write_csv(overall_labels, "eurostat_availability_labels.csv")


#https://ec.europa.eu/eurostat/databrowser/bulk?lang=en&selectedTab=codeList
#https://ec.europa.eu/eurostat/api/dissemination/files/inventory?type=codelist&lang=en

