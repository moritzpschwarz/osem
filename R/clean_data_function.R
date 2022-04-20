#' Prepare the clean dataframe for modelling
#'
#' @param raw_data A tibble or data.frame with the y variable and the x variables. Needs to have a column called "time", which is off class = date. Variable names need to be in 'na_item', values in 'values'.
#' @param preestimated_xvars A data.frame or tibble with the x variables that have been pre-estimated by another equation in levels. Needs to have a column called "time", which is off class = date.
#'
#' @return A tibble with the cleaned data.
#' @export
#'
#' @examples
#' sample_data <- tibble(time = rep(seq.Date(from = as.Date("2000-01-01"), to = as.Date("2000-12-31"),by = 1),each = 2), na_item = rep(c("yvar","xvar"),366), values = rnorm(366*2,mean = 100))
#' clean_data(sample_data, max.lag = 4)
#'

clean_data <- function(raw_data,
                       #use_logs = c("both","y","x"),
                       preestimated_xvars = NULL,
                       max.lag = 4){

  raw_data %>%
    select(na_item, time,values) %>%
    pivot_wider(id_cols = time, names_from = na_item, values_from = values) %>%
    janitor::clean_names() %>%

    # Add previously estimated data
    {if(!is.null(preestimated_xvars)){full_join(.,preestimated_xvars %>% select(-any_of("index")), by = "time")} else {.}} %>%

    arrange(.,time) %>%
    mutate(across(-time,list(ln = log),.names = "{.fn}.{.col}"),
           across(starts_with("ln."),list(D = ~c(NA,diff(., ))), .names = "{.fn}.{.col}")) -> intermed

  to_be_added <- tibble(.rows = nrow(intermed))
  for(i in 1:max.lag){
    intermed %>%
      mutate(across(c(starts_with("D."),starts_with("ln.")), ~dplyr::lag(., n = i))) %>%
      select(c(starts_with("D."),starts_with("ln."))) %>%
      rename_with(.fn = ~paste0("L",i,".",.)) %>%
      bind_cols(to_be_added,.) -> to_be_added
  }

  intermed %>%
    bind_cols(to_be_added) %>%

    mutate(index = 1:n()) %>%
    relocate(index) %>%
    mutate(q = lubridate::quarter(time, with_year = FALSE)) %>%
    fastDummies::dummy_cols(select_columns = "q", remove_first_dummy = TRUE,
                            remove_selected_columns = TRUE) -> cleaned_data

  return(cleaned_data)

}
