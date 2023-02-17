#' Prepare the clean dataframe for modelling
#'
#' @param raw_data A tibble or data.frame with the y variable and the x
#'   variables. Needs to have a column called 'time', which is of class
#'   \code{\link[base:Dates]{Date}}. Variable names need to be in column
#'   'na_item', and values in column 'values'.
#' @param max.lag Integer. The maximum number of lags to use for both the AR terms as
#'   well as for the independent variables.
#' @param trend Logical. Should a trend be added? Default is TRUE.
#'
#'
#' @return A tibble with the cleaned data.
#'
#' @export
#'
#' @examples
#' sample_data <- tibble(
#'   time = rep(seq.Date(
#'     from = as.Date("2000-01-01"),
#'     to = as.Date("2000-12-31"), by = 1
#'   ), each = 2),
#'   na_item = rep(c("yvar", "xvar"), 366), values = rnorm(366 * 2, mean = 100)
#' )
#' clean_data(sample_data, max.lag = 4)

clean_data <- function(raw_data,
                       max.lag = 4,
                       trend = TRUE) {
  raw_data %>%
    select(na_item, time, values) %>%
    pivot_wider(id_cols = time, names_from = na_item, values_from = values) %>%
    #janitor::clean_names() %>%
    #rename_with(.fn = tolower) %>%
    arrange(., time) %>%
    mutate(
      across(-time, list(ln = log), .names = "{.fn}.{.col}"),
      across(starts_with("ln."), list(D = ~ c(NA, diff(., ))), .names = "{.fn}.{.col}")
    ) -> intermed

  to_be_added <- tibble(.rows = nrow(intermed))
  for (i in 1:max.lag) {
    intermed %>%
      mutate(across(c(starts_with("D."), starts_with("ln.")), ~ dplyr::lag(., n = i))) %>%
      select(c(starts_with("D."), starts_with("ln."))) %>%
      rename_with(.fn = ~ paste0("L", i, ".", .)) %>%
      bind_cols(to_be_added, .) -> to_be_added
  }

  intermed %>%
    bind_cols(to_be_added) %>%
    mutate(index = 1:n()) %>%
    relocate(index) %>%
    mutate(q = lubridate::quarter(time, with_year = FALSE)) %>%
    fastDummies::dummy_cols(
      select_columns = "q", remove_first_dummy = TRUE,
      remove_selected_columns = TRUE
    ) %>%
    {if(trend){mutate(.,trend = as.numeric(as.factor(time)),.after = time)} else {.}} -> cleaned_data

  return(cleaned_data)

}
