#' Prepare the clean dataframe for modelling
#'
#' @param raw_data A tibble or data.frame with the y variable and the x variables. Needs to have a column called "time", which is off class = date. Variable names need to be in 'na_item', values in 'values'.
#' @param preestimated_xvars A data.frame or tibble with the x variables that have been pre-estimated by another equation in levels. Needs to have a column called "time", which is off class = date. 
#'
#' @return A tibble with the cleaned data.
#' @export
#'
#' @examples
#' 

clean_data <- function(raw_data, 
                       preestimated_xvars = NULL, 
                       max.lag = 1:4){
  
  raw_data %>% 
    select(na_item, time,values) %>%
    pivot_wider(id_cols = time, names_from = na_item, values_from = values) %>% 
    clean_names() %>% 
    
    # Add previously estimated data
    {if(!is.null(preestimated_xvars)){full_join(.,preestimated_xvars, by = "time")} else {.}} %>%
    
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
  
  
  
  
  
  #   
  #   
  #   select(c(starts_with("D."),starts_with("ln."))) %>% 
  #   names -> names_for_tk_augment_lags
  # 
  # names_for_lags <- paste0(rep(paste0("L",1:max.lag,"."),length(names_for_tk_augment_lags)),
  #                          rep(names_for_tk_augment_lags,max.lag))
  # 
  # intermed %>% 
  #   timetk::tk_augment_lags(.value = c(starts_with("D."),starts_with("ln.")), 
  #                           .lags = 1:max.lag, 
  #                           .names = paste0("text",1:(max.lag*length(names_for_tk_augment_lags)))
  #                           #.names = names_for_lags
  #   ) %>% 
  #   
  # ECM
  # across(c(starts_with("D."),starts_with("ln.")),
  #        list(L = ~lag_multiple(. , n = max.lag)),
  #        .names = "{.fn}.{col}"
  # ),
  #across(c(starts_with("D."),starts_with("ln.")),~lag_multiple(. , n = max.lag)),
  #across(c(starts_with("D."),starts_with("ln.")),~lag_multiple(. , n = max.lag)),
  # across(c(starts_with("D."),starts_with("ln.")),list(L = lag),.names = "{.fn}1.{.col}"),
  # across(c(starts_with("D."),starts_with("ln.")),list(L = ~lag(.,2)),.names = "{.fn}2.{.col}"),
  # across(c(starts_with("D."),starts_with("ln.")),list(L = ~lag(.,3)),.names = "{.fn}3.{.col}"),
  # across(c(starts_with("D."),starts_with("ln.")),list(L = ~lag(.,4)),.names = "{.fn}4.{.col}"),
  
  
}

clean_data(imports_data, max.lag = 4)



















clean_data <- function(raw_data, 
                       preestimated_xvars = NULL, 
                       max.lag = 1:4){
  
  raw_data %>% 
    select(na_item, time,values) %>%
    pivot_wider(id_cols = time, names_from = na_item, values_from = values) %>% 
    clean_names() %>% 
    
    # Add previously estimated data
    {if(!is.null(preestimated_xvars)){full_join(.,preestimated_xvars, by = "time")} else {.}} %>%
    
    arrange(.,time) %>% 
    mutate(across(-time,list(ln = log),.names = "{.fn}.{.col}"),
           across(starts_with("ln."),list(D = ~c(NA,diff(., ))), .names = "{.fn}.{.col}")) -> intermed
  
  intermed %>% 
    select(c(starts_with("D."),starts_with("ln."))) %>% 
    names -> names_for_tk_augment_lags
  
  names_for_lags <- paste0(rep(paste0("L",1:max.lag,"."),length(names_for_tk_augment_lags)),
                           rep(names_for_tk_augment_lags,max.lag))
  browser()
  intermed %>% 
    timetk::tk_augment_lags(.value = c(starts_with("D."),starts_with("ln.")), 
                            .lags = 1:max.lag, 
                            .names = paste0("text",1:(max.lag*length(names_for_tk_augment_lags)))
                            #.names = names_for_lags
    ) %>% 
    
    # ECM
    # across(c(starts_with("D."),starts_with("ln.")),
    #        list(L = ~lag_multiple(. , n = max.lag)),
    #        .names = "{.fn}.{col}"
    # ),
    #across(c(starts_with("D."),starts_with("ln.")),~lag_multiple(. , n = max.lag)),
    #across(c(starts_with("D."),starts_with("ln.")),~lag_multiple(. , n = max.lag)),
    # across(c(starts_with("D."),starts_with("ln.")),list(L = lag),.names = "{.fn}1.{.col}"),
    # across(c(starts_with("D."),starts_with("ln.")),list(L = ~lag(.,2)),.names = "{.fn}2.{.col}"),
    # across(c(starts_with("D."),starts_with("ln.")),list(L = ~lag(.,3)),.names = "{.fn}3.{.col}"),
  # across(c(starts_with("D."),starts_with("ln.")),list(L = ~lag(.,4)),.names = "{.fn}4.{.col}"),
  mutate(index = 1:n()) %>% 
    relocate(index) %>% 
    mutate(q = lubridate::quarter(time, with_year = FALSE)) %>% 
    fastDummies::dummy_cols(select_columns = "q", remove_first_dummy = TRUE, 
                            remove_selected_columns = TRUE) -> cleaned_data
  
  return(cleaned_data)
  
}

clean_data(imports_data, max.lag = 4)



lag_multiple <- function(x, n = 1L){
  map(n, dplyr::lag, x = x) %>% 
    as_tibble(.name_repair = "minimal") %>% 
    #set_names(paste0("L",n,".",cur_column()))
    set_names(paste0(n,"."))
}


lag_multiple <- function(x, cols, n = 1L){
  internal_fun <- function(n, x){
    map(n, dplyr::lag, x = x) %>% 
      as_tibble(.name_repair = "minimal") %>% 
      #set_names(paste0("L",n,".",cur_column()))
      set_names(paste0(n,"."))
  }
  
  x %>% 
    mutate(across(vars(cols), ~internal_fun(., n))) -> intermed
  
  intermed %>% 
    rename_with(across(vars(cols)))
  
}



imports_data %>% 
  select(na_item, time,values) %>%
  pivot_wider(id_cols = time, names_from = na_item, values_from = values) %>% 
  clean_names() %>% 
  
  arrange(.,time) %>% 
  
  timetk::tk_augment_lags(.value = c(imports_of_goods_and_services, gross_capital_formation), 
                          .lags = 1:max.lag, .names = paste0("text",1:(max.lag*2)))




mutate(# ECM
  across(c(-time),
         ~lag_multiple(. , n = max.lag),
         #list(L = ~lag_multiple(. , n = 1:max.lag)),
         #.names = "{.fn}.{col}"
  )) %>% names











imports_data %>% 
  select(na_item, time,values) %>%
  pivot_wider(id_cols = time, names_from = na_item, values_from = values) %>% 
  clean_names()

clean_data(imports_data, max.lag = 4)

imports_data %>% 
  select(na_item, time,values) %>%
  pivot_wider(id_cols = time, names_from = na_item, values_from = values) %>% 
  clean_names() %>% 
  arrange(.,time) %>% 
  mutate(across(-time,list(ln = log),.names = "{.fn}.{.col}"),
         across(starts_with("ln."),list(D = ~c(NA,diff(., ))), .names = "{.fn}.{.col}")) %>% 
  select(time, imports_of_goods_and_services, ln.imports_of_goods_and_services) -> test

test %>% 
  # mutate(across(c(imports_of_goods_and_services, ln.imports_of_goods_and_services), 
  #               list( L = ~lag_multiple(.,n = 1:4))))
  
  mutate(
    across(c(starts_with("D."),starts_with("ln.")),
           list(L = ~lag_multiple(. , n = max.lag)),
           .names = "{.fn}.{col}"
    ))



# Add previously estimated data
full_join(cp_estimated %>% select(-index), by = "time") %>% 
  
  arrange(time) %>% 
  mutate(across(-time,list(ln = log),.names = "{.fn}.{.col}"),
         across(starts_with("ln."),list(D = ~c(NA,diff(., ))), .names = "{.fn}.{.col}"),
         
         # ECM
         across(c(starts_with("D."),starts_with("ln.")),list(L = lag),.names = "{.fn}1.{.col}"),
         across(c(starts_with("D."),starts_with("ln.")),list(L = ~lag(.,2)),.names = "{.fn}2.{.col}"),
         across(c(starts_with("D."),starts_with("ln.")),list(L = ~lag(.,3)),.names = "{.fn}3.{.col}"),
         across(c(starts_with("D."),starts_with("ln.")),list(L = ~lag(.,4)),.names = "{.fn}4.{.col}"),
         index = 1:n()) %>% 
  relocate(index) %>% 
  mutate(q = quarter(time, with_year = FALSE)) %>% 
  dummy_cols(select_columns = "q", remove_first_dummy = TRUE, remove_selected_columns = TRUE) -> imports_data_clean