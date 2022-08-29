forecast_model <- function(model, exog_predictions = NULL, n.ahead = 4){

  if(class(model) != "aggmod"){stop("Forecasting only possible with an aggmod object. Execute 'run_model' to get such an object.")}


  # 1. Determine Exogenous Variables and wrangle future values ---------------

  # determine classification of variables: exogenous, endogenous by model, endogenous by identity/definition
  classification <- classify_variables(specification = model$module_order_eurostatvars)

  classification %>%
    filter(class == "x") %>%
    pull(var) -> exog_vars


  if(is.null(exog_predictions)){
    message("No exogenous values provided. Model will use the last available value.")
    exog_df <- model$full_data %>%
      filter(na_item %in% exog_vars) %>%
      group_by(na_item) %>%
      filter(time == max(time))

    if(!all(exog_df$time == exog_df$time[1])){
      warning("Latest Exogenous Data is not available for all variables. For those where most recent data is not available, the period before that is used.")
    }
  }


  exog_df %>%
    group_by(na_item) %>%
    rowwise() %>%
    mutate(time = list(seq(time, length = n.ahead+1, by = "3 months")[1:n.ahead+1])) %>%
    unnest(time) %>%
    ungroup %>%
    mutate(q = lubridate::quarter(time, with_year = FALSE)) %>%
    pivot_wider(names_from = "na_item",values_from = "values", id_cols = c(time,q)) %>%

    fastDummies::dummy_cols(
      select_columns = "q", remove_first_dummy = FALSE,
      remove_selected_columns = TRUE) -> exog_df_ready


  # 2. Forecasting step by step according to model order ------------------------------------------------

  for(i in seq(model$module_order_eurostatvars$order)){
    # i = 1

    current_spec <- model$module_order_eurostatvars %>%
      filter(order == i) %>%
      mutate(independent = gsub(" ", "", independent)) %>%
      rowwise() %>%
      mutate(ind_vars = list(strsplits(independent,c("\\-", "\\+")))) %>%
      unnest(ind_vars, keep_empty = TRUE) %>%
      select(index, dependent_eu, independent_eu)

    # check if current_spec is fully exogenous
    if(all(current_spec$independent_eu %in% names(exog_df_ready))){
      model$module_collection %>%
        filter(order == i) %>%
        pull(model) %>% .[[1]] -> isat_obj

      exog_df_ready %>%
        select(-time) %>%
        #select(all_of())
        as.matrix

      predict(isat_obj, newmxreg = , n.ahead = 4)



    }


    }




}
