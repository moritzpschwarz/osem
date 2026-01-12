forecast_block_loop <- function(
    model,
    i,
    exog_df_ready,
    exog_df_ready_full,
    n.ahead,
    current_spec,
    prediction_list,
    uncertainty_sample,
    nowcasted,
    ci.levels){

  # create a forecasting loop for all the subelements
  model$module_order %>%
    dplyr::mutate(block_elements = dplyr::n(), .by = "block_order") %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull("block_order") -> cur_block_order

  model$module_order %>%
    dplyr::filter(.data$block_order == cur_block_order) %>%
    dplyr::pull("order") -> indices_in_cur_block

  model$module_order %>%
    dplyr::filter(.data$block_order == cur_block_order) %>%
    dplyr::pull("sub_order") -> sub_order_in_cur_block

  mod_model <- model

  # sub_order_dep_data_collection <- dplyr::tibble()
  # for(j in sub_order_in_cur_block){
  #
  #   # in estimation of V = lag(W), W would not be in the data for module V
  #   # therefore we need to add that data to
  #   # ensure that the variable is in the data
  #
  #   sub_order_dep <- mod_model$module_collection %>%
  #     dplyr::filter(.data$block_order %in% cur_block_order,
  #                   .data$sub_order %in% j) %>%
  #     dplyr::pull("dep") %>%
  #     dplyr::first()
  #
  #   sub_order_data <- mod_model$module_collection %>%
  #     dplyr::filter(.data$block_order %in% cur_block_order,
  #                   .data$sub_order %in% j) %>%
  #     dplyr::pull("dataset") %>%
  #     dplyr::first()
  #
  #   sub_order_dep_data <- sub_order_data %>%
  #     dplyr::select("time",dplyr::all_of(sub_order_dep))
  #
  #   if(nrow(sub_order_dep_data_collection)==0){
  #     sub_order_dep_data_collection <- sub_order_dep_data
  #   } else {
  #     sub_order_dep_data_collection <- sub_order_dep_data_collection %>%
  #       dplyr::full_join(sub_order_dep_data, by = "time")
  #   }
  # }
  #
  # #overall_block_data <- list()
  # for(j in sub_order_in_cur_block){
  #
  #   init_dat <- mod_model$module_collection[mod_model$module_collection$block_order %in% cur_block_order &
  #                                             mod_model$module_collection$sub_order %in% j, "dataset"][[1]][[1]]
  #
  #   vars_to_add <- names(sub_order_dep_data_collection)[!names(sub_order_dep_data_collection) %in% names(init_dat)]
  #
  #   if(!identical(vars_to_add, character(0))){
  #     # if there are no variables to add, then skip
  #     # add the variable that is not in there from sub_order_dep_data_collection
  #     new_dat <- init_dat %>%
  #       dplyr::full_join(sub_order_dep_data_collection %>% dplyr::select("time", dplyr::all_of(vars_to_add)), by = "time")
  #
  #     mod_model$module_collection[mod_model$module_collection$block_order %in% cur_block_order &
  #                                   mod_model$module_collection$sub_order %in% j, "dataset"][[1]][[1]] <- new_dat
  #   }
  # }

  # now run in steps of 1 to n.ahead alternating between the sub-orders

  for(k in 1:n.ahead){
    for(j in sub_order_in_cur_block){
      #print(paste0("Forecasting block ", cur_block_order, " sub-order ", j, " step ", k))

      order_i <- mod_model$module_collection[mod_model$module_collection$block_order %in% cur_block_order &
                                               mod_model$module_collection$sub_order %in% j, "order"][[1]]


      current_spec <- model$module_order %>%
        dplyr::filter(.data$order == order_i) %>%
        # save original form of independent col
        dplyr::mutate(independent_orig = .data$independent) %>%
        # make sure each independent variable has a separate row
        dplyr::mutate(independent = gsub(" ", "", .data$independent)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(independent = list(strsplits(.data$independent, c("\\-", "\\+", "/", "\\*")))) %>%
        # following line added to deal with AR models when ind_vars is a list of NULL
        dplyr::bind_rows(dplyr::tibble(independent = list(""))) %>%
        tidyr::unnest("independent", keep_empty = TRUE) %>%
        tidyr::drop_na("index") %>%
        dplyr::select("index", "dependent", "independent", "independent_orig")


      prediction_list_mod <- prediction_list

      slice_or_pad <- function(x, k) {
        if (is.null(x)) {return(NULL)}
        if (identical(x, NA_complex_)) {return(NA_complex_)}
        if (nrow(x) < k) {
          # pad with NA rows to reach k
          x %>% rbind(NA)
        } else {
          dplyr::slice(x, 1:k)
        }
      }

      prediction_list_mod$central.estimate <- lapply(prediction_list_mod$central.estimate, slice_or_pad, k = k)
      prediction_list_mod$all.estimates <- lapply(prediction_list_mod$all.estimates, slice_or_pad, k = k)
      prediction_list_mod$predict.isat_object <- lapply(prediction_list_mod$predict.isat_object, slice_or_pad, k = k)

      forecast_module_estimated(
        model = mod_model,
        i = order_i,
        exog_df_ready = exog_df_ready %>% dplyr::slice(1:k),
        exog_df_ready_full = exog_df_ready_full %>% dplyr::slice(1:k),
        n.ahead = k,
        current_spec = current_spec,
        prediction_list = prediction_list_mod,
        uncertainty_sample = uncertainty_sample,
        nowcasted = if(is.data.frame(nowcasted)) {nowcasted %>% dplyr::slice(1:k)} else {nowcasted},
        ci.levels = ci.levels
      ) -> prediction_list_mod


      prediction_list[prediction_list$order == order_i,] <- prediction_list_mod[prediction_list_mod$order == order_i,]
    }
  }
  return(prediction_list)
}
