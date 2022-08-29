#' Print output of an Aggregate Model
#'
#' @param model An object of type 'aggmod'
#'
#' @return
#' @export
#'

print.aggmod <- function(model){


  cat("Aggregate Model Output\n")
  cat("-----------------------\n")

  cat(paste0("\nEstimation Options:"))
  cat(paste0("\nSample:"))
  cat(paste0("\nMax Lags Considered:"), ifelse(is.null(model$args$max.lag), 4, model$args$max.lag))
  cat(paste0("\nEstimation Option:"), ifelse(is.null(model$args$ardl_or_ecm), "ARDL", model$args$ardl_or_ecm))
  cat("\n")



  cat("\nRelationships considered: \n")

  dictionary <- {if(is.null(model$args$dictionary)){
    dict
  } else {
    model$args$dictionary
  } } %>%
    select(model_varname, full_name) %>%
    mutate(dependent = model_varname,
           splitvars = model_varname)



  model$module_order_eurostatvars %>%
    select(index, order, dependent, independent) %>%

    # deal with dependent vars
    left_join(dictionary %>% select(dependent, full_name), by = "dependent") %>%
    relocate(full_name, .after = dependent) %>%

    # deal with independet vars
    mutate(ind_spaced = independent,
           independent = gsub(" ", "", independent)) %>%
    rowwise() %>%
    mutate(splitvars = list(strsplits(independent,c("\\-", "\\+")))) %>%
    unnest(splitvars, keep_empty = TRUE) %>%
    left_join(dictionary %>% select(splitvars, full_name) %>% rename(name_ind = full_name), by = "splitvars") %>%

    group_by(index, dependent,full_name,  ind_spaced) %>%
    summarise(ind_name = toString(name_ind), .groups = "drop") %>%
    mutate(ind_name = ifelse(ind_name == "NA","Only AR Specification", ind_name)) %>%

    # styling
    rename(`Ind. Var` = ind_spaced,
           `Model` = index,
           #`Est. Order` = order,
           `Dep. Var.` = dependent,
           `Full Name Ind. Var` = ind_name,
           `Full Name Dep. Var` = full_name) %>%

    print


  cat("\n\nRelationships estimated in the order: ",paste0(model$module_collection$index, collapse = ","))



}
