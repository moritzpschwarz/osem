#' Add the estimated fitted values back to the original
#'
#' @param clean_data An input data.frame or tibble. Must be the output of clean_data() to fit all requirements.
#' @param isat_object An object of class 'isat'. Most likely should be the 'best_model' element that is returned by the 'estimate_module()' function.
#' @param dep_var_name A character string of the name of the dependent variable as contained in clean_data() in a level form (i.e. no ln or D in front of the name).
#' @param ardl_or_ecm Either 'ardl' or 'ecm' to determine whether to estimate the model as an Autoregressive Distributed Lag Function (ardl) or as an Equilibrium Correction Model (ecm).
#'
#' @return
#' @export
#'
#' @examples
add_to_original_data <- function(clean_data,
                                 isat_object,
                                 dep_var_name = "imports_of_goods_and_services",
                                 ardl_or_ecm = "ardl"){

  if(!"index" %in% names(clean_data)){stop("Clean Data Object should have an index i.e. a 1:nrow(clean_data) column that allows us to join the estimated data again with model$aux$y.index.")}

  clean_data %>%

    full_join(tibble(index = isat_object$aux$y.index,
                     fitted = as.numeric(isat_object$mean.fit)), by = "index") %>%

    {if(ardl_or_ecm == "ecm") {
      mutate(.,
             fitted.cumsum = case_when(
               is.na(fitted) & is.na(lead(fitted)) ~ 0,
               is.na(fitted) & !is.na(lead(fitted)) ~ get(paste0("L.",dep_var_name)), #L.imports_of_goods_and_services,
               !is.na(fitted) ~ fitted),
             fitted.cumsum = cumsum(fitted.cumsum),
             fitted.cumsum = ifelse(is.na(fitted),NA,fitted.cumsum))} else {.}} %>%


    {if(ardl_or_ecm == "ecm") {
      mutate(., fitted.level = exp(fitted.cumsum))
    } else if (ardl_or_ecm == "ardl") {
      mutate(., fitted.level = exp(fitted))
    } else{
      .
    }} -> intermed

  intermed %>%

    rename_with(.cols = any_of(c("fitted","fitted.level","fitted.cumsum")), .fn = ~gsub("fitted",dep_var_name,.)) %>%

    return()


}
