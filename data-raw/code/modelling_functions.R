isat_modelling <- function(clean_data, 
                           dep_var_basename = "imports_of_goods_and_services", 
                           x_vars_basename = c("gross_capital_formation",
                                               "household_and_npish_final_consumption_expenditure"),
                           use_logs = c("both","y","x"),
                           ardl_or_ecm = "ardl",
                           max.lag = 4,
                           saturation.tpval = 0.01){
  
  log_opts <- match.arg(use_logs)
  
  isat_list <- tibble(ar = 0:max.lag,
                      BIC = 0,
                      isat_object = list(NA_complex_))
  for(i in 0:max.lag){
    
    if(ardl_or_ecm == "ardl"){
      if(log_opts %in% c("both","x")){
        xvars_names <- grep("L[0-9]\\.ln",
                            grep(paste0(x_vars_basename,collapse = "|"),names(clean_data), value = TRUE), value = TRUE)
      } else {
        xvars_names <- grep("L[0-9]\\.",
                            grep(paste0(x_vars_basename,collapse = "|"),names(clean_data), value = TRUE), value = TRUE)
      }
      
      yvar <- clean_data %>% 
        select(all_of(paste0(ifelse(log_opts %in% c("both","y"),"ln.",""),dep_var_basename))) %>% 
        pull 
      
      xvars <- clean_data %>% 
        select(all_of(paste0(ifelse(log_opts %in% c("both","x"),"ln.",""),x_vars_basename)),
               if(i != 0) {all_of(xvars_names[grepl(paste0(1:i,collapse = "|"),xvars_names)])} else {NULL}, 
               q_2,q_3,q_4)
    }
    if(ardl_or_ecm == "ecm"){
      yvar <- clean_data %>% 
        select(all_of(paste0(ifelse(log_opts %in% c("both","y"),"D.ln.","D."),dep_var_basename))) %>% 
        pull 
      
      xvars_names <- grep("L[0-9]\\.D.",
                          grep(paste0(x_vars_basename,collapse = "|"),names(clean_data), value = TRUE), value = TRUE)
      
      xvars <- clean_data %>% 
        select(all_of(paste0(ifelse(log_opts %in% c("both","y"),"L1.ln.","L1."),dep_var_basename)),
               all_of(paste0(ifelse(log_opts %in% c("both","x"),"L1.ln.","L1."),x_vars_basename)),
               all_of(paste0(ifelse(log_opts %in% c("both","x"),"D.ln.","D."),x_vars_basename)),
               if(i != 0) {all_of(xvars_names[grepl(paste0(1:i,collapse = "|"),xvars_names)])} else {NULL}, 
               q_2,q_3,q_4)
    }
    
    
    intermed.model <- isat(y = yvar,
                           mxreg = xvars, 
                           ar = if (i != 0) {1:i} else{NULL},
                           plot = FALSE,
                           print.searchinfo = FALSE,
                           iis = TRUE,
                           sis = TRUE,
                           t.pval = saturation.tpval)
    
    
    
    isat_list[i + 1,"BIC"] <- BIC(intermed.model)
    isat_list[i + 1,"isat_object"] <- tibble(isat_object = list(intermed.model))
    
  }
  
  out <- list()
  out$isat_list <- isat_list
  out$best_model <- isat_list %>% filter(BIC == min(BIC)) %>% pull(isat_object) %>% first 
  out$args <- list(clean_data,dep_var_basename,x_vars_basename, use_logs, ardl_or_ecm, max.lag)
  
  return(out)
}



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