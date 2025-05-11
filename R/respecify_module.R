respecify_module <- function(model, dep_var = "EmiCO2ManInd", equation = "ln.EmiCO2ManInd ~ ln.RealVAIndustry + ln.PriceETS", mc = TRUE){

  # check in module order whether dep_var was an estimated model with type n
  if(model$module_order %>%
     filter(dependent == dep_var) %>%
     pull(type) != "n"){
    stop("Dependent variable not found in model or not of type n")
  }

  model$module_order %>%
    filter(dependent == dep_var)

  dat <- model$full_data
  for(i in 1:nrow(model$module_collection)){
    if(i == 1){
      dat <- model$module_collection$dataset[i][[1]] %>%
        select(-any_of("index")) %>%
        pivot_longer(-time) %>%
        filter(name != "trend") %>%
        filter(!grepl("\\.hat$",name))
    } else {
      dat %>%
        bind_rows(model$module_collection$dataset[i][[1]] %>%
                    select(-any_of("index")) %>%
                    pivot_longer(-time) %>%
                    filter(name != "trend") %>%
                    filter(!grepl("\\.hat$",name))) -> dat
    }
  }

  dat %>%
    drop_na(value) %>%
    distinct() %>%
    pivot_wider(names_from = name, values_from = value) -> dat

  # break up equation in dependent variable and independent variables
  eq_unlisted <- strsplit(equation, "~") %>% unlist
  dep_var_processed <- eq_unlisted[1] %>% trimws
  indep_vars <- eq_unlisted[2] %>% trimws %>% strsplit(" \\+ ") %>% unlist %>% trimws

  # isolate the ar specification by looking for ar[0-9]+
  ar <- indep_vars %>% str_subset("^ar[0-9]+") %>% str_extract("[0-9]+") %>% as.numeric
  if(identical(ar, numeric(0))){
    ar <- NULL
  }

  # Add SIS
  if(!identical(indep_vars %>% str_subset("^sis[0-9]+-[0-9]+-[0-9]+"), character(0))){
    sis <- indep_vars %>% str_subset("^sis[0-9]+-[0-9]+-[0-9]+") %>% str_extract("[0-9]+-[0-9]+-[0-9]+") %>% as.Date
    times <- dat %>% arrange(time) %>% pull("time")
    dat %>%
      arrange(time) %>%
      bind_cols(gets::sim(zoo::zoo(1:length(times), order.by = times),which.ones = sis)) -> dat
  }


  # add IIS
  if(!identical(indep_vars %>% str_subset("^iis[0-9]+-[0-9]+-[0-9]+"), character(0))){
    iis <- indep_vars %>% str_subset("^iis[0-9]+-[0-9]+-[0-9]+") %>% str_extract("[0-9]+-[0-9]+-[0-9]+") %>% as.Date
    times <- dat %>%  arrange(time) %>% pull("time")
    dat %>%
      arrange(time) %>%
      bind_cols(gets::iim(zoo::zoo(1:length(times), order.by = times),which.ones = iis) %>%
                  as_tibble() %>%
                  setNames(paste0("iis",iis))) -> dat
  }


  # check that both dep_var and indep_vars are in dat
  if(!(dep_var_processed %in% colnames(dat) & all(indep_vars[!grepl("^ar[0-9]+|trend", indep_vars)]  %in% colnames(dat)))){
    stop("Dependent variable or independent variables specified in equation not found in dataset")
  }

  y <- dat %>%  arrange(time) %>% select("time",all_of(dep_var_processed))
  y <- zoo::zoo(y[,2, drop = TRUE], order.by = y$time)

  # if trend is in indep_vars, add one
  if("trend" %in% indep_vars){
    dat$trend <- 1:nrow(dat)
  }
  mxreg <- dat %>% arrange(time) %>% select("time",all_of(indep_vars[!grepl("^ar[0-9]+", indep_vars)]))
  mxreg <- zoo::zoo(mxreg[,!names(mxreg) %in% "time", drop = FALSE], order.by = mxreg$time)

  out <- list()

  out$model <- gets::arx(y = y,
                         mxreg = mxreg,
                         mc = mc,
                         ar = ar)

  if(exists("sis")){
    out$model$ISnames <- paste0("sis",sis)
  }
  if(exists("iis")){
    out$model$ISnames <- paste0("iis",iis)
  }
  if(exists("iis") & exists("sis")){
    out$model$ISnames <- c(paste0("iis",iis), paste0("sis",sis))
  }


  colnames(out$model$aux$mX) <- out$model$aux$mXnames
  out$model$aux$args <- if(i != 0){list(ar = ar)} else {list(ar = NULL)}
  out$model$aux$y.name <- dep_var_processed
  out$model$aux$args$mc <- mc

  out$dat <- dat
  return(out)
}
