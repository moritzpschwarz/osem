#' Internal function to run and prepare isat estimation
#' @param yvar The y variable
#' @param y.name The name of the y variable.
#' @param xvars All x variables.
#' @param mc Logical. Whether to use an intercept.
#' @param ar numeric vector or \code{NULL}. The AR order to be used.
#' @inheritParams estimate_module
#' @inheritParams run_module
#' @param determine.blocksize Logical. Whether max.block.size or needs to be determined.
#'
#' @returns An isat object.
#'
run_isat <- function(yvar,
                     y.name,
                     xvars,
                     clean_data,
                     ar = NULL,
                     mc,
                     saturation,
                     saturation.tpval,
                     max.block.size,
                     pretest_steps,
                     determine.blocksize){

  # debug_list <- list(yvar = yvar, xvars = xvars,i = i,saturation.tpval = saturation.tpval)
  # save(debug_list, file = "debug_list.RData")

  # try to prevent that isat does not run out of degrees of freedom
  if(determine.blocksize){
    if((ncol(xvars) + ((max.block.size*2)+1)) > nrow(xvars)){
      maxblocksize <- round((nrow(xvars) - ncol(xvars))/3)
    } else {maxblocksize <- max.block.size}

    if(maxblocksize < 1){
      stop(paste0("Specification not valid - the sample for estimating the module with the dependent variable ",y.name," is not extensive enough to be estimated with lag ",ar,".\n Specification skipped."))
    }
  } else {
    maxblocksize <- max.block.size
  }
  if(!is.null(xvars)){
    if(tidyr::drop_na(cbind(yvar,dplyr::as_tibble(xvars))) %>% nrow == 0){
      stop()
    }
    xvar_opts <- if(nrow(zoo::zoo(xvars, order.by = clean_data$time))>0){
      zoo::zoo(xvars, order.by = clean_data$time)
    } else {NULL}
  } else {
    xvar_opts <- NULL
  }


  # Pre-impose Steps  ----------------------------------------------------------------
  if("SIS" %in% saturation & pretest_steps){
    init_sis_mod <- gets::isat(
      y = zoo::zoo(yvar, order.by = clean_data$time),
      mxreg = xvar_opts,
      ar = ar,
      mc = mc,
      plot = FALSE,
      print.searchinfo = FALSE,

      sis = TRUE,

      iis = FALSE,
      tis = FALSE,
      t.pval = saturation.tpval,
      max.block.size = maxblocksize,
      include.gum = FALSE
    )

    if("IIS" %in% saturation | "TIS" %in% saturation){
      dplyr::tibble(time = clean_data$time) %>%
        dplyr::mutate(index = 1:dplyr::n()) %>%
        dplyr::filter(.data$time %in% gets::isatdates(init_sis_mod)$sis$date) %>%
        dplyr::pull("index") -> sis_dates_index

      if(!identical(sis_dates_index, integer(0))){
        add_steps <- gets::sim(length(yvar),
                               which.ones = sis_dates_index) %>%
          setNames(gets::isatdates(init_sis_mod)$sis$breaks)

        new_data <- zoo::zoo(dplyr::bind_cols(xvar_opts, add_steps), order.by = clean_data$time)
      } else {
        new_data <- zoo::zoo(xvar_opts, order.by = clean_data$time)
      }

      super_sat <- gets::isat(y = zoo::zoo(yvar, order.by = clean_data$time),
                              mxreg = new_data,
                              ar = ar,
                              mc = mc,
                              sis = FALSE,
                              iis = ifelse("IIS" %in% saturation, TRUE, FALSE),
                              tis = ifelse("TIS" %in% saturation, TRUE, FALSE),
                              plot = FALSE,
                              print.searchinfo = FALSE,
                              t.pval = saturation.tpval,
                              include.gum = FALSE,
                              max.block.size = maxblocksize)

      # union gets
      keep_num <- which(row.names(super_sat$mean.results) %in% c("mconst",paste0("ar",ar), names(xvar_opts)))
      super_sat_sel <- gets::gets(super_sat,
                                  t.pval = saturation.tpval, # because we are still dealing with indicators, use this t.pval not the one for selection
                                  keep = keep_num,
                                  print.searchinfo = FALSE,
                                  ar.LjungB = NULL,
                                  arch.LjungB = NULL,
                                  normality.JarqueB = NULL,
                                  plot = FALSE)

      # modify the super_sat_sel into an isat object
      intermed.model <- super_sat_sel
      intermed.model$aux$y.name <- y.name
      class(intermed.model) <- "isat"
      intermed.model$aux$y.index <- super_sat$aux$y.index
      zoo::index(intermed.model$mean.fit) <- super_sat$aux$y.index
      colnames(intermed.model$aux$mX) <- intermed.model$aux$mXnames
      intermed.model$ISnames <- grep("^sis[0-9]+|^iis[0-9]+|^tis[0-9]+",colnames(intermed.model$aux$mX), value = TRUE)
      intermed.model$ISfinalmodels <- list(colnames(intermed.model$aux$mX))
      intermed.model$gets.type <- "isat"
      attr(intermed.model$coefficients, "names") <- row.names(intermed.model$mean.results)
      names(intermed.model$specific.spec) <- row.names(intermed.model$mean.results)
      intermed.model$diagnostics <- intermed.model$specific.diagnostics

      intermed.model$call$ar <- ar
      intermed.model$call$iis <- ifelse("IIS" %in% saturation, TRUE, FALSE)
      intermed.model$call$sis <- ifelse("SIS" %in% saturation, TRUE, FALSE)
      intermed.model$call$tis <- ifelse("TIS" %in% saturation, TRUE, FALSE)

      intermed.model$aux$args$ar <- ar
      intermed.model$aux$args$iis <- ifelse("IIS" %in% saturation, TRUE, FALSE)
      intermed.model$aux$args$sis <- ifelse("SIS" %in% saturation, TRUE, FALSE)
      intermed.model$aux$args$tis <- ifelse("TIS" %in% saturation, TRUE, FALSE)

    } else {
      intermed.model <- init_sis_mod
    }
  } else {

    # if SIS is FALSE, just do the standard isat
    try(intermed.model <- gets::isat(
      y = zoo::zoo(yvar, order.by = clean_data$time),
      mxreg = xvar_opts,
      ar = ar,
      mc = mc,
      plot = FALSE,
      print.searchinfo = FALSE,
      iis = ifelse("IIS" %in% saturation, TRUE, FALSE),
      sis = ifelse("SIS" %in% saturation, TRUE, FALSE),
      tis = ifelse("TIS" %in% saturation, TRUE, FALSE),
      t.pval = saturation.tpval,
      max.block.size = maxblocksize,
      include.gum = FALSE,
    ), silent = TRUE)
  }

  # TODO necessary to add the tis argument to the call due to error in gets package
  if(exists("intermed.model")){intermed.model$call$tis <- intermed.model$aux$args$tis}
  if(exists("intermed.model")){intermed.model$aux$y.name <- y.name}

  if(exists("intermed.model")){return(intermed.model)}


}
