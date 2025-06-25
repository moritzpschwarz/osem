#' Extract relevant information from the osem model object for forecasting and nowcasting
#' @inheritParams forecast_model
#' @inheritParams forecast_setup_estimated_relationships
#' @return The relevant information in a list format that enables forecasting and nowcasting
#'
forecast_extract_info <- function(model, i, n.ahead, exog_df_ready){

  # get isat obj
  model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model) %>% .[[1]] -> isat_obj

  # get data obj (that is the data that was used in the estimation)
  model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$dataset) %>% .[[1]] -> data_obj

  # determine ARDL or ECM
  is_ardl <- is.null(model$args$ardl_or_ecm) | identical(model$args$ardl_or_ecm,"ARDL")

  # determine log y
  ylog <- model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model.args) %>%
    .[[1]] %>%
    .$use_logs %in% c("both","y")

  # determine log x
  xlog <- model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model.args) %>%
    .[[1]] %>%
    .$use_logs %in% c("both","x")

  # determine x vars
  x_vars_basename <- model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model.args) %>%
    .[[1]] %>%
    .$x_vars_basename

  y_vars_basename <- model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model.args) %>%
    .[[1]] %>%
    .$dep_var_basename

  # check quarterly dummies to drop
  q_pred_todrop <- c("q_1","q_2","q_3","q_4")[!c("q_1","q_2","q_3","q_4") %in% colnames(isat_obj$aux$mX)]

  # check if mconst is used
  if ("mconst" %in% colnames(isat_obj$aux$mX)) {
    mconst <- TRUE
  } else {
    mconst <- FALSE
  }

  # identify any ar terms in the estimated data
  pred_ar_needed <- colnames(isat_obj$aux$mX)[grepl("ar[0-9]+",colnames(isat_obj$aux$mX))]
  # identify any dl terms in the estimated data
  pred_dl_needed <- colnames(isat_obj$aux$mX)[grepl("^L[0-9]+",colnames(isat_obj$aux$mX))]

  # this condition checks whether there are any ar terms that need to be created
  if (!is.null(pred_ar_needed) & !identical(character(0),pred_ar_needed)) {

    # if we need AR terms, the following loop creates the names of those variables (incl. considering whether they are logged)
    ar_vec <- 0:max(as.numeric(gsub("ar","",pred_ar_needed)))
    y_names_vec <- c()
    for (ar in ar_vec) {
      # ar = 0
      y_names_vec <- c(y_names_vec,paste0(paste0(ifelse(ar == 0,"",paste0("L",ar,"."))),ifelse(ylog,"ln.",""),y_vars_basename))
    }
  } else {
    # if we do not need any AR terms then we simply use the standard name (and add ln. if necessary)
    y_names_vec <- paste0(ifelse(ylog,"ln.",""),y_vars_basename)
    ar_vec <- 0
  }

  # TODO: check whether the AR vector is the correct one for the x variables
  if (!identical(character(0),x_vars_basename)) {
    x_names_vec_nolag <- paste0(ifelse(xlog,"ln.",""),x_vars_basename)
    x_names_vec <- c(x_names_vec_nolag, pred_dl_needed)
  } else {
    x_names_vec <- NULL
    x_names_vec_nolag <- NULL
  }

  # get iis dummies
  if (!is.null(gets::isatdates(isat_obj)$iis)) {
    iis_pred <- matrix(0,
                       nrow = nrow(exog_df_ready),
                       ncol = nrow(gets::isatdates(isat_obj)$iis),
                       dimnames  = list(NULL,
                                        gets::isatdates(isat_obj)$iis$breaks)) %>%
      dplyr::as_tibble()
  }

  # get sis dummies
  if (!is.null(gets::isatdates(isat_obj)$sis)) {
    sis_pred <- matrix(1,
                       nrow = nrow(exog_df_ready),
                       ncol = nrow(gets::isatdates(isat_obj)$sis),
                       dimnames  = list(NULL,
                                        gets::isatdates(isat_obj)$sis$breaks)) %>%
      dplyr::as_tibble()
  }

  if ("trend" %in% names(coef(isat_obj))) {
    trend_pred <- dplyr::tibble(trend = (max(isat_obj$aux$mX[,"trend"]) + 1):(max(isat_obj$aux$mX[,"trend"]) + n.ahead))
  }

  # adding together all variables apart from x-variables (so IIS, SIS, trends, etc.)  -------------------------------
  exog_df_ready %>%

    # select the relevant variables
    dplyr::select("time", dplyr::any_of(c("q_1","q_2","q_3","q_4")), dplyr::any_of(names(data_obj))) %>%

    # drop not used quarterly dummies
    dplyr::select(-dplyr::any_of(q_pred_todrop)) %>%

    {if ("trend" %in% names(coef(isat_obj))) {
      dplyr::bind_cols(.,trend_pred)
    } else { . }} %>%

    {if (!is.null(gets::isatdates(isat_obj)$iis)) {
      dplyr::bind_cols(.,iis_pred)
    } else { . }} %>%

    {if (!is.null(gets::isatdates(isat_obj)$sis)) {
      dplyr::bind_cols(.,sis_pred)
    } else { . }} %>%

    {if (xlog) {
      dplyr::mutate(.,
                    #dplyr::across(.cols = dplyr::any_of(x_vars_basename), .fns = list(ln = log), .names = "{.fn}.{.col}"),
                    #dplyr::across(dplyr::starts_with("ln."), list(D = ~ c(NA, diff(., ))), .names = "{.fn}.{.col}"
                    dplyr::across(.cols = dplyr::any_of(x_vars_basename), .fns = ~ if (any(. <= 0, na.rm = TRUE)) {asinh(.)} else {log(.)}, .names = "ln.{.col}")
      )
    } else {.}} -> current_pred_raw

  current_pred_raw_all <- current_pred_raw


  out <- list()
  out$y_names_vec <- y_names_vec
  out$x_names_vec <- x_names_vec
  out$x_names_vec_nolag <- x_names_vec_nolag
  out$ar_vec <- ar_vec
  out$ylog <- ylog
  out$xlog <- xlog
  out$mconst <- mconst
  out$current_pred_raw <- current_pred_raw
  out$current_pred_raw_all <- current_pred_raw_all
  out$is_ardl <- is_ardl
  out$isat_obj <- isat_obj
  out$data_obj <- data_obj
  out$exog_df_ready <- exog_df_ready
  out$pred_ar_needed <- pred_ar_needed
  out$pred_dl_needed <- pred_dl_needed

  return(out)
}

