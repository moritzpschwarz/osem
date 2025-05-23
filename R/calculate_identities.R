#' Calculate values for identity/definition variables not available in Eurostat
#'
#' Calculates true values for variables that are given by identity or definition but are not directly available from Eurostat.
#'
#' @param specification A tibble or data.frame as returned by
#'   \code{\link{translate_variables}}.
#' @param data A tibble or data.frame containing the downloaded or locally
#'   loaded data as returned by \code{\link{load_or_download_variables}}.
#' @inheritParams translate_variables
#'
#' @return Returns the original tibble or data.frame \code{data} with added
#'   values for the identity/definition variables that were not available in
#'   Eurostat.
#'

calculate_identities <- function(specification, data, dictionary = NULL) {
  # identity must be given as a module (i.e. must be a dependent variable)
  identities <- specification %>%
    dplyr::filter(.$type == "d")

  # not sure whether can solve without dropping these vars
  # could add back later but not necessary?
  #dat <- data %>% dplyr::select(-dplyr::any_of(c("unit","geo","s_adj")))
  dat <- data %>% dplyr::select(dplyr::any_of(c("time","na_item","values")))

  for (i in seq_len(nrow(identities))) {
    identity <- identities[i, ]
    dep <- identity$dependent
    indep <- identity$independent

    dat %>%
      tidyr::pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values") -> dat_tmp

    # make sure the column names are not using * as denominator for NACE codes
    # when parsing this, it would appear that we would need to multiply the values
    # therefore changing the denominator from * to _ only for here
    # UPDATE: do not allow for * in variable names, instead interpret it as multiplication
    dat_tmp %>%
      dplyr::mutate(!!dep := eval(parse(text = indep))) %>%
      dplyr::select(-dplyr::any_of("nace_r2")) %>%
      tidyr::pivot_longer(cols = !"time", names_to = "na_item", values_to = "values") -> dat
  }

  return(dat %>%
           dplyr::as_tibble() %>%
           dplyr::select(-dplyr::any_of("nace_r2")))

}
