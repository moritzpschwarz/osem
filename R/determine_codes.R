#' Determine which Eurostat datasets need to be downloaded
#'
#' Takes the translated specification, extracts the Eurostat variable codes, and
#' uses a dictionary to check from which Eurostat datasets they should be
#' retrieved.
#'
#' @param specification A tibble or data.frame as returned by
#'   \code{\link{translate_variables}}.
#' @param dictionary A tibble or data.frame storing the Eurostat variable code
#'   in column 'eurostat_code' and the Eurostat dataset id in column
#'   'dataset_id' . When \code{NULL}, the \link[=dict]{default dictionary} is
#'   used.
#'
#' @return Returns a list with two character vectors:
#' \describe{
#'   \item{var.ids}{contains the Eurostat variable ids}
#'   \item{data.ids}{contains the Eurostat dataset ids.}
#' }
#'
#'

determine_datacodes <- function(specification, dictionary = NULL) {

  # new <- TRUE
  #
  # if(new){
  dictionary %>%
    dplyr::rowwise() %>%
    dplyr::mutate(codes.avail = dplyr::case_when(!is.na(nace_r2)~paste0(eurostat_code,"*",nace_r2),
                                                 TRUE ~eurostat_code)) %>%
    dplyr::pull(.data$codes.avail) -> codes.avail

  dictionary %>%
    dplyr::pull(.data$model_varname) -> model.ids

  # delete irrelevant values, such as TOTS
  codes.avail <- codes.avail[!is.na(dictionary$dataset_id)]

  # extract the codes used in the model
  dep.set <- specification$dependent_eu
  indep <- specification$independent_eu
  indep.set <- NULL

  for (i in seq_along(indep)) {

    # each element of character vector may contain formula, e.g. "A + B", must split
    vars <- strsplits(
      indep[i],
      c(" \\- ", " \\+ "))
    indep.set <- union(indep.set, vars)

  }
  indep.set <- gsub(" ", "", indep.set)
  codes.used <- union(dep.set, indep.set)

  # the codes that cannot be found in codes.avail must be aggregate level vars (e.g. TOTS)
  # don't need to download them
  codes.eurostat <- codes.used[which(codes.used %in% codes.avail)]

  # match the Eurostat variable codes to the Eurostat dataset id
  codes <- data.frame(eurostat_code = codes.eurostat)
  codes <- dplyr::left_join(x = codes, y = dictionary %>%
                              dplyr::select("eurostat_code", "nace_r2", "dataset_id") %>%
                              dplyr::rowwise() %>%
                              dplyr::mutate(eurostat_code = dplyr::case_when(!is.na(.data$nace_r2) ~ paste0(.data$eurostat_code, "*", .data$nace_r2),
                                                                             TRUE ~ .data$eurostat_code)), by = "eurostat_code")

  # which datasets need to be downloaded?
  dataset.ids <- unique(codes$dataset_id)



  # } else if(old){
  #   if (is.null(dictionary)) {
  #     dictionary <- aggregate.model::dict
  #   }
  #
  #   codes.avail <- dictionary$eurostat_code
  #   # delete irrelevant values, such as TOTS
  #   codes.avail <- codes.avail[!is.na(dictionary$dataset_id)]
  #
  #   # extract the codes used in the model
  #   dep.set <- specification$dependent_eu
  #   indep <- specification$independent_eu
  #   indep.set <- NULL
  #   for (i in seq_along(indep)) {
  #
  #     # each element of character vector may contain formula, e.g. "A + B", must split
  #     vars <- strsplits(
  #       indep[i],
  #       c("\\-", "\\+"))
  #     indep.set <- union(indep.set, vars)
  #
  #   }
  #   indep.set <- gsub(" ", "", indep.set)
  #   codes.used <- union(dep.set, indep.set)
  #
  #   # the codes that cannot be found in codes.avail must be aggregate level vars (e.g. TOTS)
  #   # don't need to download them
  #   codes.eurostat <- codes.used[which(!codes.used %in% dictionary$model_varname[is.na(dictionary$dataset_id)])]
  #
  #
  #
  #   # match the Eurostat variable codes to the Eurostat dataset id
  #   codes <- data.frame(eurostat_code = codes.eurostat)
  #   codes <- dplyr::left_join(x = codes, y = dictionary %>% dplyr::select(eurostat_code, dataset_id), by = "eurostat_code")
  #
  #   # which datasets need to be downloaded?
  #   dataset.ids <- unique(codes$dataset_id)
  # }


  # output list
  out <- list(var.ids = codes.eurostat, data.ids = dataset.ids)
  return(out)

}


#' Determine which Eurostat variables need to be found
#'
#' Takes the translated specification and extracts the Eurostat variable codes
#'
#' @param specification A tibble or data.frame as returned by
#'   \code{\link{translate_variables}}.
#' @param dictionary A tibble or data.frame storing the Eurostat variable code
#'   in column 'eurostat_code'. When \code{NULL}, the
#'   \link[=dict]{default dictionary} is used.
#'
#' @return Returns a list with one character vector:
#' \describe{
#'   \item{var.ids}{contains the Eurostat variable ids}
#' }
#'
#'

determine_eurocodes <- function(specification, dictionary = NULL) {

  if (is.null(dictionary)) {
    dictionary <- aggregate.model::dict
  }

  #codes.avail <- dictionary$eurostat_code
  codes.avail <- dictionary %>%
    tidyr::drop_na(.data$dataset_id) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(codes.avail = dplyr::case_when(!is.na(.data$nace_r2)~paste0(.data$eurostat_code,"*",.data$nace_r2),
                                                 TRUE ~ .data$eurostat_code)) %>%
    dplyr::pull(.data$codes.avail)

  # extract the codes used in the model
  dep.set <- specification$dependent_eu
  indep <- specification$independent_eu
  indep.set <- NULL
  for (i in seq_along(indep)) {

    # each element of character vector may contain formula, e.g. "A + B", must split
    vars <- strsplits(
      indep[i],
      c("\\-", "\\+"))
    indep.set <- union(indep.set, vars)

  }
  indep.set <- gsub(" ", "", indep.set)
  codes.used <- union(dep.set, indep.set)

  # the codes that cannot be found in codes.avail must be aggregate level vars (e.g. TOTS)
  # don't need to download them
  codes.eurostat <- codes.used[which(codes.used %in% codes.avail)]

  dictionary %>%
    tidyr::drop_na(.data$dataset_id) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(codes.avail = dplyr::case_when(!is.na(.data$nace_r2)~paste0(.data$eurostat_code,"*",.data$nace_r2),
                                                 TRUE ~ .data$eurostat_code)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$codes.avail %in% codes.used) %>%
    dplyr::pull(.data$model_varname) -> codes.varname

  # output list
  out <- list(var.ids = codes.eurostat, model_varname = codes.varname)
  return(out)

}


#' Determine which model variables need to be found
#'
#' Takes the specification and returns a dictionary-like data frame with the
#' required model variables, their filters, and where they can be found.

determine_variables <- function(specification, dictionary) {

  # extract the codes used in the model
  dep.set <- specification$dependent
  indep <- specification$independent
  indep.set <- NULL

  for (i in seq_along(indep)) {
    # each element of character vector may contain formula, e.g. "A + B", must split
    vars <- strsplits(
      indep[i],
      c(" \\- ", " \\+ "))
    indep.set <- union(indep.set, vars)
  }
  indep.set <- gsub(" ", "", indep.set)
  codes.used <- union(dep.set, indep.set)

  # some model variable might be aggregate level vars (e.g. TOTS)
  # they are constructed by definition, so need to exclude them
  # exclusion by is.na(database)

  to_obtain <- dictionary %>%
    dplyr::filter(model_varname %in% codes.used) %>%
    dplyr::filter(!is.na(database)) %>%
    dplyr::mutate(found = FALSE) # will keep track of whether found (download or local)

  return(to_obtain)

}







