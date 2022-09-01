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
#' @export
#'

determine_datacodes <- function(specification, dictionary = NULL) {

  if (is.null(dictionary)) {
    dictionary <- aggregate.model::dict
  }

  codes.avail <- dictionary$eurostat_code
  # delete irrelevant values, such as TOTS
  codes.avail <- codes.avail[!is.na(codes.avail)]

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

  # match the Eurostat variable codes to the Eurostat dataset id
  codes <- data.frame(eurostat_code = codes.eurostat)
  codes <- left_join(x = codes, y = dictionary %>% select(eurostat_code, dataset_id), by = "eurostat_code")

  # which datasets need to be downloaded?
  dataset.ids <- unique(codes$dataset_id)

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
#' @export
#'

determine_eurocodes <- function(specification, dictionary = NULL) {

  if (is.null(dictionary)) {
    dictionary <- aggregate.model::dict
  }

  codes.avail <- dictionary$eurostat_code

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

  # output list
  out <- list(var.ids = codes.eurostat)
  return(out)

}
