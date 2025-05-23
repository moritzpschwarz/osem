

# To check the errors

# This has a direct endogeneity
# config_table_small <- dplyr::tibble(
#   type = c("d",
#            "d",
#            "n"),
#   dependent = c("JL",
#                 "TOTS",
#                 "B"),
#   independent = c("TOTS - CP - CO - J - A",
#                   "YF + B",
#                   "CP + J + B"))



# This has an indirect endogeneity
# config_table_small <- dplyr::tibble(
#   type = c("d",
#            "d",
#            "n"),
#   dependent = c("JL",
#                 "TOTS",
#                 "B",
#                 "CP"),
#   independent = c("TOTS - CP - CO - J - A",
#                   "YF + B",
#                   "CP + J",
#                   "B + CO"))


# This is the correct specification
# config_table_small <- dplyr::tibble(
#   dependent = c("JL",
#                 "TOTS",
#                 "B"),
#   independent = c("TOTS - CP - CO - J - A",
#                   "YF + B",
#                   "CP + J"))
#






#' Check the configuration of the model contained in the config table
#'
#' @param config_table A tibble or data.frame with one column named 'dependent', containing the LHS (Y variables) and one named 'independent' containing the RHS (x variables separated by + and - ).
#'
#' @return A tibble that gives the order of the modules to be run.
#'
#' @examples
#' config_table_small <- dplyr::tibble(
#'   type = c("d","d","n"),
#'   dependent = c("JL", "TOTS", "B"),
#'   independent = c("TOTS - CP - CO - J - A", "YF + B", "CP + J")
#' )
#' osem:::check_config_table(config_table_small)
#'
check_config_table <- function(config_table) {

  # Define a strsplits function that is used below
  strsplits <- function(x, splits, ...) {
    if (identical(x, "")) {
      return(NULL)
    } else {
      for (split in splits)
      {
        x <- unlist(strsplit(x, split, ...))
      }
      return(x[!x == ""]) # Remove empty values
    }
  }

  # check that there is no direct endogeneity (a RHS variable appears on the LHS in the same equation)
  config_table %>%
    dplyr::mutate(independent = gsub(" ", "", .data$independent)) %>%
    dplyr::rowwise() %>%
    # use the splitvars function to separate the specification table into individual pieces in a list
    dplyr::mutate(splitvars = list(strsplits(
      .data$independent,
      c("\\-", "\\+", "/", "\\*")
    ))) %>%
    dplyr::ungroup() %>%

    # create a separate row for each element
    tidyr::unnest("splitvars", keep_empty = TRUE) %>% # keep_empty = TRUE to allow for AR models
    # dplyr::group_by(dependent) %>%
    # dplyr::rowwise() %>%
    dplyr::mutate(direct_endog = dplyr::case_when(.data$dependent == .data$splitvars ~ TRUE,
                                                  is.na(.data$splitvars) ~ FALSE,
                                                  TRUE ~ FALSE)) -> direct_endog

  if (any(direct_endog$direct_endog)) {
    stop(
      "A direct endogeneity was detected in this configuration table.
    You seem to have included a variable on the LHS as well as the RHS.
    Modify the config_table"
    )
  }

  # check if there are any indirect endogeneities (i.e. a variable can only depend on another variable if that variable does not depend on the other)
  direct_endog %>%
    dplyr::rowwise() %>%
    dplyr::mutate(pairs = list(sort(c(.data$dependent, .data$splitvars)))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(duplicated(.data$pairs)) %>%
    dplyr::pull(.data$pairs) -> indirect_endog

  # if an indirect engodeneity is detected, print a detailed statement.
  if (!identical(indirect_endog, list())) {
    overall <- vector()
    for (i in 1:length(indirect_endog)) {
      intermed <- paste0(indirect_endog[[i]], collapse = " and ")
      overall <- c(overall, intermed)
    }
    stop(
      paste0(
        "An endogeneity was detected.\nCheck the relationship between ",
        paste0(overall, collapse = "; "),
        "\nIn the OSEM model a variable can only depend on another variable if that variable does not depend on the other."
      )
    )
  }

  # create the right order for estimation
  config_table %>%
    dplyr::mutate(
      index = 1:dplyr::n(), .before = 1,
      independent = gsub(" ", "", .data$independent)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(splitvars = list(strsplits(
      .data$independent,
      c("\\-", "\\+", "/", "\\*")
    ))) %>%
    dplyr::ungroup() %>%

    tidyr::unnest("splitvars", keep_empty = TRUE) %>% # keep_empty = TRUE to allow for AR models
    # set endog = TRUE when one an x variable appears anywhere in the "dependent" column - this tells us whether an x-variable is ever estimated
    dplyr::mutate(endog = ifelse(.data$splitvars %in% .data$dependent, TRUE, FALSE)) -> config_table_endog_exog


  # determine those relationships where all x-variables are exogenous - those can be estimated first
  config_table_endog_exog %>%
    dplyr::group_by(.data$dependent, .data$independent) %>%
    dplyr::mutate(all_exog = dplyr::case_when(is.na(.data$splitvars) ~ FALSE, # this is the case for AR - means none are exog
                                              !any(.data$endog) ~ TRUE, # if not all are endogenous there must be at least one exog
                                              TRUE ~ FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dplyr::across(c("index", "type", "dependent", "independent", "all_exog"))) %>%
    dplyr::filter(.data$all_exog) %>%
    dplyr::select(-"all_exog") %>%
    #dplyr::mutate(order = if_else(condition = (dplyr::n()>0),true = (1:dplyr::n()),false = NA_integer_)) -> order_exog
    {if (nrow(.) > 0) {dplyr::mutate(.,order = 1:dplyr::n())} else {dplyr::mutate(., order = NA_integer_)}} -> order_exog

  # get the table of relationships where not all x-variables are exogenous - this is just the opposite of above
  config_table_endog_exog %>%

    dplyr::group_by(.data$dependent, .data$independent) %>%
    dplyr::mutate(all_exog = dplyr::case_when(is.na(.data$splitvars) ~ FALSE, # this is the case for AR - means none are exog
                                              !any(.data$endog) ~ TRUE, # if not all are endogenous there must be at least one exog
                                              TRUE ~ FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!.data$all_exog) -> not_exog

  # in the following part, we are reducing the not_exog table down part by part
  while (nrow(not_exog) != 0) {
    not_exog %>%

      # if a relationship is estimated elsewhere (endog = TRUE) and the x-variable is estimated previously then set already_estimated = TRUE
      dplyr::mutate(
        already_estimated = dplyr::case_when(
          .data$endog & (.data$splitvars %in% order_exog$dependent) ~ TRUE,
          TRUE ~ FALSE)) %>%

      # if a variable was earlier an endogeneous variable but has already been estimated, then we can set endog = FALSE
      dplyr::mutate(endog = dplyr::case_when(
          .data$endog & .data$already_estimated ~ FALSE,
          TRUE ~ .data$endog)) %>%

      dplyr::group_by(.data$index, .data$type) %>%
      # get all relationships where no endogenous terms are left
      dplyr::filter(!any(.data$endog)) %>%
      dplyr::distinct(dplyr::across(c("index", "dependent", "independent"))) %>%

      dplyr::ungroup() %>%

      # give those relationships an order number - these should now be run after all those that are already in the order_exog tibble
      dplyr::mutate(
        order = 1:dplyr::n(),
        order = order + if (is.null(order_exog$order) | identical(integer(0),order_exog$order)) {0} else {max(order_exog$order)}
      ) -> remove_from_not_exog

    dplyr::bind_rows(remove_from_not_exog, order_exog) -> order_exog

    not_exog %>% dplyr::filter(!.data$index %in% remove_from_not_exog$index) -> not_exog
  }


  # config_table_endog_exog %>%
  #   dplyr::group_by(dependent, independent) %>%
  #   dplyr::mutate(all_exog = !any(endog)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::filter(!all_exog) %>%
  #   dplyr::mutate(already_estimated = dplyr::case_when(endog & (splitvars %in% order_exog$dependent)~TRUE,
  #                                        TRUE ~ FALSE),
  #          endog = dplyr::case_when(endog & already_estimated ~ FALSE,
  #                            TRUE ~ endog)) %>%
  #   dplyr::group_by(index) %>%
  #   dplyr::filter(!any(endog)) %>%
  #   dplyr::distinct(index, dependent, independent) %>%
  #   dplyr::mutate(order = 1:dplyr::n(),
  #          order = order + max(order_exog$order)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::bind_rows(order_exog)
  #

  order_exog %>%
    dplyr::arrange(.data$order) %>%
    dplyr::mutate(
      independent = gsub("\\+", " + ", .data$independent),
      independent = gsub("\\-", " - ", .data$independent),
      independent = gsub("/", " / ", .data$independent),
      independent = gsub("\\*", " * ", .data$independent)
    ) %>%
    return()
}

#
# translation_table <- dplyr::tibble(
#   eurostat = c("B1GQ", "B1G", "P6", "P7", "P5G",
#                "P3", "P3_S13", "P31_S13", "P32_S13", "P31_S14_S15", "P31_S14",
#                "P31_S15", "YA1", "YA0", "YA2"),
#   aggregate_variable = c("Y",
#                          "YF", "A", "B", "J", "CP + CO", "CO", NA, NA, "CP", NA, NA, "JL",
#                          "JL", "JL"),
#   full_name = c("Gross domestic product at market prices",
#                 "Value added, gross", "Exports of goods and services", "Imports of goods and services",
#                 "Gross capital formation", "Final consumption expenditure", "Final consumption expenditure of general government",
#                 "Individual consumption expenditure of general government", "Collective consumption expenditure of general government",
#                 "Household and NPISH final consumption expenditure", "Final consumption expenditure of households",
#                 "Final consumption expenditure of NPISH", "Statistical discrepancy (production approach)",
#                 "Statistical discrepancy (expenditure approach)", "Statistical discrepancy (income approach)")
# )
#
# translate_config_table_eurostat <- function(config_table){
#
#
#
# }
