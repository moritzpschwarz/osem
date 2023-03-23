

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
#' @export
#'
#' @examples
#' config_table_small <- dplyr::tibble(
#'   type = c("d","d","n"),
#'   dependent = c("JL", "TOTS", "B"),
#'   independent = c("TOTS - CP - CO - J - A", "YF + B", "CP + J")
#' )
#' check_config_table(config_table_small)
#'
check_config_table <- function(config_table) {

  strsplits <- function(x, splits, ...) {
    for (split in splits)
    {
      x <- unlist(strsplit(x, split, ...))
    }
    return(x[!x == ""]) # Remove empty values
  }

  # check that there is no endogeneity
  config_table %>%
    dplyr::mutate(independent = gsub(" ", "", .data$independent)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(splitvars = list(strsplits(
      .data$independent,
      c("\\-", "\\+")
    ))) %>%
    dplyr::ungroup() %>%
    tidyr::unnest("splitvars", keep_empty = TRUE) %>% # keep_empty = TRUE to allow for AR models
    # dplyr::group_by(dependent) %>%
    # dplyr::rowwise() %>%
    dplyr::mutate(direct_endog = dplyr::case_when(.data$dependent == .data$splitvars~TRUE,
                                                  is.na(.data$splitvars)~FALSE,
                                                  TRUE ~ FALSE)) -> direct_endog

  if (any(direct_endog$direct_endog)) {
    stop(
      "A direct endogeneity was detected in this configuration table.
    You seem to have included a variable on the LHS as well as the RHS.
    Modify the config_table"
    )
  }

  direct_endog %>%
    dplyr::rowwise() %>%
    dplyr::mutate(pairs = list(sort(c(.data$dependent, .data$splitvars)))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(duplicated(.data$pairs)) %>%
    dplyr::pull(.data$pairs) -> indirect_endog


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
        "\nIn the aggregate model a variable can only depend on another variable if that variable does not depend on the other."
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
      c("\\-", "\\+")
    ))) %>%
    dplyr::ungroup() %>%
    tidyr::unnest("splitvars", keep_empty = TRUE) %>% # keep_empty = TRUE to allow for AR models
    dplyr::mutate(endog = ifelse(.data$splitvars %in% .data$dependent, TRUE, FALSE)) -> config_table_endog_exog

  config_table_endog_exog %>%
    dplyr::group_by(.data$dependent, .data$independent) %>%
    dplyr::mutate(all_exog = dplyr::case_when(is.na(.data$splitvars)~FALSE,
                                              !any(.data$endog)~TRUE,
                                              TRUE ~ FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dplyr::across(c("index", "type", "dependent", "independent", "all_exog"))) %>%
    dplyr::filter(.data$all_exog) %>%
    dplyr::select(-"all_exog") %>%
    #dplyr::mutate(order = if_else(condition = (dplyr::n()>0),true = (1:dplyr::n()),false = NA_integer_)) -> order_exog
    {if (nrow(.) > 0) {dplyr::mutate(.,order = 1:dplyr::n())} else {dplyr::mutate(., order = NA_integer_)}} -> order_exog

  config_table_endog_exog %>%
    dplyr::group_by(.data$dependent, .data$independent) %>%
    dplyr::mutate(all_exog = dplyr::case_when(is.na(.data$splitvars)~FALSE,
                                              !any(.data$endog)~TRUE,
                                              TRUE ~ FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!.data$all_exog) -> not_exog

  while (nrow(not_exog) != 0) {
    not_exog %>%
      dplyr::mutate(
        already_estimated = dplyr::case_when(
          .data$endog & (.data$splitvars %in% order_exog$dependent) ~ TRUE,
          TRUE ~ FALSE
        ),
        endog = dplyr::case_when(
          .data$endog & .data$already_estimated ~ FALSE,
          TRUE ~ .data$endog
        )
      ) %>%
      dplyr::group_by(.data$index, .data$type) %>%
      dplyr::filter(!any(.data$endog)) %>%
      dplyr::distinct(dplyr::across(c("index", "dependent", "independent"))) %>%
      dplyr::ungroup() %>%
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
      independent = gsub("\\-", " - ", .data$independent)
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
