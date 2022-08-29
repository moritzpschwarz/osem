

# To check the errors

# This has a direct endogeneity
# config_table_small <- tibble(
#   dependent = c("JL",
#                 "TOTS",
#                 "B"),
#   independent = c("TOTS - CP - CO - J - A",
#                   "YF + B",
#                   "CP + J + B"))



# This has an indirect endogeneity
# config_table_small <- tibble(
#   dependent = c("JL",
#                 "TOTS",
#                 "B",
#                 "CP"),
#   independent = c("TOTS - CP - CO - J - A",
#                   "YF + B",
#                   "CP + J",
#                   "B + CO"))


# This is the correct specification
# config_table_small <- tibble(
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
#' config_table_small <- tibble(
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
    mutate(independent = gsub(" ", "", independent)) %>%
    rowwise() %>%
    mutate(splitvars = list(strsplits(
      independent,
      c("\\-", "\\+")
    ))) %>%
    ungroup() %>%
    unnest(splitvars, keep_empty = TRUE) %>% # keep_empty = TRUE to allow for AR models
    # group_by(dependent) %>%
    # rowwise() %>%
    mutate(direct_endog = case_when(dependent == splitvars~TRUE,
                                    is.na(splitvars)~FALSE,
                                    TRUE ~ FALSE)) -> direct_endog

  if (any(direct_endog$direct_endog)) {
    stop(
      "A direct endogeneity was detected in this configuration table.
    You seem to have included a variable on the LHS as well as the RHS.
    Modify the config_table"
    )
  }

  direct_endog %>%
    rowwise() %>%
    mutate(pairs = list(sort(c(dependent, splitvars)))) %>%
    ungroup() %>%
    filter(duplicated(pairs)) %>%
    pull(pairs) -> indirect_endog


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
    mutate(
      index = 1:n(), .before = 1,
      independent = gsub(" ", "", independent)
    ) %>%
    rowwise() %>%
    mutate(splitvars = list(strsplits(
      independent,
      c("\\-", "\\+")
    ))) %>%
    ungroup() %>%
    unnest(splitvars, keep_empty = TRUE) %>% # keep_empty = TRUE to allow for AR models
    mutate(endog = ifelse(splitvars %in% dependent, TRUE, FALSE)) -> config_table_endog_exog

  config_table_endog_exog %>%
    group_by(dependent, independent) %>%
    mutate(all_exog = case_when(is.na(splitvars)~FALSE,
                                !any(endog)~TRUE,
                                TRUE ~ FALSE)) %>%
    ungroup() %>%
    distinct(index, type, dependent, independent, all_exog) %>%
    filter(all_exog) %>%
    select(-all_exog) %>%
    mutate(order = ifelse(n()>0,1:n(),NA_integer_)) -> order_exog

  config_table_endog_exog %>%
    group_by(dependent, independent) %>%
    mutate(all_exog = case_when(is.na(splitvars)~FALSE,
                                !any(endog)~TRUE,
                                TRUE ~ FALSE)) %>%
    ungroup() %>%
    filter(!all_exog) -> not_exog


  while (nrow(not_exog) != 0) {
    not_exog %>%
      mutate(
        already_estimated = case_when(
          endog & (splitvars %in% order_exog$dependent) ~ TRUE,
          TRUE ~ FALSE
        ),
        endog = case_when(
          endog & already_estimated ~ FALSE,
          TRUE ~ endog
        )
      ) %>%
      group_by(index, type) %>%
      filter(!any(endog)) %>%
      distinct(index, dependent, independent) %>%
      mutate(
        order = 1:n(),
        order = order + max(order_exog$order)
      ) %>%
      ungroup() -> remove_from_not_exog

    bind_rows(remove_from_not_exog, order_exog) -> order_exog

    not_exog %>% filter(!index %in% remove_from_not_exog$index) -> not_exog
  }


  # config_table_endog_exog %>%
  #   group_by(dependent, independent) %>%
  #   mutate(all_exog = !any(endog)) %>%
  #   ungroup() %>%
  #   filter(!all_exog) %>%
  #   mutate(already_estimated = case_when(endog & (splitvars %in% order_exog$dependent)~TRUE,
  #                                        TRUE ~ FALSE),
  #          endog = case_when(endog & already_estimated ~ FALSE,
  #                            TRUE ~ endog)) %>%
  #   group_by(index) %>%
  #   filter(!any(endog)) %>%
  #   distinct(index, dependent, independent) %>%
  #   mutate(order = 1:n(),
  #          order = order + max(order_exog$order)) %>%
  #   ungroup() %>%
  #   bind_rows(order_exog)
  #

  order_exog %>%
    arrange(order) %>%
    mutate(
      independent = gsub("\\+", " + ", independent),
      independent = gsub("\\-", " - ", independent)
    ) %>%
    return()
}

#
# translation_table <- tibble(
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
