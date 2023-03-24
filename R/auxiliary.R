#' unpack formulas
#'
#' NOTE: exported for now for interactive programming, later will be internal
#' @param x String to split
#' @param splits vector of elements to split the string by.
#' @param ... Further arguments.
#'

strsplits <- function(x, splits, ...) {
  for (tosplit in splits)
  {
    x <- unlist(strsplit(x, tosplit, ...))
  }
  return(x[!x == ""]) # Remove empty values
}


#' Classify variables
#'
#' Classifies variables according to their state in the model: exogenous (x),
#' endogenous by modelling (n), and endogenous by definition/identity (d).
#'
#' NOTE: exported for now, later will be internal
#' @param specification A specification to be classified.
#'

classify_variables <- function(specification) {

  dep <- specification$dependent
  indep <- specification$independent

  indep <- strsplits(indep, splits = c("\\+", "\\-"))
  indep <- gsub(" ", "", indep)

  vars.all <- union(dep, indep)

  # x are all variables that are not modelled at some point
  vars.x <- setdiff(vars.all, dep)

  # n are all variables that are in dep and have type == "n" in classification
  vars.n <- specification[specification$type == "n", ] %>% dplyr::pull(.data$dependent)

  # d are all variables that are in dep and have type == "d" in classification
  vars.d <- specification[specification$type == "d", ] %>% dplyr::pull(.data$dependent)

  # sanity check: all elements member of at least one set, no overlap between them -> partition
  stopifnot(setequal(vars.all, union(union(vars.x, vars.n), vars.d)))
  stopifnot(intersect(vars.x, vars.n) == character(0))
  stopifnot(intersect(vars.x, vars.d) == character(0))
  stopifnot(intersect(vars.n, vars.d) == character(0))

  # output
  classification <- data.frame(var = vars.all) %>%
    dplyr::mutate(class = dplyr::case_when(var %in% vars.x ~ "x",
                             var %in% vars.n ~ "n",
                             var %in% vars.d ~ "d",
                             TRUE ~ NA_character_)
    )

  return(classification)

}


#' Updates the aggregate model dataset with fitted values
#'
#' NOTE: exported for now, later will be internal
#' @param orig_data Original data that is to be updated.
#' @param new_data New dataset that will update the original data.
#'

update_data <- function(orig_data, new_data) {

  # which values to add (always add fitted level)
  add <- new_data %>%
    dplyr::select(dplyr::contains(c("time", ".level.hat")))

  # change name to make consistent with identify_module_data()
  cnames <- colnames(add)
  cur_name <- gsub("\\.level\\.hat","",cnames[cnames != "time"])
  orig_name_index <- grep(cur_name,orig_data %>%
                            dplyr::distinct(.data$na_item) %>%
                            dplyr::pull(), fixed = TRUE)

  orig_data_names <- orig_data %>%
    dplyr::distinct(.data$na_item) %>%
    dplyr::pull()
  orig_name <- orig_data_names[orig_name_index]

  cnames[cnames != "time"] <- gsub(cur_name, orig_name,cnames[cnames != "time"])
  cnames <- gsub("\\.level", "", cnames)
  #cnames <- gsub("HAT", "hat", cnames)
  colnames(add) <- cnames

  # bring original data into wide format
  orig_data_wide <- orig_data %>%
    tidyr::pivot_wider(names_from = "na_item", values_from = "values")

  # combine
  final_wide <- dplyr::full_join(x = orig_data_wide, y = add, by = "time")

  # pivot longer again b/c is how clean_data() and identify_module_data() work
  final <- tidyr::pivot_longer(final_wide, cols = !"time", names_to = "na_item", values_to = "values")

  return(final)

}






