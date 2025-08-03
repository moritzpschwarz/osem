#' Check the configuration of the model contained in the config table
#'
#' @param config_table A tibble or data.frame specifying the model. It must include the following columns:
#'  \describe{
#'    \item{type}{Character specifying whether the LHS variable is given as a definition/identity ("d") or modelled endogenously ("n")}
#'    \item{dependent}{LHS variable.}
#'    \item{independent}{RHS variables separated by +, -, /, or *.}
#'    \item{lag}{RHS variables that should only enter as lags and not contemporaneously, separated by a comma.}
#'    \item{cvar}{Unique character identifiers to group LHS variables into estimation as a cointegrated vector autoregression (CVAR).}
#'  }
#'
#' @return A tibble that appends the order of the modules to be run to the input tibble/data.frame. Variable rows from the same cvar system are collapsed to a single row.
#'
#' @examples
#' config_table_small <- dplyr::tibble(
#'   type = c("d", "d", "n"),
#'   dependent = c("JL", "TOTS", "B"),
#'   independent = c("TOTS - CP - CO - J - A", "YF + B", "CP + J"),
#'   lag = c("", "", ""),
#'   cvar = c("", "", "")
#' )
#' osem:::check_config_table(config_table_small)
#'
#' mwe <- dplyr::tibble(
#'   type = c("n", "n", "n", "n", "n", "n", "d", "n", "n"),
#'   dependent = c("X", "Y", "U", "V", "W", "M", "T", "Q", "S"),
#'   independent = c("U", "U", "", "U + W", "U + V", "Y + U", "U + V + W", "", "R"),
#'   lag = c("", "", "", "W", "", "U, Y", "", "", ""),
#'   cvar = c("system1", "system1", "", "", "", "", "", "", "")
#' )
#' osem:::check_config_table(mwe)
#'
check_config_table <- function(config_table) {
  if (!setequal(colnames(config_table), c("type", "dependent", "independent", "lag", "cvar"))) {
    stop("config_table does not contain all required columns.")
  }

  # check that CVAR system specifies same regressors
  check_cvar_dependent <- config_table %>%
    dplyr::filter(.data$cvar != "") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(indep_sets = list(sort(trimws(unlist(strsplit(.data$independent, "\\+")))))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$cvar) %>%
    dplyr::summarise(indep_sets_num = dplyr::n_distinct(.data$indep_sets)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(indep_set_equal = .data$indep_sets_num == 1L)
  if (any(check_cvar_dependent$indep_set_equal == FALSE)) {
    stop("Please specify the same independent variables within each CVAR system.")
  }

  # check that CVAR system is of type "n" and has no "lag" specified
  check_cvar_vals <- config_table %>%
    dplyr::filter(.data$cvar != "")
  if (!all(check_cvar_vals$type == "n")) {
    stop("All CVAR modules must be of type 'n'.")
  }
  if (!all(check_cvar_vals$lag == "")) {
    stop("CVAR modules cannot specify exogenous variables that enter only as lags.")
  }

  # check that for single equation modules, any "lag" vars are also part of "independent"
  check_single_lag <- config_table %>%
    dplyr::filter(.data$cvar == "") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      indep_sets = list(sort(trimws(unlist(strsplit(.data$independent, "\\+"))))),
      lag_sets = list(sort(trimws(unlist(strsplit(.data$lag, ",")))))
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      is_lag_subset_of_independent = dplyr::case_when(
        .data$lag == "" ~ TRUE,
        TRUE ~ all(.data$lag_sets %>% purrr::pluck() %in% .data$indep_sets %>% purrr::pluck())
      )
    )
  if (any(check_single_lag$is_lag_subset_of_independent == FALSE)) {
    stop("Any variable specified to enter only lagged (column 'lag') has to also be specified in the 'independent' formula.")
  }

  # internal logic:
  # estimate CVAR sub-systems separately
  # allow for CVAR to have exogenous regressors in principle (despite not in urca-pkg)
  # to check for contemporaneous simultaneity, filter out vars labelled as "lag"
  # check whether Cholesky is possible by using graph theory

  #### Part 0 - Input validation
  # add tests that the entries for "independent", "lag", "type" are the same within the same cvar-system

  #### Part 1 - check for cycles/Cholesky ordering -----------------------------

  # check contemporaneous connections between nodes
  node_edge_tbl <- config_table %>%
    # remove potential blank spaces around LHS variables
    dplyr::mutate(independent = gsub(" ", "", .data$independent)) %>%
    dplyr::rowwise() %>%
    # extract variables that only enter as lag
    dplyr::mutate(lag_vars = list(strsplits2(.data$lag, ","))) %>%
    # extract RHS variables
    dplyr::mutate(rhs_vars = list(strsplits2(.data$independent, c("\\-", "\\+", "/", "\\*")))) %>%
    # remove vars that are only added as lags
    dplyr::mutate(rhs_vars_contemp = list(setdiff(.data$rhs_vars, .data$lag_vars))) %>%
    # unnest, so have one row per LHS-RHS variable
    dplyr::ungroup() %>%
    tidyr::unnest(.data$rhs_vars_contemp, keep_empty = TRUE)
  # result: tbl where each row is an edge: LHS and contemporaneous RHS pair per row

  # extract all nodes/variables
  # want to show all nodes and later determine order for all vars including AR models
  all_vars <- union(node_edge_tbl$dependent, unlist(node_edge_tbl$rhs_vars))
  # store whether variable is part of a CVAR system
  node_tbl <- tidyr::tibble(name = all_vars) %>%
    dplyr::mutate(exog = !(.data$name %in% config_table$dependent)) %>%
    dplyr::left_join(
      config_table %>%
        dplyr::select(name = .data$dependent, .data$cvar),
      by = "name"
    ) %>%
    # will be NA if it is only a RHS var (purely exogenous)
    # turn the non-subsystem vars with cvar == "" into NA too
    dplyr::mutate(cvar = dplyr::na_if(.data$cvar, ""))

  # focus on nodes that have incoming edges (ignore pure AR or if only lagged RHS vars)
  edge_tbl <- node_edge_tbl %>%
    tidyr::drop_na(.data$rhs_vars_contemp) # drop nodes without incoming edges

  # create graph from edges
  g_full <- edge_tbl %>%
    dplyr::mutate(
      from = .data$rhs_vars_contemp,
      to = .data$dependent
    ) %>%
    dplyr::select(.data$from, .data$to) %>%
    igraph::graph_from_data_frame(d = ., directed = TRUE, vertices = node_tbl)

  # check that there are no cycles -> is it a directed, acyclical graph?
  if (!igraph::is_dag(g_full)) {
    # wait for "simple_cycles()" function from igraph to tell user where cycle exists
    stop("Contemporaneous simultaneity detected. Model cannot be identified with Cholesky ordering.")
  }

  # plot the graph (if pkgs in Suggests: are installed)
  if (requireNamespace("tidygraph") & requireNamespace("ggraph") & requireNamespace("ggforce")) {
    g_tbl <- tidygraph::as_tbl_graph(g_full) %>%
      dplyr::mutate(node_type = ifelse(.data$exog, "exog", "endog"))
    ggraph::ggraph(g_tbl, layout = "sugiyama") +
      ggraph::geom_node_circle(ggplot2::aes(r = 0.2, fill = .data$node_type), colour = "black") +
      ggraph::geom_node_text(ggplot2::aes(label = .data$name), size = 3) +
      ggraph::geom_edge_link(
        arrow = ggplot2::arrow(
          type = "closed",
          length = ggplot2::unit(4, "pt")
        ),
        end_cap = ggraph::circle(0.8, "cm"),
        start_cap = ggraph::circle(0.8, "cm"),
        alpha = 0.6
      ) +
      ggforce::geom_mark_hull(ggplot2::aes(x = .data$x, y = .data$y, filter = !is.na(.data$cvar), fill = .data$cvar),
        concavity = 5, linetype = "dashed"
      )
  } else {
    message("Skipping plot of model configuration - install tidygraph, ggraph, ggforce.")
  }

  #### Part 2 - determine a possible Cholesky ordering -------------------------

  # collapse sub-system CVAR nodes into a single node
  # igraph needs a numeric vector of length = #nodes, same numbers get contracted
  # step 1: vector of names: dependent if is.na(cvar), otherwise the cvar value
  subsystems <- ifelse(is.na(igraph::V(g_full)$cvar), igraph::V(g_full)$name, igraph::V(g_full)$cvar)
  # step 2: make numeric
  subsystems <- as.integer(factor(subsystems))
  # step 3: store original member name as attribute
  igraph::V(g_full)$members <- igraph::V(g_full)$name
  # step 4: contract the nodes of the same sub-system
  g_sub <- igraph::contract(g_full, subsystems, vertex.attr.comb = list(
    name = "first",
    cvar = "first",
    exog = "first",
    # keep original member list
    members = function(x) paste(x, collapse = ",")
  )) %>%
    igraph::simplify(remove.multiple = TRUE, remove.loops = FALSE) # remove multiple vertices from/into the cvar sub-systems
  # step 5: rename subsystem node
  igraph::V(g_sub)$name <- ifelse(is.na(igraph::V(g_sub)$cvar), igraph::V(g_sub)$name, igraph::V(g_sub)$cvar)

  # double check but this should not have changed whether the model is a DAG
  stopifnot(igraph::is_dag(g_sub))

  # determine order
  # topological sorting of a DAG: linear ordering of nodes s.t. each node comes before all nodes to which it has edges (sort by outgoing edges)
  # caution: ordering may not be unique! I think this matters for the interpretation of the shock order of Cholesky ordering
  ordering <- igraph::topo_sort(g_sub, mode = "out")
  # remove purely exogenous variables (they are not a row in the config_table)
  ordering_modelled <- ordering[!igraph::V(g_sub)$exog[ordering]]
  # expand the CVAR sub-systems into their members again
  members <- igraph::V(g_sub)$members[ordering_modelled] # elements of CVAR are separated by comma
  members <- strsplit(members, ",")
  # create the numeric ordering vector: increasing ordering, multiple same number for CVAR system
  ordering_numeric <- rep(seq_along(members), times = lengths(members))
  # create lookup table
  ordering_lookup <- setNames(ordering_numeric, unlist(members))
  # add ordering to config_table
  config_table %>%
    dplyr::mutate(order = ordering_lookup[.data$dependent]) %>%
    dplyr::arrange(.data$order) %>%
    # code below can be taken out if want to keep separate rows for cvar system variables
    dplyr::group_by(.data$order) %>%
    dplyr::summarise(
      type = dplyr::first(.data$type),
      dependent = if (dplyr::first(.data$cvar) == "") {
        dplyr::first(.data$dependent)
      } else {
        paste(.data$dependent, collapse = ",")
      },
      independent = dplyr::first(.data$independent),
      lag = dplyr::first(.data$lag),
      cvar = dplyr::first(.data$cvar),
      order = dplyr::first(.data$order)
    ) %>%
    dplyr::ungroup() %>%
    # this is from old code (wasn't explained why we need this)
    dplyr::mutate(
      independent = gsub("\\+", " + ", .data$independent),
      independent = gsub("\\-", " - ", .data$independent),
      independent = gsub("/", " / ", .data$independent),
      independent = gsub("\\*", " * ", .data$independent)
    ) %>%
    # old code created a column "index", which is used by other functions
    # my understanding (Jonas) is that this was essentially random, so simply
    # add another column called index that mirrors the order column
    dplyr::mutate(index = .data$order) %>%
    return()
}
