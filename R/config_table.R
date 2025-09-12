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
#' @param quiet Logical whether should plot the DAG.
#'
#' @return A tibble that appends the order of the modules to be run to the input tibble/data.frame. Variable rows from the same cvar system are collapsed to a single row.
#'
#' @details
#' For backwards compatibility, we still allow for specifications table that
#' only specify "type", "dependent", and "independent" columns. In these cases,
#' we add empty columns "lag" and "cvar".
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
check_config_table <- function(config_table, quiet = TRUE) {
  if (!setequal(colnames(config_table), c("type", "dependent", "independent", "lag", "cvar"))) {
    # for backwards compatibility, allow subset of columns, then add empty "lag" and "cvar" columns
    if (setequal(colnames(config_table), c("type", "dependent", "independent"))) {
      config_table <- config_table %>%
        dplyr::mutate(
          lag = "",
          cvar = ""
        )
    } else {
      stop("config_table does not contain all required columns.")
    }
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
    dplyr::ungroup()
  # result: tbl where each row is a module, RHS vars are still in list
  # tidyr::unnest("rhs_vars_contemp", keep_empty = TRUE)

  # extract all nodes/variables
  # want to show all nodes and later determine order for all vars including AR models
  all_vars <- union(node_edge_tbl$dependent, unlist(node_edge_tbl$rhs_vars))
  # store whether variable is part of a CVAR system
  node_tbl <- tidyr::tibble(name = all_vars) %>%
    dplyr::mutate(exog = !(.data$name %in% config_table$dependent)) %>%
    dplyr::left_join(
      config_table %>%
        dplyr::select(name = "dependent", "cvar"),
      by = "name"
    ) %>%
    # will be NA if it is only a RHS var (purely exogenous)
    # turn the non-subsystem vars with cvar == "" into NA too
    dplyr::mutate(cvar = dplyr::na_if(.data$cvar, ""))

  # for Cholesky, focus on contemporaneous edges
  # also, focus on nodes that have incoming edges (ignore pure AR or if only lagged RHS vars)
  edge_tbl_contemp <- node_edge_tbl %>%
    tidyr::unnest("rhs_vars_contemp", keep_empty = TRUE) %>%
    tidyr::drop_na("rhs_vars_contemp") # drop nodes without incoming edges

  # create graph from edges
  contemp_edges <- edge_tbl_contemp %>%
    dplyr::mutate(
      from = .data$rhs_vars_contemp,
      to = .data$dependent
    ) %>%
    dplyr::select("from", "to")
  g_contemp <- igraph::graph_from_data_frame(d = contemp_edges, directed = TRUE, vertices = node_tbl)

  # check that there are no (contemporaneous) cycles -> is it a directed, acyclical graph?
  if (!igraph::is_dag(g_contemp)) {
    # wait for "simple_cycles()" function from igraph to tell user where cycle exists
    stop("Contemporaneous simultaneity detected. Model cannot be identified with Cholesky ordering.")
  }

  if (!quiet) {
    # plot the graph (if pkgs in Suggests: are installed)
    if (requireNamespace("tidygraph", quietly = TRUE) & requireNamespace("ggraph", quietly = TRUE) & requireNamespace("ggforce", quietly = TRUE)) {
      g_tbl <- tidygraph::as_tbl_graph(g_contemp) %>%
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
  }

  #### Part 2 - build full dependency graph
  edge_tbl_full <- node_edge_tbl %>%
    tidyr::unnest("rhs_vars", keep_empty = TRUE) %>%
    tidyr::drop_na("rhs_vars") # drop nodes without incoming edges

  # create graph from edges
  g_full <- edge_tbl_full %>%
    dplyr::mutate(
      from = .data$rhs_vars,
      to = .data$dependent
    ) %>%
    dplyr::select("from", "to") %>%
    igraph::graph_from_data_frame(d = ., directed = TRUE, vertices = node_tbl)

  #### Part 3 - identify "cycles" in lags using strongly connected components (SCC)
  scc_blocks <- igraph::components(g_full, mode = "strong")$membership

  #### Part 4 - identify all CVAR systems
  cvar_blocks <- igraph::V(g_full)$cvar # may be NA, create new name then for those
  cvar_blocks[is.na(cvar_blocks)] <- paste0("single_", igraph::V(g_full)$name[is.na(cvar_blocks)])

  #### Part 5 - identify joint blocks
  # vars should be in same block if are in same CVAR system or same SCC system (or both)
  block_df <- dplyr::tibble(
    name = igraph::V(g_full)$name,
    scc_group = scc_blocks,
    cvar_group = cvar_blocks
  )
  # create all pairs within SCC groups
  scc_pairs <- block_df %>%
    dplyr::select("name", "scc_group") %>%
    dplyr::inner_join(., ., by = "scc_group", relationship = "many-to-many") %>%
    dplyr::filter(.data$name.x < .data$name.y) %>%
    dplyr::select(from = .data$name.x, to = .data$name.y)
  # create all pairs within CVAR groups
  cvar_pairs <- block_df %>%
    dplyr::select("name", "cvar_group") %>%
    dplyr::inner_join(., ., by = "cvar_group", relationship = "many-to-many") %>%
    dplyr::filter(.data$name.x < .data$name.y) %>%
    dplyr::select(from = .data$name.x, to = .data$name.y)
  # combine get the unique set
  block_relations <- dplyr::bind_rows(scc_pairs, cvar_pairs) %>%
    dplyr::distinct()
  # create graph
  g_blocks <- igraph::graph_from_data_frame(block_relations, directed = FALSE, vertices = node_tbl)
  final_grouping <- igraph::components(g_blocks, mode = "weak")$membership

  #### Part 6 - determine order of blocks
  # store original member name as attribute
  igraph::V(g_full)$members <- igraph::V(g_full)$name
  # contract nodes of same block
  g_components <- igraph::contract(g_full, final_grouping, vertex.attr.comb = list(
    name = function(x) paste(sort(x), collapse = ","),
    cvar = "first",
    exog = "first"
  )) %>%
    igraph::simplify(remove.multiple = TRUE, remove.loops = TRUE)
  # determine order
  # topological sorting of a DAG: linear ordering of blocks s.t. each block comes before all blocks to which it has edges (sort by outgoing edges)
  # caution: ordering may not be unique! I think this matters for the interpretation of the shock order of Cholesky ordering
  ordering <- igraph::topo_sort(g_components, mode = "out")

  # the blocks include simultaneous systems but between blocks should be a linear ordering
  stopifnot(igraph::is_dag(g_components))

  #### Part 7 - create final ordering and output table

  # remove purely exogenous variables (they are not a row in the config_table)
  ordering_modelled <- ordering[!igraph::V(g_components)$exog[ordering]]
  # expand the blocks into their members again
  members <- igraph::V(g_components)$name[ordering_modelled] # elements of blocks are separated by comma
  members <- strsplit(members, ",")
  # create the numeric ordering vector: increasing ordering, multiple same number for CVAR system
  ordering_numeric <- rep(seq_along(members), times = lengths(members))
  # create lookup table
  ordering_lookup <- setNames(ordering_numeric, unlist(members))
  # add ordering to config_table based on block ordering
  final_table <- config_table %>%
    dplyr::mutate(order = ordering_lookup[.data$dependent]) %>%
    dplyr::arrange(.data$order)
  # issue: within a block, we still need to determine the sub-ordering
  final_table_with_sub_order <- final_table %>%
    # for each group
    dplyr::group_by(order) %>%
    # combine each order-group into a single row using a tibble
    tidyr::nest() %>%
    # iterate through each row/tibble (for single modules this tibble has only one row, otherwise more)
    dplyr::mutate(data_with_sub_order = purrr::map(.data$data, function(block_df) {
      # function to operate on the tibbles
      # extract the variables contained in this block
      block_vars <- block_df$dependent
      if (nrow(block_df) == 1) { # if only one row, sub-order is trivially 1 & can end
        return(block_df %>% dplyr::mutate(sub_order = 1))
      }
      # if more than one variable in this block, need to determine order of estimation
      # retain any contemporaneous edges between members of a block
      internal_edges <- contemp_edges %>%
        dplyr::filter(.data$from %in% block_vars, .data$to %in% block_vars)
      # create graph from edges between members of a block
      internal_graph <- igraph::graph_from_data_frame(internal_edges, directed = TRUE, vertices = block_vars)
      # now need to determine order within the block
      # set up zero empty vector, name it, copy internal graph
      sub_orders <- rep(0, length(block_vars))
      names(sub_orders) <- block_vars
      g_temp <- internal_graph
      current_level <- 1
      while (igraph::vcount(g_temp) > 0) { # while still nodes/vars in this block
        # determine all nodes with no internal incoming edges, they can be done first
        # NOTE: cvar systems don't show up as contemporaneous edges (equ. for X does not specify Y as indep. & vice versa)
        level_nodes <- igraph::V(g_temp)$name[igraph::degree(g_temp, mode = "in") == 0]
        if (length(level_nodes) == 0) stop("Internal cycle detected. Should not occur.")
        sub_orders[level_nodes] <- current_level # give them current level (starting with 1)
        g_temp <- igraph::delete_vertices(g_temp, level_nodes) # delete those variables from the graph
        current_level <- current_level + 1 # increase order number for next iteration
      }
      block_df %>%
        dplyr::mutate(sub_order = sub_orders[.data$dependent]) # add sub_order vector as new column to tibble
    })) %>%
    # convert to normal tibble using the new tibbles
    tidyr::unnest(cols = "data_with_sub_order") %>%
    dplyr::select(-c("data")) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$order, .data$sub_order) %>%
    dplyr::rename(block_order = .data$order) %>%
    dplyr::relocate("block_order", .before = "sub_order") %>%
    dplyr::mutate(order = 1:dplyr::n()) %>%
    dplyr::relocate("order")
  # I think cvar systems should always have the same sub_order number
  cvar_sub_order_check <- final_table_with_sub_order %>%
    dplyr::filter(.data$cvar != "" & !is.na(.data$cvar)) %>%
    dplyr::group_by(.data$cvar) %>%
    dplyr::summarise(sub_order_num = dplyr::n_distinct(.data$sub_order)) %>%
    dplyr::pull("sub_order_num")
  if (any(cvar_sub_order_check > 1)) {
    stop("Let the developers know about this. This should not happen.")
  }
  # contract cvar system as a single row again b/c estimation has to be jointly
  # cannot simply group by cvar yet because all that are not cvar vars have "" or NA
  out <- final_table_with_sub_order %>%
    dplyr::mutate(
      contraction = dplyr::if_else(!is.na(.data$cvar) & .data$cvar != "", .data$cvar, .data$dependent)
    ) %>%
    dplyr::group_by(.data$contraction) %>%
    dplyr::summarise(
      type = dplyr::first(.data$type),
      dependent = paste(sort(.data$dependent), collapse = ","),
      independent = dplyr::first(.data$independent),
      lag = dplyr::first(.data$lag),
      cvar = dplyr::first(.data$cvar),
      block_order = dplyr::first(.data$block_order),
      sub_order = dplyr::first(.data$sub_order)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"contraction") %>%
    # sort now that have combined cvar systems
    dplyr::arrange(.data$block_order, .data$sub_order) %>%
    dplyr::mutate(order = 1:dplyr::n()) %>%
    dplyr::relocate("order")
  # final clean-up
  out %>%
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
