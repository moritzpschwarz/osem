#' Visualize dependence between modules
#'
#' @param model An aggregate model of class 'aggmod'
#'
#' @return Returns a network graph illustrating the dependence between the
#'   different modules.
#'
#' @export

network <- function(model) {

  # dependency check because only listed as "Suggests"
  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("Package 'ggraph' required for network visualization.")
  }
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop("Package 'tidygraph' required for network visualization.")
  }

  # stores the dependencies between the modules
  dependency <- model$module_collection

  # classification of the variables
  class <- classify_variables(dependency)

  # create the union of all variables used in the aggregate model
  dep_raw <- dependency$dependent
  indep_raw <- dependency$independent
  indep <- strsplits(indep_raw, splits = c("\\+", "\\-"))
  indep <- gsub(" ", "", indep)
  vars <- union(dep_raw, indep)

  # set up adjacency matrix
  adj <- matrix(data = 0, nrow = length(vars), ncol = length(vars))
  colnames(adj) <- rownames(adj) <- vars

  # populate the adjacency matrix with the edges
  # row = from = regressor, col = to = regressand
  for (i in 1:NROW(dependency)) {
    to <- dep_raw[i]
    from <- indep_raw[i]
    if (from == "") { # means that is only AR
      from <- to
    }
    from <- strsplits(from, splits = c("\\+", "\\-"))
    from <- gsub(" ", "", from)
    stopifnot(length(to) == 1L)
    adj[from, to] <- 1
  }

  # now check whether is actually used
  models <- dependency$model
  names(models) <- dependency$dependent
  models <- models[!sapply(models,is.null)]
  for (i in 1:length(models)) {
    # since variables may appear in transformed form (ln, lag), need to search for it
    yvar <- names(models)[i]
    potential <- rownames(adj)[adj[, colnames(adj) == yvar] == 1]
    retained <- rownames(models[i][[1]]$mean.results)
    for (k in 1:length(potential)) {
      # check whether was retained
      found <- sum(grepl(pattern = potential[k], x = retained)) >= 1
      if (!found) {
        adj[potential[k], yvar] <- 2 # if not found, replace 1 by 2 (linetype)
      }
    }
  }

  graph_df <- tidygraph::as_tbl_graph(adj)
  graph_df <- graph_df %>%
    tidygraph::activate(nodes) %>%
    dplyr::inner_join(y = class, by = c("name" = "var"))

    out <- ggraph::ggraph(graph_df, layout = "kk") +
      ggraph::geom_node_point(aes(color = class), size = 3) +
      ggraph::geom_edge_link(aes(edge_linetype = weight),
                             arrow = arrow(length = unit(2, 'mm')),
                             end_cap = ggraph::circle(4, 'mm')) +
      ggraph::geom_edge_loop(aes(edge_linetype = weight, end_cap = ggraph::circle(1, 'mm'), span = 120, direction = -45),
                             arrow = arrow(length = unit(2, 'mm')),
                             position = "jitter") +
      ggraph::geom_node_text(aes(label = name), repel = TRUE, size = 3) +
      scale_color_discrete(name = "Type of Variable",
                           labels = c("definition/identity", "endogenous", "exogenous")) +
      theme(legend.position = "bottom")

  return(out)

}
