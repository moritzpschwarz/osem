# analyse how the functions in our package are connected

if (requireNamespace("pkgnet", "igraph", "visNetwork")) {
  # install & load OSEM first
  devtools::load_all(".")

  # create automatic report
  report <- pkgnet::CreatePackageReport(pkg_name = "osem")

  # extract graph of our own functions
  g <- report$FunctionReporter$pkg_graph$igraph

  # simplify the graph: want to exclude forecast functions for now
  to_remove <- c(
    "forecast_comparison", "forecast_comparison2", "model_table",
    "print.osem.forecast.insample", "print.osem.forecast",
    "create_dictionaries", "forecast_model", "forecast_identities",
    "forecast_exogenous_values", "forecast_sensitivity",
    "forecast_setup_estimated_relationships",
    "forecast_extract_info", "nowcast_setup_estimated_relationships",
    "nowcasting", "print.osem", "run_shiny", "present_model",
    "diagnostics_model", "network", "forecast_insample", "rmsfe",
    "forecast_failure", "plot.osem", "plot.osem.forecast",
    "plot.osem.forecast.insample"
  )
  # also ignore auxiliary functions
  to_remove <- c(to_remove, "strsplits", "strsplits2", "translate_variables")
  g_pruned <- igraph::delete_vertices(g, to_remove)

  # The 'id' and 'label' columns will use the function names
  nodes_df <- data.frame(
    id = igraph::V(g_pruned)$name,
    label = igraph::V(g_pruned)$name,
    stringsAsFactors = FALSE
  )

  # edges
  edges_df <- igraph::as_data_frame(g_pruned, what = "edges")
  edges_df$arrows <- "to" # directed

  # interactive plot
  visNetwork::visNetwork(nodes = nodes_df, edges = edges_df) %>%
    visNetwork::visHierarchicalLayout() %>%
    visNetwork::visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) %>%
    visNetwork::visPhysics(enabled = FALSE)
}
