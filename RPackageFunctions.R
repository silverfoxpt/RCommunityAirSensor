# Uninstall
remove.packages("testPackage")

src <- "D:/R/RCommunityAirSensor/"
library(pkgnet)
library(usethis)
library(devtools)

# Reinstalls
devtools::install(src, upgrade = "never")

# Load and generate pkgnet graph
library(pkgnet)
report <- CreatePackageReport(
  pkg_name      = "testPackage",
  pkg_path      = ".",
  pkg_reporters = list(FunctionReporter$new())
)

fr <- report$FunctionReporter  # <-- correct accessor
stopifnot(inherits(fr, "FunctionReporter"))

# Build igraph from reporter tables
edges_df <- as.data.frame(fr$edges)[, c("SOURCE","TARGET")]
colnames(edges_df) <- c("from","to")
verts_df <- data.frame(name = as.data.frame(fr$nodes)$node, stringsAsFactors = FALSE)

library(igraph)
g <- igraph::graph_from_data_frame(edges_df, directed = TRUE, vertices = verts_df)
write_graph(g, "function_graph.dot", format = "dot")

igraph_to_drawio_csv <- function(g) {
  stopifnot(igraph::is.igraph(g))

  # --- IDs ---
  has_names <- !is.null(igraph::V(g)$name)
  ids <- if (has_names) as.character(igraph::V(g)$name) else as.character(seq_len(igraph::vcount(g)))

  # --- Labels (shown on node) ---
  labels <- igraph::V(g)$label
  name_col <- if (!is.null(labels)) as.character(labels) else ids

  # --- Refs (targets) ---
  is_dir <- igraph::is.directed(g)
  # For directed graphs, use OUT neighbors; for undirected, use all neighbors
  neigh_fun <- if (is_dir) igraph::neighbors else function(graph, v) igraph::neighbors(graph, v, mode = "all")

  refs <- vapply(seq_along(ids), function(i) {
    nbrs_idx <- neigh_fun(g, i)
    if (length(nbrs_idx) == 0) return("")
    nbr_ids <- if (has_names) as.character(igraph::V(g)$name[nbrs_idx]) else as.character(nbrs_idx)
    # Remove self and duplicates, keep stable order
    nbr_ids <- unique(nbr_ids[nbr_ids != ids[i]])
    paste(nbr_ids, collapse = ",")
  }, character(1))

  # --- Data frame in draw.io column order ---
  df <- data.frame(
    id   = ids,
    name = name_col,
    refs = refs,
    stringsAsFactors = FALSE
  )

  # --- Emit single string: your exact header + CSV rows ---
  header <- paste(
    "# label: %name%",
    "# style: whiteSpace=wrap;html=1;",
    "# connect: {\"from\":\"refs\",\"to\":\"id\",\"style\":\"endArrow=blockThin;endFill=1;\"}",
    "# identity: id",
    "# namespace: csvimport-",
    "# layout: organic",
    sep = "\n"
  )

  tc <- textConnection(NULL, "w", local = TRUE)
  on.exit(close(tc), add = TRUE)
  utils::write.table(df, file = tc, sep = ",", row.names = FALSE, col.names = TRUE, quote = TRUE)
  body <- textConnectionValue(tc)

  paste(header, paste(body, collapse = "\n"), sep = "\n")
}
txt <- igraph_to_drawio_csv(g)
cat(txt)

#------------------------------------------------------
# Generate documentation based off of Roxygen2 comments
devtools::document()

# Check package okay
devtools::check()

# List all functions
lsf.str("package:testPackage")
