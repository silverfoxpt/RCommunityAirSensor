# install.packages(c("tibble","dplyr","purrr","stringr","readr","rlang"))
library(tibble)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(rlang)

# Helper: parse a single @concept line payload into key/value(s)
.parse_concept <- function(x) {
  # Trim leading/trailing spaces
  x <- str_trim(x)
  # If empty, ignore
  if (x == "") return(list())
  # Split on whitespace but keep chunks with embedded ':' intact
  parts <- str_split(x, "\\s+")[[1]]
  # For items like "role:download" or "flag:true"
  kvs <- map(parts, function(tok) {
    if (str_detect(tok, ":")) {
      kv <- str_split_fixed(tok, ":", 2)
      key <- kv[,1]
      val <- kv[,2]
      # normalize boolean strings to logical
      if (str_to_lower(val) %in% c("true","false")) {
        val <- as.logical(str_to_lower(val))
      }
      set_names(list(val), key)
    } else {
      # plain tag like "@concept experimental" -> experimental = TRUE
      set_names(list(TRUE), tok)
    }
  })
  # merge into a single named list (later entries win on duplicate keys)
  reduce(kvs, ~modifyList(.x, .y))
}

# Helper: extract roxygen blocks and the function name they annotate
.extract_blocks_with_fun <- function(file) {
  lines <- read_lines(file)
  n <- length(lines)
  i <- 1L
  out <- list()
  while (i <= n) {
    # find start of a roxygen block
    if (str_starts(lines[i], "#'")) {
      block <- character()
      while (i <= n && str_starts(lines[i], "#'")) {
        block <- c(block, lines[i])
        i <- i + 1L
      }
      # Now find the *next* non-empty, non-comment line and try to get the function name
      j <- i
      while (j <= n && (str_trim(lines[j]) == "" || str_starts(str_trim(lines[j]), "#"))) {
        j <- j + 1L
      }
      fun <- NA_character_
      if (j <= n) {
        # Match "name <- function(" including S3/S4-style names like "print.foo <- function("
        m <- str_match(lines[j], "^\\s*([A-Za-z][A-Za-z0-9_.]*)\\s*<-\\s*function\\s*\\(")
        if (!is.na(m[1,2])) fun <- m[1,2]
        # Also handle namespaced assignment "pkg:::name <- function(" (rare in packages, but safe)
        if (is.na(fun)) {
          m2 <- str_match(lines[j], "^\\s*(?:[A-Za-z][A-Za-z0-9_.]*:::+)?([A-Za-z][A-Za-z0-9_.]*)\\s*<-\\s*function\\s*\\(")
          if (!is.na(m2[1,2])) fun <- m2[1,2]
        }
      }
      out <- append(out, list(list(fun = fun, block = block, file = file)))
    } else {
      i <- i + 1L
    }
  }
  out
}

# Main: scan package_path/R/*.R and build tibble of concepts
concept_table <- function(package_path = ".") {
  rdir <- file.path(package_path, "R")
  if (!dir.exists(rdir)) {
    abort(glue::glue("No 'R/' directory under {normalizePath(package_path)}"))
  }
  files <- list.files(rdir, pattern = "\\.[rR]$", full.names = TRUE, recursive = TRUE)
  blocks <- map(files, .extract_blocks_with_fun) |> list_flatten()

  # Parse @concept lines per block
  parsed <- map(blocks, function(b) {
    # strip "#'" and leading spaces
    block_clean <- str_remove(b$block, "^#'\\s?")
    concept_lines <- block_clean[str_starts(block_clean, "@concept")]
    if (length(concept_lines) == 0) return(NULL)

    # Remove the "@concept" token and parse payload
    payloads <- str_remove(concept_lines, "^@concept\\s+")
    kv <- reduce(map(payloads, .parse_concept), ~modifyList(.x, .y), .init = list())
    list(fun = b$fun %||% NA_character_, file = b$file, concepts = kv)
  }) |> compact()

  if (length(parsed) == 0) {
    return(tibble(function_name = character(), file = character()))
  }

  # Discover all concept keys across the package
  all_keys <- parsed |>
    map(~names(.x$concepts)) |>
    unlist(use.names = FALSE) |>
    unique() |>
    sort()

  # Row-bind into a tibble, aligning keys
  rows <- map(parsed, function(p) {
    vals <- p$concepts
    # ensure every known key exists
    for (k in setdiff(all_keys, names(vals))) vals[[k]] <- NA
    tibble(
      function_name = p$fun,
      file = p$file,
      !!!vals
    )
  }) |>
    list_rbind()

  # Make logical columns actual logical where possible
  rows <- rows |>
    mutate(across(where(is.character), ~ifelse(. %in% c("TRUE","FALSE"), as.logical(.), .)))
  rows
}
tbl <- concept_table(".")
write.csv(tbl, file = "status_table.csv", row.names = FALSE)
