## Combine input data functions ----
## Helper function to sort data based on the given criteria
#' Sort combined monitor information
#'
#' Sorts a combined sites information table according to project-specific
#' priorities (ShortCode MNR/SYD first, Reference and Co-located grouping,
#' Park vs Non-park, then device type).
#'
#' @param data A data.frame or tibble containing at least the columns
#'   `ShortCode`, `Subtype`, and `Type`.
#' @return The input `data` arranged according to the sorting logic.
#' @examples
#' \dontrun{
#' sort_combined_info(my_sites_tbl)
#' }
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
sort_combined_info <- function(data) {
  data %>%
    dplyr::arrange(
      # First priority: MNR and SYD first
      factor(ShortCode, levels = c("MNR", "SYD"), ordered = TRUE),

      # Within MNR and SYD: Reference first, then Co-located
      dplyr::case_when(
        ShortCode %in% c("MNR", "SYD") & Subtype == "Reference" ~ 1,
        ShortCode %in% c("MNR", "SYD") & Subtype == "Co-located" ~ 2,
        TRUE ~ 3
      ),

      # Second priority: Park > Non-park
      dplyr::case_when(
        Subtype == "Park" ~ 1,
        Subtype == "Non-park" ~ 2,
        TRUE ~ 3
      ),

      # Third priority: Group by ShortCode
      ShortCode,

      # Fourth priority: Clarity > PurpleAir
      factor(Type, levels = c("Clarity", "PurpleAir"), ordered = TRUE)
    )
}

# Generalized function to combine and sort data
##' Combine lists of site data and sort by site info
##'
#' Compact helper to combine multiple lists/tibbles and return a list
#' with Day, Hour and Info elements ordered by device id.
#'
#' @param day_list A list of daily data lists (each element a named list of tibbles).
#' @param hour_list A list of hourly data lists.
#' @param info_list A list of site information tibbles to row-bind and sort.
#' @return A list with entries `Day`, `Hour`, and `Info`.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
combine_and_sort <- function(day_list, hour_list, info_list) {
  combined_day <- do.call(c, day_list)
  combined_hour <- do.call(c, hour_list)
  combined_info <- dplyr::bind_rows(info_list)

  sorted_info <- sort_combined_info(combined_info)

  list(
    Day = combined_day[sorted_info$DeviceID],
    Hour = combined_hour[sorted_info$DeviceID],
    Info = sorted_info
  )
}

##' Duplicate and combine data groups
##'
#' Create combined data groupings (with and without PurpleAir/reference
#' stations) used by analysis reports. This function preserves the original
#' behaviour but centralises the combination logic.
#'
#' @param dataCompFile A list-like object that contains `Day`, `Hour`,
#'   `PurpleAirDay`, `PurpleAirHour`, `DayReference`, `HourReference`,
#'   `Info`, `PurpleAirInfo`, `ReferenceInfo` elements.
#' @return The modified `dataCompFile` list with additional combined entries.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
duplicate_data <- function(dataCompFile) {
  dataCompFile[c("CombinedDay", "CombinedHour", "CombinedInfo")] <-
    combine_and_sort(
      list(dataCompFile$Day, dataCompFile$PurpleAirDay, dataCompFile$DayReference),
      list(dataCompFile$Hour, dataCompFile$PurpleAirHour, dataCompFile$HourReference),
      list(dataCompFile$Info, dataCompFile$PurpleAirInfo, dataCompFile$ReferenceInfo)
    )

  dataCompFile[c("CombinedDayNoRef", "CombinedHourNoRef", "CombinedInfoNoRef")] <-
    combine_and_sort(
      list(dataCompFile$Day, dataCompFile$PurpleAirDay),
      list(dataCompFile$Hour, dataCompFile$PurpleAirHour),
      list(dataCompFile$Info, dataCompFile$PurpleAirInfo)
    )

  dataCompFile[c("CombinedDayNoPPA", "CombinedHourNoPPA", "CombinedInfoNoPPA")] <-
    combine_and_sort(
      list(dataCompFile$Day, dataCompFile$DayReference),
      list(dataCompFile$Hour, dataCompFile$HourReference),
      list(dataCompFile$Info, dataCompFile$ReferenceInfo)
    )

  dataCompFile[c("CombinedDayNoPPARef", "CombinedHourNoPPARef", "CombinedInfoNoPPARef")] <-
    combine_and_sort(
      list(dataCompFile$Day),
      list(dataCompFile$Hour),
      list(dataCompFile$Info)
    )

  # Filtered subsets for the new groupings
  info_ref_coloc <- dataCompFile$CombinedInfo %>%
    dplyr::filter(Subtype %in% c("Reference", "Co-located")) %>%
    dplyr::pull(DeviceID)
  info_park <- dataCompFile$CombinedInfo %>%
    dplyr::filter(Subtype == "Park") %>%
    dplyr::pull(DeviceID)
  info_nonpark <- dataCompFile$CombinedInfo %>%
    dplyr::filter(Subtype == "Non-park") %>%
    dplyr::pull(DeviceID)

  dataCompFile[c("CombinedDayRefCo", "CombinedHourRefCo", "CombinedInfoRefCo")] <-
    combine_and_sort(
      list(dataCompFile$Day[info_ref_coloc], dataCompFile$PurpleAirDay[info_ref_coloc], dataCompFile$DayReference[info_ref_coloc]),
      list(dataCompFile$Hour[info_ref_coloc], dataCompFile$PurpleAirHour[info_ref_coloc], dataCompFile$HourReference[info_ref_coloc]),
      list(dplyr::filter(dataCompFile$Info, Subtype %in% c("Reference", "Co-located")),
           dplyr::filter(dataCompFile$PurpleAirInfo, Subtype %in% c("Reference", "Co-located")),
           dplyr::filter(dataCompFile$ReferenceInfo, Subtype %in% c("Reference", "Co-located"))
      )
    )

  dataCompFile[c("CombinedDayPark", "CombinedHourPark", "CombinedInfoPark")] <-
    combine_and_sort(
      list(dataCompFile$Day[info_park], dataCompFile$PurpleAirDay[info_park]),
      list(dataCompFile$Hour[info_park], dataCompFile$PurpleAirHour[info_park]),
      list(dplyr::filter(dataCompFile$Info, Subtype %in% c("Park")),
           dplyr::filter(dataCompFile$PurpleAirInfo, Subtype %in% c("Park"))
      )
    )

  dataCompFile[c("CombinedDayNonPark", "CombinedHourNonPark", "CombinedInfoNonPark")] <-
    combine_and_sort(
      list(dataCompFile$Day[info_nonpark], dataCompFile$PurpleAirDay[info_nonpark]),
      list(dataCompFile$Hour[info_nonpark], dataCompFile$PurpleAirHour[info_nonpark]),
      list(dplyr::filter(dataCompFile$Info, Subtype %in% c("Non-park")),
           dplyr::filter(dataCompFile$PurpleAirInfo, Subtype %in% c("Non-park"))
      )
    )

  # Data without PurpleAir
  dataCompFile[c("CombinedDayRefCoNoPA", "CombinedHourRefCoNoPA", "CombinedInfoRefCoNoPA")] <-
    combine_and_sort(
      list(dataCompFile$Day[info_ref_coloc], dataCompFile$DayReference[info_ref_coloc]),
      list(dataCompFile$Hour[info_ref_coloc], dataCompFile$HourReference[info_ref_coloc]),
      list(dplyr::filter(dataCompFile$Info, Subtype %in% c("Reference", "Co-located")),
           dplyr::filter(dataCompFile$ReferenceInfo, Subtype %in% c("Reference", "Co-located"))
      )
    )

  dataCompFile[c("CombinedDayParkNoPA", "CombinedHourParkNoPA", "CombinedInfoParkNoPA")] <-
    combine_and_sort(
      list(dataCompFile$Day[info_park]),
      list(dataCompFile$Hour[info_park]),
      list(dplyr::filter(dataCompFile$Info, Subtype %in% c("Park"))
      )
    )

  dataCompFile[c("CombinedDayNonParkNoPA", "CombinedHourNonParkNoPA", "CombinedInfoNonParkNoPA")] <-
    combine_and_sort(
      list(dataCompFile$Day[info_nonpark]),
      list(dataCompFile$Hour[info_nonpark]),
      list(dplyr::filter(dataCompFile$Info, Subtype %in% c("Non-park"))
      )
    )

  return(dataCompFile)
}

## Support functions ----
##' Fit a flextable to a target page width
##'
#' Applies `flextable` autofit and adjusts column widths to a target page
#' width. This is a small convenience wrapper used by report generation.
#'
#' @param ft A flextable object.
#' @param pgwidth Numeric page width in inches. Default `6.5`.
#' @return A flextable object modified for page width.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
FitFlextableToPage <- function(ft, pgwidth = 6.5) {
  ft_out <- flextable::autofit(ft)
  # Calculate proportional widths using flextable helper
  fw <- flextable::flextable_dim(ft_out)$widths
  ft_out <- flextable::width(ft_out, width = dim(ft_out)$widths * pgwidth / fw)
  ft_out <- flextable::paginate(ft_out, init = FALSE, hdr_ftr = TRUE)
  return(ft_out)
}

##' Check non-empty string
##'
#' Small utility to validate that a character value is not NULL/NA/empty.
#'
#' @param x Character scalar.
#' @return Logical scalar.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
CheckNotEmptyString <- function(x) {
  !is.null(x) && !is.na(x) && x != "" && length(x) > 0
}

##' Annotation values for scatterplots
##'
#' Return plotting annotation values (labels and numeric thresholds) for
#' common pollutants used in plots.
#'
#' @param type Character: one of "PM25" or "NO2".
#' @return A named list with `hourlyLabel`, `annualLabel`, `hourlyValue`, and `annualValue`.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
GetAnnotateInfoForScatterplot <- function(type) {
  if (type == "PM25") {
    return(list(
      hourlyLabel = expression("Hourly standard (35 \u00b5g/m"^{3}*")"),
      annualLabel = expression("Annual standard (9 \u00b5g/m"^{3}*")"),
      hourlyValue = 35.0,
      annualValue = 9.0
    ))
  } else if (type == "NO2") {
    return(list(
      hourlyLabel = expression("`Hourly standard (100 ppb)`"),
      annualLabel = expression("`Annual standard (53 ppb)`"),
      hourlyValue = 100.0,
      annualValue = 53.0
    ))
  }
  message("GetAnnotateInfoForScatterplot: unknown type '", type, "'")
  return(NULL)
}

##' Print a list of ggplot objects two-per-row
##'
#' Helper that prints a list of ggplot/patched plots two per row using
#' `patchwork::plot_layout`. Accepts NULL placeholders in the list.
#'
#' @param myGraph A list of ggplot/patchwork objects.
#' @return NULL (called for side effects - prints plots).
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
PrintGraphsTwoColumns <- function(myGraph) {
  i <- 1  # Initialize the iterator
  while (i <= length(myGraph)) {
    graph1 <- myGraph[[i]]
    graph2 <- if (i + 1 <= length(myGraph)) myGraph[[i + 1]] else NULL

    # Check if both graphs are NULL; if so, skip to the next graph
    if (is.null(graph1) && is.null(graph2)) {
      i <- i + 2
      next
    }

    # Combine plots based on which graphs are NULL
    if (!is.null(graph1) && !is.null(graph2)) {
      combined_plot <- graph1 +
        (graph2 + ggplot2::theme(axis.title.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())) +
        patchwork::plot_layout(ncol = 2, nrow = 1)
      i <- i + 2
    } else if (!is.null(graph1)) {
      j <- i + 2
      foundGraph <- FALSE
      while (j <= length(myGraph)) {
        if (!is.null(myGraph[[j]])) {
          foundGraph <- TRUE
          break
        }
        j <- j + 1
      }
      if (foundGraph) {
        combined_plot <- graph1 +
          (myGraph[[j]] + ggplot2::theme(axis.title.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())) +
          patchwork::plot_layout(ncol = 2, nrow = 1)
      } else {
        combined_plot <- graph1 + patchwork::plot_layout(ncol = 2, nrow = 1)
      }
      i <- j + 1
    } else {
      i <- i + 1
      next
    }

    print(combined_plot)
  }
}

## Descriptive statistics functions ----
##' Generate a simple boxplot (deprecated)
##'
#' Deprecated convenience wrapper that assembles a boxplot from multiple
#' data sources. Left for backward compatibility.
#'
#' @param datafiles A named list of data frames.
#' @param columnName Column name (string) to plot.
#' @param renameName Y-axis label.
#' @param newLabel Plot title.
#' @param dataInfo Monitor information table used for labels.
#' @return A `ggplot` object.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
generate_box_plot_data <- function(datafiles, columnName, renameName, newLabel, dataInfo) {
  dataTb <- purrr::map_dfr(
    .x = datafiles,
    .f = function(x) {
      x %>% dplyr::select(datasourceId, !!rlang::sym(columnName)) %>%
        dplyr::rename(y_value = !!rlang::sym(columnName))
    }
  )

  label_map <- setNames(dataInfo$ShortCode, dataInfo$DeviceID)

  plot <- ggplot2::ggplot(dataTb, mapping = ggplot2::aes(x = datasourceId, y = y_value)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "Short Code", y = renameName, color = "Legend", title = newLabel) +
    ggplot2::ylim(0, 60) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
                   legend.position = c(0.8, 0.82),
                   legend.background = ggplot2::element_rect(fill = "white", color = "black", size = 0.5),
                   legend.key.height = grid::unit(0.35, 'cm')) +
    ggplot2::scale_x_discrete(labels = label_map)
  return(plot)
}

##' Generate paired boxplots
##'
#' Create a double-boxplot for two variables across multiple data sources.
#' @param datafiles Named list of data frames.
#' @param columnName1 First column name (string).
#' @param columnName2 Second column name (string).
#' @param renameName Y-axis label.
#' @param newLabel Plot title.
#' @param dataInfo Monitor metadata table.
#' @param renameLegend1 Legend label for first variable.
#' @param renameLegend2 Legend label for second variable.
#' @return A `ggplot` object.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
generate_double_box_plot <- function(datafiles, columnName1, columnName2, renameName, newLabel, dataInfo, renameLegend1, renameLegend2) {
  dataTb <- purrr::map_dfr(
    .x = datafiles,
    .f = function(x) {
      x %>% dplyr::select(datasourceId, !!rlang::sym(columnName1), !!rlang::sym(columnName2))
    }
  )

  dataTb <- tidyr::pivot_longer(
    dataTb,
    cols = c(!!rlang::sym(columnName1), !!rlang::sym(columnName2)),
    names_to = "variable",
    values_to = "y_value"
  )

  dataInfo <- dataInfo %>%
    dplyr::mutate(BoxLabel = paste(DeviceID, "\n", ShortCode, sep = ""),
                  BoxLabel = ifelse(Type == "Reference", paste0(BoxLabel, " (ref)"), BoxLabel))

  label_map <- setNames(dataInfo$BoxLabel, dataInfo$DeviceID)
  variable_labels <- c(renameLegend1, renameLegend2)

  dataTb$datasourceId <- factor(dataTb$datasourceId, levels = unique(dataTb$datasourceId))

  plot <- ggplot2::ggplot(dataTb, mapping = ggplot2::aes(x = datasourceId, y = y_value, fill = variable)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "Short Code", y = renameName, color = "Legend", title = newLabel) +
    ggplot2::ylim(0, 60) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 55, vjust = 0.5),
                   legend.position = c(0.8, 0.9),
                   legend.background = ggplot2::element_rect(fill = "white", color = "black", size = 0.5),
                   legend.key.height = grid::unit(0.35, 'cm')) +
    ggplot2::scale_fill_discrete(labels = variable_labels, na.translate = FALSE) +
    ggplot2::scale_x_discrete(labels = label_map)
  return(plot)
}

##' Generate descriptive statistics table
##'
#' Produce a `flextable` summary table for a list of data sources.
#'
#' @param datafiles Named list of data frames.
#' @param columnName Column name (string) to summarize.
#' @param renameName Title/label for the statistic column.
#' @param monitorInfo Monitor metadata table with `DeviceID`, `ShortCode`, and `Type`.
#' @param title Optional title used in table header (not currently shown).
#' @return A `flextable` object.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
generate_descriptive_stats_table <- function(datafiles, columnName, renameName, monitorInfo, title = "Sample Title - Please fill") {
  datafiles <- purrr::map(
    .x = datafiles,
    .f = function(x) { return(dplyr::filter(x, !!rlang::sym(columnName) <= 500)) }
  )

  result <- data.frame(
    names(datafiles),
    purrr::map_int(datafiles, function(x) nrow(x)),
    purrr::map_dbl(datafiles, function(x) x %>% dplyr::pull(!!rlang::sym(columnName)) %>% na.omit() %>% min()),
    purrr::map_dbl(datafiles, function(x) x %>% dplyr::pull(!!rlang::sym(columnName)) %>% na.omit() %>% quantile(probs = 0.25)),
    purrr::map_dbl(datafiles, function(x) x %>% dplyr::pull(!!rlang::sym(columnName)) %>% na.omit() %>% quantile(probs = 0.50)),
    purrr::map_dbl(datafiles, function(x) x %>% dplyr::pull(!!rlang::sym(columnName)) %>% na.omit() %>% quantile(probs = 0.75)),
    purrr::map_dbl(datafiles, function(x) x %>% dplyr::pull(!!rlang::sym(columnName)) %>% na.omit() %>% max())
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.double), ~ round(., 2)))

  colnames(result) <- c("Datasource", "Count", "Minimum", "25th Quantile", "Median", "75th Quantile", "Maximum")

  monitorInfo <- monitorInfo %>% dplyr::select(DeviceID, ShortCode, Type)
  result <- dplyr::full_join(result, monitorInfo, by = c("Datasource" = "DeviceID")) %>%
    dplyr::rename(Site = ShortCode) %>%
    dplyr::relocate(Site, .after = Datasource) %>%
    dplyr::relocate(Type, .after = Site)

  light_red_palette <- grDevices::colorRampPalette(c("#ffd3d3", "#FAA0A0", "#FF5733"))(100)
  light_blue_palette <- grDevices::colorRampPalette(c("#E4F6F8", "lightblue", "#4B92DB"))(100)
  light_yellow_palette <- grDevices::colorRampPalette(c("#fff2cc", "#ffe599", "#ffd966"))(100)

  median_domain <- c(min(result$Median, na.rm = TRUE), max(result$Median, na.rm = TRUE))
  minimum_domain <- c(min(result$Minimum[is.finite(result$Minimum)], na.rm = TRUE),
                      max(result$Minimum[is.finite(result$Minimum)], na.rm = TRUE))
  maximum_domain <- c(min(result$Maximum[is.finite(result$Maximum)], na.rm = TRUE),
                      max(result$Maximum[is.finite(result$Maximum)], na.rm = TRUE))

  tb <- flextable::flextable(result) %>%
    FitFlextableToPage() %>%
    flextable::width(j = "Type", width = 0.6) %>%
    flextable::set_table_properties(opts_pdf = list(tabcolsep = 4)) %>%
    flextable::bg(j = "Median", bg = scales::col_numeric(light_blue_palette, domain = median_domain)(result$Median)) %>%
    flextable::bg(j = "Minimum", bg = scales::col_numeric(light_yellow_palette, domain = minimum_domain)(result$Minimum)) %>%
    flextable::bg(j = "Maximum", bg = scales::col_numeric(light_red_palette, domain = maximum_domain)(result$Maximum))

  tb
}

# Trend graphs functions ####
##' Generate combined scatterplot for a month
##'
#' Create a time-series scatterplot combining two columns (typically raw
#' and calibrated values) across a one-month range. The function will
#' normalize the `startOfPeriod` column, expand the x-axis to include the
#' entire month (based on `start_date`), and annotate with pollutant
#' standards based on `type`.
#'
#' @param data A data.frame or tibble containing a `startOfPeriod` column and the
#'   measured variables named by `columnName1` and `columnName2`.
#' @param columnName1 String name of the first column (e.g. raw values).
#' @param columnName2 String name of the second column (e.g. calibrated values).
#' @param renameName Y-axis label string.
#' @param newLabel Plot title string.
#' @param type Character; one of `"PM25"` or `"NO2"` used to choose annotations.
#' @param start_date Optional Date or coercible value indicating the month
#'   start (defaults to `params$myData$Date` when available).
#' @return A `ggplot` object showing daily points for the month.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
generate_scatter_plot_data <- function(data, columnName1, columnName2, renameName, newLabel, type = "PM25", start_date = NULL) {
  data <- data %>%
    dplyr::mutate(!!rlang::sym(columnName1) := as.double(!!rlang::sym(columnName1))) %>%
    dplyr::mutate(!!rlang::sym(columnName2) := as.double(!!rlang::sym(columnName2))) %>%
    tidyr::pivot_longer(cols = c(!!rlang::sym(columnName1), !!rlang::sym(columnName2)),
                        names_to = "variable",
                        values_to = "y_value") %>%
    dplyr::mutate(y_value = as.double(y_value)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(startOfPeriod = as.POSIXct(startOfPeriod, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") %>%
                    format("%Y-%m-%d", tz = "Etc/GMT+5") %>%
                    as.Date()) %>%
    dplyr::ungroup()

  variable_labels <- setNames(c("Raw Values", "Calibrated Values"), c(columnName1, columnName2))

  # Determine start and end dates: prefer explicit param, fall back to params$myData if present.
  if (is.null(start_date)) {
    if (exists("params", envir = .GlobalEnv)) {
      prm <- get("params", envir = .GlobalEnv)
      if (!is.null(prm$myData$Date)) {
        startDate <- lubridate::as_date(prm$myData$Date)
      } else {
        stop("start_date must be provided when params$myData$Date is not available")
      }
    } else {
      stop("start_date must be provided when params is not present in the global environment")
    }
  } else {
    startDate <- lubridate::as_date(start_date)
  }
  endDate <- (startDate + months(1)) - days(1)

  tmpDf <- data.frame(startOfPeriod = seq.Date(startDate, endDate, by = "day"))
  data <- dplyr::full_join(tmpDf, data, by = "startOfPeriod")

  annotateInfo <- GetAnnotateInfoForScatterplot(type)

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = startOfPeriod, y = y_value, color = variable)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = annotateInfo$hourlyValue), color = "orange") +
    ggplot2::annotate("text", x = min(data$startOfPeriod, na.rm = TRUE),
                      y = annotateInfo$hourlyValue,
                      label = annotateInfo$hourlyLabel,
                      vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = annotateInfo$annualValue), color = "orange") +
    ggplot2::annotate("text", x = min(data$startOfPeriod, na.rm = TRUE),
                      y = annotateInfo$annualValue,
                      label = annotateInfo$annualLabel,
                      vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) +
    ggplot2::geom_point() +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "Date", y = renameName, color = "Legend", title = newLabel) +
    ggplot2::ylim(0, 60) +
    ggplot2::scale_color_discrete(labels = variable_labels, na.translate = FALSE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
                   legend.position = c(0.8, 0.82),
                   legend.background = ggplot2::element_rect(fill = "white", color = "black", size = 0.5),
                   legend.key.height = grid::unit(0.35, 'cm')) +
    ggplot2::scale_x_date(breaks = seq.Date(startDate, endDate, by = "5 days"), date_labels = "%m / %d")

  return(plot)
}

##' Generate diurnal (hourly) boxplots for a month
##'
#' Produce boxplots grouped by hour-of-day (0-23) for the provided
#' observations. Time values in `startOfPeriod` are converted to the
#' project's timezone and grouped by hour. Useful for visualizing diurnal
#' patterns across devices or data sources.
#'
#' @param data A data.frame or tibble containing `startOfPeriod` and the
#'   variable column named by `columnName`.
#' @param columnName String name of the column to plot.
#' @param renameName Y-axis label string.
#' @param newLabel Plot title string.
#' @param type Character; one of `"PM25"` or `"NO2"` used to choose annotations.
#' @return A `ggplot` object showing hourly boxplots for the available data.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
generate_diurnal_boxplot_data <- function(data, columnName, renameName, newLabel, type = "PM25") {
  lev <- as.character(0:23)
  data <- data %>%
    dplyr::mutate(!!rlang::sym(columnName) := as.double(!!rlang::sym(columnName))) %>%
    dplyr::mutate(y_value = !!rlang::sym(columnName))

  # Convert the original data's startOfPeriod column to POSIXct with the same timezone
  data <- data %>%
    dplyr::mutate(startOfPeriod = as.POSIXct(startOfPeriod, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") %>%
                    lubridate::with_tz(tzone = "Etc/GMT+5"))

  # Group data by hours in day
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(hourTime = factor(lubridate::hour(startOfPeriod), levels = 0:23, labels = lev)) %>%
    dplyr::ungroup()

  annotateInfo <- GetAnnotateInfoForScatterplot(type)

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = hourTime, y = y_value)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = annotateInfo$annualValue), color = "orange") +
    ggplot2::annotate("text", x = "0", y = annotateInfo$annualValue, label = annotateInfo$annualLabel,
                      vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = annotateInfo$hourlyValue), color = "orange") +
    ggplot2::annotate("text", x = "0", y = annotateInfo$hourlyValue, label = annotateInfo$hourlyLabel,
                      vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) +
    ggplot2::geom_boxplot(size = 0.5) +
    ggplot2::stat_summary(fun = mean, geom = "line", ggplot2::aes(group = 1), color = "blue", size = 0.5) +
    ggplot2::labs(x = "Time of Day (UTC-05:00)", y = renameName, color = "Legend", title = newLabel) +
    ggplot2::ylim(0, 60) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)) +
    ggplot2::scale_x_discrete(limits = lev, drop = FALSE, labels = lev)

  return(plot)
}

##' Generate weekly (day-of-week) boxplots for a month
##'
#' Create boxplots grouped by day-of-week (Mon..Sun) covering the
#' specified month. If `start_date` is not provided the function will
#' attempt to read the month start from `params$myData$Date` in the Rmd
#' rendering environment.
#'
#' @param data A data.frame or tibble containing `startOfPeriod` and the
#'   variable column named by `columnName`.
#' @param columnName String name of the column to plot.
#' @param renameName Y-axis label string.
#' @param newLabel Plot title string.
#' @param type Character; one of `"PM25"` or `"NO2"` used to choose annotations.
#' @param start_date Optional Date or coercible value indicating the month start.
#' @return A `ggplot` object showing day-of-week boxplots for the month.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
generate_weekly_boxplot_data <- function(data, columnName, renameName, newLabel, type = "PM25", start_date = NULL) {
  lev = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  data <- data %>%
    dplyr::mutate(!!rlang::sym(columnName) := as.double(!!rlang::sym(columnName))) %>%
    dplyr::mutate(y_value = !!rlang::sym(columnName)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(startOfPeriod = as.POSIXct(startOfPeriod, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") %>%
                    format("%Y-%m-%d", tz = "Etc/GMT+5") %>%
                    as.Date()) %>%
    dplyr::ungroup()

  # Define the full date range for the month
  if (is.null(start_date)) {
    if (exists("params", envir = .GlobalEnv)) {
      prm <- get("params", envir = .GlobalEnv)
      if (!is.null(prm$myData$Date)) {
        startDate <- lubridate::as_date(prm$myData$Date)
      } else stop("start_date required when params$myData$Date is not available")
    } else stop("start_date required when params is not present in the global environment")
  } else {
    startDate <- lubridate::as_date(start_date)
  }
  endDate <- (startDate + months(1)) - days(1)
  tmpDf <- data.frame(startOfPeriod = seq.Date(startDate, endDate, by = "day"))
  data <- dplyr::full_join(tmpDf, data, by = "startOfPeriod")

  # Group data by days in week
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dayTime = factor(lubridate::wday(startOfPeriod, abbr = T, label = T), levels = lev)) %>%
    dplyr::ungroup()

  annotateInfo <- GetAnnotateInfoForScatterplot(type)

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = dayTime, y = y_value)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = annotateInfo$annualValue), color = "orange") +
    ggplot2::annotate("text", x = "Mon", y = annotateInfo$annualValue, label = annotateInfo$annualLabel,
                      vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = annotateInfo$hourlyValue), color = "orange") +
    ggplot2::annotate("text", x = "Mon", y = annotateInfo$hourlyValue, label = annotateInfo$hourlyLabel,
                      vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) +
    ggplot2::geom_boxplot(size = 0.5) +
    ggplot2::stat_summary(fun = mean, geom = "line", ggplot2::aes(group = 1), color = "blue", size = 0.5) +
    ggplot2::labs(x = "Day of Week", y = renameName, color = "Legend", title = newLabel) +
    ggplot2::ylim(0, 60) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)) +
    ggplot2::scale_x_discrete(labels = lev)

  return(plot)
}

##' Generate site-comparison scatterplot for a ShortCode group
##'
#' Build a multi-source scatterplot comparing devices belonging to a
#' particular `ShortCode` (e.g. project region). The function filters
#' `infoData` by `shortToFilter`, binds matching `data` entries, fills
#' missing dates over the month, and returns a plot or `NULL` when only a
#' single datasource is present.
#'
#' @param data A named list of tibbles (indexed by DeviceID) or similar
#'   collection where each element contains a `startOfPeriod` and measurement column.
#' @param infoData A tibble/data.frame of device metadata with `DeviceID` and `ShortCode`.
#' @param shortToFilter ShortCode value used to select the group.
#' @param columnName String name of the measurement column.
#' @param renameName Y-axis label string.
#' @param newLabel Plot title string.
#' @param type Character; one of `"PM25"` or `"NO2"` used to choose annotations.
#' @param start_date Optional Date or coercible value indicating the month start.
#' @return A `ggplot` object or `NULL` if there are not enough distinct datasources.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
generate_sitebase_scatter_plot_data <- function(data, infoData, shortToFilter, columnName, renameName, newLabel, type = "PM25", start_date = NULL) {
    sitesInfo <- infoData %>%
      dplyr::filter(ShortCode == shortToFilter) %>%
      dplyr::pull(DeviceID)

    sitesData <- data[sitesInfo] %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(y_value = !!rlang::sym(columnName))

    if (unique(sitesData %>% dplyr::pull(datasourceId)) %>% length() <= 1) {
      return(NULL);
    }

    sitesData <- sitesData %>%
      dplyr::mutate(Label = dplyr::if_else(grepl("^R", sourceId),
                                    paste(datasourceId, " (ref)", sep = ""),
                                    datasourceId
      )
      )

    # Define the full date range for the month and normalize dates
    sitesData <- sitesData %>%
      dplyr::rowwise() %>%
      dplyr::mutate(startOfPeriod = as.POSIXct(startOfPeriod, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") %>%
                      format("%Y-%m-%d", tz = "Etc/GMT+5") %>%
                      as.Date()) %>%
      dplyr::ungroup()

    if (is.null(start_date)) {
      if (exists("params", envir = .GlobalEnv)) {
        prm <- get("params", envir = .GlobalEnv)
        if (!is.null(prm$myData$Date)) {
          startDate <- lubridate::as_date(prm$myData$Date)
        } else stop("start_date required when params$myData$Date is not available")
      } else stop("start_date required when params is not present in the global environment")
    } else {
      startDate <- lubridate::as_date(start_date)
    }
    endDate <- (startDate + months(1)) - days(1)
    tmpDf <- data.frame(startOfPeriod = seq.Date(startDate, endDate, by = "day"))
    sitesData <- dplyr::full_join(tmpDf, sitesData, by = "startOfPeriod")

    annotateInfo <- GetAnnotateInfoForScatterplot(type)

    plot <- ggplot2::ggplot(sitesData, ggplot2::aes(x = startOfPeriod, y = y_value, color = Label)) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = annotateInfo$annualValue), color = "orange") +
      ggplot2::annotate("text", x = min(sitesData$startOfPeriod, na.rm = TRUE), y = annotateInfo$annualValue, label = annotateInfo$annualLabel,
                        vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = annotateInfo$hourlyValue), color = "orange") +
      ggplot2::annotate("text", x = min(sitesData$startOfPeriod, na.rm = TRUE), y = annotateInfo$hourlyValue, label = annotateInfo$hourlyLabel,
                        vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Date", y = renameName, color = "Legend", title = newLabel) +
      ggplot2::ylim(0, 60) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
                     legend.position = c(0.8, 0.85),
                     legend.background = ggplot2::element_rect(fill = "white", color = "black", size = 0.5),
                     legend.key.height = grid::unit(0.35, 'cm')) +
      ggplot2::scale_color_discrete(na.translate = FALSE) +
      ggplot2::scale_x_date(breaks = seq.Date(startDate, endDate, by = "5 days"), date_labels = "%m / %d")

    return(plot)
  }
