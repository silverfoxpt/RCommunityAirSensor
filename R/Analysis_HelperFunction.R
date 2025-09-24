# Combine input data functions ####
# Helper function to sort data based on the given criteria
sort_combined_info <- function(data) {
  data %>%
    arrange(
      # First priority: MNR and SYD first
      factor(ShortCode, levels = c("MNR", "SYD"), ordered = TRUE),
      
      # Within MNR and SYD: Reference first, then Co-located
      case_when(
        ShortCode %in% c("MNR", "SYD") & Subtype == "Reference" ~ 1,
        ShortCode %in% c("MNR", "SYD") & Subtype == "Co-located" ~ 2,
        TRUE ~ 3
      ),
      
      # Second priority: Park > Non-park
      case_when(
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
combine_and_sort <- function(day_list, hour_list, info_list) {
  combined_day <- do.call(c, day_list)
  combined_hour <- do.call(c, hour_list)
  combined_info <- bind_rows(info_list)
  
  sorted_info <- sort_combined_info(combined_info)
  
  list(
    Day = combined_day[sorted_info$DeviceID],
    Hour = combined_hour[sorted_info$DeviceID],
    Info = sorted_info
  )
}

# Main function
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
    filter(Subtype %in% c("Reference", "Co-located")) %>%
    pull(DeviceID)
  info_park <- dataCompFile$CombinedInfo %>% 
    filter(Subtype == "Park") %>%
    pull(DeviceID)
  info_nonpark <- dataCompFile$CombinedInfo %>% 
    filter(Subtype == "Non-park") %>%
    pull(DeviceID)
  
  dataCompFile[c("CombinedDayRefCo", "CombinedHourRefCo", "CombinedInfoRefCo")] <- 
    combine_and_sort(
      list(dataCompFile$Day[info_ref_coloc], dataCompFile$PurpleAirDay[info_ref_coloc], dataCompFile$DayReference[info_ref_coloc]),
      list(dataCompFile$Hour[info_ref_coloc], dataCompFile$PurpleAirHour[info_ref_coloc], dataCompFile$HourReference[info_ref_coloc]),
      list(dataCompFile$Info %>% filter(Subtype %in% c("Reference", "Co-located")), 
           dataCompFile$PurpleAirInfo %>% filter(Subtype %in% c("Reference", "Co-located")), 
           dataCompFile$ReferenceInfo %>% filter(Subtype %in% c("Reference", "Co-located"))
      )
    )
  
  dataCompFile[c("CombinedDayPark", "CombinedHourPark", "CombinedInfoPark")] <- 
    combine_and_sort(
      list(dataCompFile$Day[info_park], dataCompFile$PurpleAirDay[info_park]),
      list(dataCompFile$Hour[info_park], dataCompFile$PurpleAirHour[info_park]),
      list(dataCompFile$Info %>% filter(Subtype %in% c("Park")), 
           dataCompFile$PurpleAirInfo %>% filter(Subtype %in% c("Park"))
      )
    )
  
  dataCompFile[c("CombinedDayNonPark", "CombinedHourNonPark", "CombinedInfoNonPark")] <- 
    combine_and_sort(
      list(dataCompFile$Day[info_nonpark], dataCompFile$PurpleAirDay[info_nonpark]),
      list(dataCompFile$Hour[info_nonpark], dataCompFile$PurpleAirHour[info_nonpark]),
      list(dataCompFile$Info %>% filter(Subtype %in% c("Non-park")), 
           dataCompFile$PurpleAirInfo %>% filter(Subtype %in% c("Non-park"))
      )
    )
  
  # Data without PurpleAir
  dataCompFile[c("CombinedDayRefCoNoPA", "CombinedHourRefCoNoPA", "CombinedInfoRefCoNoPA")] <- 
    combine_and_sort(
      list(dataCompFile$Day[info_ref_coloc], dataCompFile$DayReference[info_ref_coloc]),
      list(dataCompFile$Hour[info_ref_coloc], dataCompFile$HourReference[info_ref_coloc]),
      list(dataCompFile$Info %>% filter(Subtype %in% c("Reference", "Co-located")),
           dataCompFile$ReferenceInfo %>% filter(Subtype %in% c("Reference", "Co-located"))
      )
    )
  
  dataCompFile[c("CombinedDayParkNoPA", "CombinedHourParkNoPA", "CombinedInfoParkNoPA")] <- 
    combine_and_sort(
      list(dataCompFile$Day[info_park]),
      list(dataCompFile$Hour[info_park]),
      list(dataCompFile$Info %>% filter(Subtype %in% c("Park"))
      )
    )
  
  dataCompFile[c("CombinedDayNonParkNoPA", "CombinedHourNonParkNoPA", "CombinedInfoNonParkNoPA")] <- 
    combine_and_sort(
      list(dataCompFile$Day[info_nonpark]),
      list(dataCompFile$Hour[info_nonpark]),
      list(dataCompFile$Info %>% filter(Subtype %in% c("Non-park"))
      )
    )
  
  return(dataCompFile)
}

# Support functions ####
# Hack-y function for autofitting tables - Hopefully RMarkdown fix it in the future :'D
FitFlextableToPage <- function(ft, pgwidth = 6.5){
  ft_out <- ft %>% autofit()
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  ft_out <- ft_out %>% paginate(init = FALSE, hdr_ftr = TRUE)
  return(ft_out)
}

CheckNotEmptyString <- function(x) {
  return(!is.null(x) && !is.na(x) && x != "" && length(x) > 0)
}

GetAnnotateInfoForScatterplot <- function(type) {
  if (type == "PM25") {
    return(list(
      hourlyLabel = expression("Hourly standard (35 µg/m"^{3}*")"),
      annualLabel = expression("Annual standard (9 µg/m"^{3}*")"),
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
  print("Type not found!")
  return(NULL);
}

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
      # Both graphs are available
      combined_plot <- graph1 + 
        (graph2 + theme(axis.title.y = element_blank(), axis.text.y = element_blank())) +
        plot_layout(ncol = 2, nrow = 1)
      
      i <- i + 2  # Move to the next pair
    } 
    else if (!is.null(graph1)) {
      j <- i + 2
      foundGraph = F
      while (j <= length(myGraph)) {
        if (!is.null(myGraph[[j]])) {
          foundGraph = T
          break
        }
        j <- j + 1
      }
      if (foundGraph) { # Found a j not null
        # Both graphs are available
        combined_plot <- graph1 + 
          (myGraph[[j]] + theme(axis.title.y = element_blank(), axis.text.y = element_blank())) +
          plot_layout(ncol = 2, nrow = 1)
      }
      else {
        combined_plot <- graph1 + plot_layout(ncol = 2, nrow = 1)
      }
      i <- j + 1  # move to next index
    } 
    else {
      # Only graph2 is available
      i <- i + 1  # Move to the next graph
      next
    }
    
    print(combined_plot)
  }
}

# Descriptive statistics functions ####
# Deprecated
generate_box_plot_data <- function(datafiles, columnName, renameName, newLabel, dataInfo) {
  dataTb <- purrr::map_dfr(
    .x = datafiles,
    .f = function(x) {
      x %>% dplyr::select(datasourceId, !!columnName) %>%
        dplyr::rename(y_value := !!columnName)
    }
  ) 
  
  # Create a named vector to map datasourceId to ShortCode
  label_map <- setNames(dataInfo$ShortCode, dataInfo$DeviceID)
  
  plot <- ggplot(dataTb, mapping = aes(x = datasourceId, y = y_value)) + 
    geom_boxplot() +
    theme_classic() +
    labs(x = "Short Code", y = renameName, color = "Legend", title = newLabel) +
    ylim(0, 60) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5), 
          legend.position = c(0.8, 0.82), 
          legend.background = element_rect(fill = "white", color = "black", size = 0.5),
          legend.key.height = unit(0.35, 'cm')
    ) + 
    scale_x_discrete(labels = label_map) 
  return(plot)
}

generate_double_box_plot <- 
  function(datafiles, columnName1, columnName2, renameName, newLabel, dataInfo, renameLegend1, renameLegend2) {
    dataTb <- purrr::map_dfr(
      .x = datafiles,
      .f = function(x) {
        x %>% dplyr::select(datasourceId, !!columnName1, !!columnName2)
      }
    ) 
    
    # Reshape data to long format for ggplot
    dataTb <- tidyr::pivot_longer(
      dataTb,
      cols = c(!!columnName1, !!columnName2),
      names_to = "variable",
      values_to = "y_value"
    )
    dataInfo <- dataInfo %>%
      dplyr::mutate(BoxLabel = paste(DeviceID, "\n", ShortCode, sep = ""),
                    BoxLabel = ifelse(Type == "Reference", paste0(BoxLabel, " (ref)"), BoxLabel))
    
    # Create a named vector to map datasourceId to ShortCode
    label_map <- setNames(dataInfo$BoxLabel, dataInfo$DeviceID)
    variable_labels <- c(renameLegend1, renameLegend2)
    
    # Ensure the order of datasourceId is preserved
    dataTb$datasourceId <- factor(dataTb$datasourceId, levels = unique(dataTb$datasourceId))
    
    plot <- ggplot(dataTb, mapping = aes(x = datasourceId, y = y_value, fill = variable)) + 
      geom_boxplot() +
      theme_classic() +
      labs(x = "Short Code", y = renameName, color = "Legend", title = newLabel) +
      ylim(0, 60) + 
      theme(axis.text.x = element_text(angle = 55, vjust = 0.5), 
            legend.position = c(0.8, 0.9), 
            legend.background = element_rect(fill = "white", color = "black", size = 0.5),
            legend.key.height = unit(0.35, 'cm')
      ) + 
      #theme_tufte() + # http://motioninsocial.com/tufte/
      scale_fill_discrete(labels = variable_labels, na.translate = F) + 
      scale_x_discrete(labels = label_map) 
    return(plot)
}

generate_descriptive_stats_table <- 
  function(datafiles, columnName, renameName, monitorInfo, title = "Sample Title - Please fill") {
    datafiles <- purrr::map(
      .x = datafiles,
      .f = \(x) { return(x %>% dplyr::filter(!!rlang::sym(columnName) <= 500)) } # filter data above 500
    )
    result <- data.frame(
      names(datafiles),
      purrr::map_int(datafiles, \(x) nrow(x)),
      purrr::map_dbl(datafiles, \(x) x %>% pull(columnName) %>% na.omit() %>% min()),
      purrr::map_dbl(datafiles, \(x) x %>% pull(columnName) %>% na.omit() %>% quantile(probs = 0.25)),
      purrr::map_dbl(datafiles, \(x) x %>% pull(columnName) %>% na.omit() %>% quantile(probs = 0.50)),
      purrr::map_dbl(datafiles, \(x) x %>% pull(columnName) %>% na.omit() %>% quantile(probs = 0.75)),
      purrr::map_dbl(datafiles, \(x) x %>% pull(columnName) %>% na.omit() %>% max())
    ) %>% 
    as_tibble() %>% 
    mutate(across(where(is.double), ~ round(., 2)))
    
    colnames(result) <- c("Datasource", "Count", "Minimum", "25th Quantile", "Median", "75th Quantile", "Maximum")
    
    monitorInfo <- monitorInfo %>% dplyr::select(DeviceID, ShortCode, Type)
    result <- dplyr::full_join(result, monitorInfo, by = c("Datasource" = "DeviceID")) %>%
      dplyr::rename(Site = ShortCode) %>%
      dplyr::relocate(Site, .after = Datasource) %>%
      dplyr::relocate(Type, .after = Site) #%>%
      #dplyr::relocate(Subtype, .after = Type) 
    
    light_red_palette <- colorRampPalette(c("#ffd3d3", "#FAA0A0", "#FF5733"))(100)
    light_blue_palette <- colorRampPalette(c("#E4F6F8", "lightblue", "#4B92DB"))(100)
    light_yellow_palette <- colorRampPalette(c("#fff2cc", "#ffe599", "#ffd966"))(100)
    
    tb <- flextable(result) %>%
      #add_header_row(values = c(title), colwidths = c(ncol(result))) %>%
      FitFlextableToPage() %>%
      width(j = "Type", width = 0.5) %>%
      width(j = "Type", width = 0.6) %>%
      set_table_properties(opts_pdf = list(tabcolsep = 4)) %>%
      bg(j = "Median", 
         bg = scales::col_numeric(light_blue_palette, 
                                  domain = range(result$Median, na.rm = TRUE))(result$Median)) %>%
      bg(j = "Minimum", 
         bg = scales::col_numeric(light_yellow_palette, 
                                  domain = range(result$Minimum[is.finite(result$Minimum)], na.rm = TRUE))(result$Minimum)) %>%
      bg(j = "Maximum", 
         bg = scales::col_numeric(light_red_palette, 
                                  domain = range(result$Maximum[is.finite(result$Maximum)], na.rm = TRUE))(result$Maximum))
    tb
  }

# Trend graphs functions ####
generate_scatter_plot_data <- 
  function(data, columnName1, columnName2, renameName, newLabel, type = "PM25") {
  data <- data %>%
    dplyr::mutate(!!rlang::sym(columnName1) := as.double(!!rlang::sym(columnName1))) %>%
    dplyr::mutate(!!rlang::sym(columnName2) := as.double(!!rlang::sym(columnName2))) %>%
    pivot_longer(cols = c(!!sym(columnName1), !!sym(columnName2)),
                 names_to = "variable",
                 values_to = "y_value") %>%
    dplyr::mutate(y_value = as.double(y_value)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(startOfPeriod = as.POSIXct(startOfPeriod, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") %>%
                                  format("%Y-%m-%d", tz = "Etc/GMT+5") %>%
                                  as.Date()) %>%
    dplyr::ungroup()
  
  variable_labels <- setNames(c("Raw Values", "Calibrated Values"),
                              c(columnName1, columnName2))
  
  # Define the full date range for the month
  startDate <- params$myData$Date
  endDate <- (startDate + months(1)) - days(1)
  
  tmpDf <- data.frame(startOfPeriod = seq.Date(startDate, endDate, by = "day"))
  data <- dplyr::full_join(tmpDf, data, by = join_by(startOfPeriod))
  
  annotateInfo <- GetAnnotateInfoForScatterplot(type)
  
  plot <- ggplot(data, aes(x = startOfPeriod, y = y_value, color = variable)) +
    geom_hline(aes(yintercept = annotateInfo$hourlyValue), color = "orange") +
    annotate("text", x = min(data$startOfPeriod), 
             y = annotateInfo$hourlyValue, 
             label = annotateInfo$hourlyLabel, 
             vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) + 
    geom_hline(aes(yintercept = annotateInfo$annualValue), color = "orange") +
    annotate("text", x = min(data$startOfPeriod), 
             y = annotateInfo$annualValue, 
             label = annotateInfo$annualLabel, 
             vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) + 
    geom_point() +
    theme_classic() + 
    labs(x = "Date", y = renameName, color = "Legend", title = newLabel) +
    ylim(0, 60) + 
    scale_color_discrete(labels = variable_labels, na.translate = F) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5), 
          legend.position = c(0.8, 0.82), 
          legend.background = element_rect(fill = "white", color = "black", size = 0.5),
          legend.key.height = unit(0.35, 'cm')
    ) + 
    scale_x_date(breaks = seq.Date(startDate, endDate, by = "5 days"),
                 date_labels = "%m / %d")
  
  return(plot)
}

generate_diurnal_boxplot_data <- function(data, columnName, renameName, newLabel, type = "PM25") {
  lev <- as.character(0:23)
  data <- data %>%
    dplyr::mutate(!!rlang::sym(columnName) := as.double(!!rlang::sym(columnName))) %>%
    dplyr::mutate(y_value = !!rlang::sym(columnName)) 
  
  # Convert the original data's startOfPeriod column to POSIXct with the same timezone
  data <- data %>%
    dplyr::mutate(startOfPeriod = as.POSIXct(startOfPeriod, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") %>%
                    with_tz(tzone = "Etc/GMT+5"))
  
  # Group data by hours in day
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(hourTime = factor(lubridate::hour(startOfPeriod), levels = 0:23, labels = lev)) %>%
    dplyr::ungroup()
  
  annotateInfo <- GetAnnotateInfoForScatterplot(type)
  
  plot <- ggplot(data, aes(x = hourTime, y = y_value)) +
    geom_hline(aes(yintercept = annotateInfo$annualValue), color = "orange") +
    annotate("text", x = "0", y = annotateInfo$annualValue, label = annotateInfo$annualLabel, 
             vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) + 
    geom_hline(aes(yintercept = annotateInfo$hourlyValue), color = "orange") +
    annotate("text", x = "0", y = annotateInfo$hourlyValue, label = annotateInfo$hourlyLabel, 
             vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) + 
    geom_boxplot(size = 0.5) +
    stat_summary(fun = mean, geom = "line", aes(group = 1), color = "blue", size = 0.5) +
    labs(x = "Time of Day (UTC-05:00)", y = renameName, color = "Legend", title = newLabel) +
    ylim(0, 60) + 
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
    scale_x_discrete(limits = lev, drop = FALSE, labels = lev)
  
  return(plot)
}

generate_weekly_boxplot_data <- function(data, columnName, renameName, newLabel, type = "PM25") {
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
  startDate <- params$myData$Date
  endDate <- (startDate + months(1)) - days(1)
  tmpDf <- data.frame(startOfPeriod = seq.Date(startDate, endDate, by = "day"))
  data <- dplyr::full_join(tmpDf, data, by = join_by(startOfPeriod))
  
  # Group data by days in week
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dayTime = factor(lubridate::wday(startOfPeriod, abbr = T, label = T), levels = lev)) %>%
    dplyr::ungroup() 
  
  annotateInfo <- GetAnnotateInfoForScatterplot(type)
  
  plot <- ggplot(data, aes(x = dayTime, y = y_value)) +
    geom_hline(aes(yintercept = annotateInfo$annualValue), color = "orange") +
    annotate("text", x = "Mon", y = annotateInfo$annualValue, label = annotateInfo$annualLabel, 
             vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) + 
    geom_hline(aes(yintercept = annotateInfo$hourlyValue), color = "orange") +
    annotate("text", x = "Mon", y = annotateInfo$hourlyValue, label = annotateInfo$hourlyLabel, 
             vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) + 
    geom_boxplot(size = 0.5) +
    stat_summary(fun = mean, geom = "line", aes(group = 1), color = "blue", size = 0.5) +
    labs(x = "Day of Week", y = renameName, color = "Legend", title = newLabel) +
    ylim(0, 60) + 
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
    scale_x_discrete(labels = lev) 
  
  return(plot)
}

generate_sitebase_scatter_plot_data <- 
  function(data, infoData, 
           shortToFilter, columnName, renameName, newLabel, type = "PM25") 
  {
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
      dplyr::mutate(Label = if_else(grepl("^R", sourceId), 
                                    paste(datasourceId, " (ref)", sep = ""),
                                    datasourceId
        )
      )
    
    # Define the full date range for the month
    sitesData <- sitesData %>%
      dplyr::rowwise() %>%
      dplyr::mutate(startOfPeriod = as.POSIXct(startOfPeriod, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") %>%
                      format("%Y-%m-%d", tz = "Etc/GMT+5") %>%
                      as.Date()) %>%
      dplyr::ungroup()
    
    startDate <- params$myData$Date
    endDate <- (startDate + months(1)) - days(1)
    tmpDf <- data.frame(startOfPeriod = seq.Date(startDate, endDate, by = "day"))
    sitesData <- dplyr::full_join(tmpDf, sitesData, by = join_by(startOfPeriod))
    
    annotateInfo <- GetAnnotateInfoForScatterplot(type)
    
    plot <- ggplot(sitesData, aes(x = startOfPeriod, y = y_value, color = Label)) +
      geom_hline(aes(yintercept = annotateInfo$annualValue), color = "orange") +
      annotate("text", x = min(sitesData$startOfPeriod), y = annotateInfo$annualValue, label = annotateInfo$annualLabel, 
               vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) + 
      geom_hline(aes(yintercept = annotateInfo$hourlyValue), color = "orange") +
      annotate("text", x = min(sitesData$startOfPeriod), y = annotateInfo$hourlyValue, label = annotateInfo$hourlyLabel, 
               vjust = -0.5, hjust = +0.02, color = "black", size = 3, parse = TRUE) + 
      geom_point() +
      labs(x = "Date", y = renameName, color = "Legend", title = newLabel) +
      ylim(0, 60) + 
      theme_classic() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5), 
            legend.position = c(0.8, 0.85), 
            legend.background = element_rect(fill = "white", color = "black", size = 0.5),
            legend.key.height = unit(0.35, 'cm')
      ) + 
      scale_color_discrete(na.translate = F) +
      scale_x_date(breaks = seq.Date(startDate, endDate, by = "5 days"),
                   date_labels = "%m / %d")
    
    return(plot)
  }