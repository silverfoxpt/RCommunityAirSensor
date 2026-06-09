# IMPORTANT: ALL FILES IN THIS FOLDER ARE SUBJECTED TO CHANGE ONCE THE FOLDERS' PATHS CHANGE
# BE CAREFUL WHEN USING! ####

# Weekly CSV files' functions ####
 #' Read weekly Qualtrics log CSV
 #'
 #' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @return A tibble with the weekly log contents.
 #' @concept role:helper
get_weekly_log <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  logfile <- read.csv(file.path(root_folder, "CSV", "Exports", "QualtricsWeeklyLog.csv")) %>%
    tibble::as_tibble()
  return(logfile)
}

 #' Read template update log CSV
 #'
 #' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @return A tibble with the update log contents.
 #' @concept role:helper
get_update_log <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  logfile <- read.csv(file.path(root_folder, "CSV", "Exports", "QualtricsUpdateLog.csv")) %>%
    tibble::as_tibble()
  return(logfile)
}

 #' Read weekly personnel list from monitor tracking
 #'
 #' Reads the QATimeshift Excel sheet and extracts Name/Email/SiteShortCode.
 #'
 #' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @return A tibble of personnel and short codes.
 #' @concept role:helper
get_weekly_personnel_list <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  date_suffix <- lubridate::floor_date(Sys.Date(), unit = "month")
  timeshift_filename <- sprintf("CAMNMonitorTracking_%s.xlsx", date_suffix)
  timeshift_file <- file.path(root_folder, "CSV", "QATimeshift", timeshift_filename)

  readMonitorTracking <-
    readxl::read_xlsx(
      path = timeshift_file,
      sheet = "SitesAndHosts",
      range = "A2:G100",
      .name_repair = "unique_quiet"
    ) %>%
    tibble::as_tibble() %>%
    dplyr::rename("Name" = "Host contact person") %>%
    dplyr::rename("Email" = "Email") %>%
    dplyr::rename("SiteShortCode" = "Short code") %>%
    dplyr::select(Name, Email, SiteShortCode) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(FirstName = strsplit(Name, split = " ")[[1]][1]) %>%
    dplyr::mutate(LastName = strsplit(Name, split = " ")[[1]][2]) %>%
    dplyr::ungroup()

  readMonitorTracking <- readMonitorTracking %>%
    dplyr::filter(nchar(SiteShortCode) >= 3)

  return(readMonitorTracking)
}

 #' Read main personnel CSV and optionally filter by role
 #'
 #' @param role Optional character or list of roles to filter by.
 #' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @return A tibble of participants, optionally filtered.
 #' @concept role:helper
get_main_personnel_list <- function(role = NULL, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  participants <- read.csv(file.path(root_folder, "CSV", "Imports", "MainPersonnel.csv")) %>%
    tibble::as_tibble()

  if (is.null(role)) {
    return(participants)
  }

  if (is.list(role)) {
    role <- unlist(role)
  }

  return(participants %>% dplyr::filter(Role %in% role))
}

# Monthly CSV files' functions ####
 #' Read monthly Qualtrics log CSV
 #'
 #' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @return A tibble with the monthly log contents.
 #' @concept role:helper
get_monthly_log <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  logfile <- read.csv(file.path(root_folder, "CSV", "Exports", "QualtricsMonthlyLog.csv")) %>%
    tibble::as_tibble()
  return(logfile)
}
#tmp <- get_monthly_log()

 #' Read monthly question shortlist
 #'
 #' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @return A tibble with question shortlist.
 #' @concept role:helper
get_monthly_question_shortlist <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  logfile <- read.csv(file.path(root_folder, "CSV", "Imports", "MonthlyUpdateQuestion.csv")) %>%
    tibble::as_tibble()
  return(logfile)
}
#tmp <- get_monthly_question_shortlist()

 #' Read weekly question shortlist
 #'
 #' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @return A tibble with question shortlist.
 #' @concept role:helper
get_weekly_question_shortlist <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  logfile <- read.csv(file.path(root_folder, "CSV", "Imports", "WeeklyUpdateQuestion.csv")) %>%
    tibble::as_tibble()
  return(logfile)
}
#tmp <- get_weekly_question_shortlist()

 #' Pull first SaveData matching originDate and action
 #'
 #' @param logFile Tibble or data.frame log file.
 #' @param originDate Character or date to match in `OriginDate`.
 #' @param neededAction Character to match in `Action`.
 #' @return The first matching `SaveData` value (or NULL if none).
 #' @concept role:helper
get_first_save_data_from_weekly_log <- function(logFile, originDate, neededAction) {
  info <- logFile %>% dplyr::filter(OriginDate == originDate & Action == neededAction)
  saveData <- info %>% dplyr::slice(1) %>% dplyr::pull("SaveData")
  return(saveData)
}

 #' Alias for monthly logs
 #'
#' @param logFile Tibble or data.frame log file.
#' @param originDate Character or date to match in `OriginDate`.
#' @param neededAction Character to match in `Action`.
#' @return The first matching `SaveData` value (or NULL if none).
 #' @concept role:helper
get_first_save_data_from_monthly_log <- function(logFile, originDate, neededAction) {
  return(get_first_save_data_from_weekly_log(logFile, originDate, neededAction))
}

 #' Check existence in log
 #'
#' @param logFile Tibble or data.frame log file.
#' @param originDate Character or date to match in `OriginDate`.
#' @param neededAction Character to match in `Action`.
 #' @return Logical TRUE if a matching row exists, otherwise FALSE.
 #' @concept role:helper
check_exist_in_log <- function(logFile, originDate, neededAction) {
  if (nrow(logFile %>% dplyr::filter(OriginDate == originDate & Action == neededAction)) > 0) {
    return(TRUE)
  }
  return(FALSE)
}

 #' Check not exist in log
 #'
#' @param logFile Tibble or data.frame log file.
#' @param originDate Character or date to match in `OriginDate`.
#' @param neededAction Character to match in `Action`.
 #' @return Logical TRUE if no matching row exists, otherwise FALSE.
 #' @concept role:helper
check_not_exist_in_log <- function(logFile, originDate, neededAction) {
  if (nrow(logFile %>% dplyr::filter(OriginDate == originDate & Action == neededAction)) <= 0) {
    return(TRUE)
  }
  return(FALSE)
}

 #' Append a row to the weekly log CSV
 #'
 #' @param originDate Character or date to record.
 #' @param neededAction Character action label.
 #' @param saveData Character save data to store.
 #' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @concept role:helper
write_to_weekly_log <- function(originDate, neededAction, saveData, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  write.table(
    data.frame(
      OriginDate = c(originDate),
      Action = c(neededAction),
      SaveData = c(saveData)
    ),
    file = file.path(root_folder, "CSV", "Exports", "QualtricsWeeklyLog.csv"),
    sep = ",",
    col.names = FALSE,
    row.names = FALSE,
    append = TRUE
  )
}

 #' Append a row to the monthly log CSV
 #'
 #' @param originDate Character or date to record.
 #' @param neededAction Character action label.
 #' @param saveData Character save data to store.
 #' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @concept role:helper
write_to_monthly_log <- function(originDate, neededAction, saveData, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  write.table(
    data.frame(
      OriginDate = c(originDate),
      Action = c(neededAction),
      SaveData = c(saveData)
    ),
    file = file.path(root_folder, "CSV", "Exports", "QualtricsMonthlyLog.csv"),
    sep = ",",
    col.names = FALSE,
    row.names = FALSE,
    append = TRUE
  )
}

 #' Append a row to the weekly template update log
 #'
 #' @param originDate Character or date to record.
 #' @param neededAction Character action label.
 #' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @concept role:helper
write_to_weekly_template_update_log <- function(originDate, neededAction, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  write.table(
    data.frame(
      OriginDate = c(originDate),
      Action = c(neededAction)
    ),
    file = file.path(root_folder, "CSV", "Exports", "QualtricsUpdateLog.csv"),
    sep = ",",
    col.names = FALSE,
    row.names = FALSE,
    append = TRUE
  )
}

 #' Append a row to the monthly template update log (alias)
 #'
#' @param originDate Character or date to record.
#' @param neededAction Character action label.
#' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @concept role:helper
write_to_monthly_template_update_log <- function(originDate, neededAction, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  write_to_weekly_template_update_log(originDate, neededAction, root_folder = root_folder)
}

## For unresolved monitor only ####
 #' Read unresolved monitor CSV
 #'
 #' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @return A tibble with unresolved monitors.
 #' @concept role:helper
get_unresolved_monitor_log <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  logfile <- read.csv(file.path(root_folder, "CSV", "Imports", "UnresolvedMonitor.csv"),
                      colClasses = "character") %>%
    tibble::as_tibble()
  return(logfile)
}
#tmp2 <- get_unresolved_monitor_log()

 #' Concentrate unresolved monitor entries into unresolvedList
 #'
 #' Scans monitor summaries for flagged entries (asterisk) and appends rows
 #' to `unresolvedList` describing the reason.
 #'
 #' @param myData A list with a `Monitors` tibble/data.frame.
 #' @param unresolvedList A tibble to which unresolved rows will be appended.
 #' @return The updated `unresolvedList`.
 #' @concept role:helper
concentrate_unresolved_monitor_qualtrics <- function(myData, unresolvedList) {
  current_date <- Sys.Date()
  start_of_current_month <- lubridate::floor_date(current_date, unit = "month")

  purpleTrackingSummary <- myData$Monitors %>%
    dplyr::filter(grepl("PA", Label)) %>%
    dplyr::rename(`Tracking Needed` = PATQuestion1)

  purpleHealthSummary <- myData$Monitors %>%
    dplyr::filter(grepl("PA", Label)) %>%
    dplyr::mutate(`Maintenance Needed` = dplyr::if_else(
      rowSums(dplyr::across(dplyr::starts_with("PAH"), ~ grepl("\\*", .))) > 0,
      "Needs follow-up (*)",
      "No"
    )) %>%
    dplyr::select(DeviceID, `Maintenance Needed`)

  purpleDataSummary <- myData$Monitors %>%
    dplyr::filter(grepl("PA", Label)) %>%
    dplyr::mutate(`Data Follow-up Needed` = dplyr::if_else(
      rowSums(dplyr::across(dplyr::starts_with("PAD"), ~ grepl("\\*", .))) > 0,
      "Needs follow-up (*)",
      "No"
    )) %>%
    dplyr::select(DeviceID, `Data Follow-up Needed`)

  paMerger <- purpleTrackingSummary %>%
    dplyr::full_join(purpleHealthSummary, by = "DeviceID") %>%
    dplyr::full_join(purpleDataSummary, by = "DeviceID") %>%
    dplyr::rename(ID = DeviceID)

  clarityTrackingSummary <- myData$Monitors %>%
    dplyr::filter(grepl("CN", Label)) %>%
    dplyr::rename(`Tracking Summary` = CTQuestion1)

  clarityHealthSummary <- myData$Monitors %>%
    dplyr::filter(grepl("CN", Label)) %>%
    dplyr::mutate(`Maintenance Needed` = dplyr::if_else(
      rowSums(dplyr::across(dplyr::starts_with("CH"), ~ grepl("\\*", .))) > 0,
      "Needs follow-up (*)",
      "No"
    )) %>%
    dplyr::select(DeviceID, `Maintenance Needed`)

  clarityDataSummary <- myData$Monitors %>%
    dplyr::filter(grepl("CN", Label)) %>%
    dplyr::mutate(`Data Follow-up Needed` = dplyr::if_else(
      rowSums(dplyr::across(dplyr::starts_with("CD"), ~ grepl("\\*", .))) > 0,
      "Needs follow-up (*)",
      "No"
    )) %>%
    dplyr::select(DeviceID, `Data Follow-up Needed`)

  clMerger <- clarityTrackingSummary %>%
    dplyr::full_join(clarityHealthSummary, by = "DeviceID") %>%
    dplyr::full_join(clarityDataSummary, by = "DeviceID") %>%
    dplyr::rename(ID = DeviceID) %>%
    dplyr::rename(`Tracking Needed` = `Tracking Summary`)

  add_unresolved_rows <- function(df, column_name, reason) {
    df_filtered <- df %>%
      dplyr::filter(grepl("\\*", .data[[column_name]]))

    if (nrow(df_filtered) == 0) { return(); }

    unresolvedList <<- unresolvedList %>%
      dplyr::add_row(
        OriginDate = as.character(start_of_current_month),
        DeviceID = df_filtered$ID,
        SiteName = df_filtered$SiteName,
        Reason = reason,
        Resolved = as.character("No")
      )
  }

  add_unresolved_rows(paMerger, "Tracking Needed", "TrackingFail")
  add_unresolved_rows(paMerger, "Maintenance Needed", "HealthFail")
  add_unresolved_rows(paMerger, "Data Follow-up Needed", "DataArchiveFail")

  add_unresolved_rows(clMerger, "Tracking Needed", "TrackingFail")
  add_unresolved_rows(clMerger, "Maintenance Needed", "HealthFail")
  add_unresolved_rows(clMerger, "Data Follow-up Needed", "DataArchiveFail")

  return(unresolvedList)
}

# Others ####
# Updated: 20 Jan 2025
 #' Create mailing list and add contacts from personnel list
#'
#' Creates a new mailing list and adds each participant to it.
#'
#' @param qualtricsKey Character API token.
#' @param directoryID Character directory id.
#' @param mailingListName Character name for the new mailing list.
#' @param participantList A tibble/data.frame with at least FirstName, LastName, Email.
#' @return The created mailing list id (character).
#' @concept role:helper
create_and_add_contact_from_personnel_list <- function(qualtricsKey, directoryID, mailingListName, participantList) {
  # Filter by email
  participantList <- participantList %>% dplyr::distinct(Email, .keep_all = TRUE)

  # create mailing list
  mailingId <- create_mailing_list(
    qualtricsKey,
    directoryID,
    mailingListName
  )

  # add all contacts to mailing list
  purrr::pwalk(
    .l = list(
      participantList %>% dplyr::pull("FirstName"),
      participantList %>% dplyr::pull("LastName"),
      participantList %>% dplyr::pull("Email")
    ),
    .f = \(x, y, z) add_mailing_contact(qualtricsKey, directoryID, mailingId, x, y, z)
  )

  # sleep - wait for batch update on qualtrics
  # TODO: Add this into a loop for the future. Query qualtrics to check if mailing list has been fully updated.
  Sys.sleep(30)

  return(mailingId)
}

# Tested 17 Jan 2025
 #' Read monitor sites from timeshift Excel
#'
#' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble of monitor site metadata.
#' @concept role:helper
get_monitor_sites <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  date_suffix <- lubridate::floor_date(Sys.Date(), unit = "month")
  timeshift_filename <- sprintf("CAMNMonitorTracking_%s.xlsx", date_suffix)
  timeshift_file <- file.path(root_folder, "CSV", "QATimeshift", timeshift_filename)

  readMonitorTracking <-
    readxl::read_xlsx(
      path = timeshift_file,
      sheet = "MonitorStatus",
      range = "A2:J100",
      .name_repair = "unique_quiet"
    ) %>%
    tibble::as_tibble() %>%
    dplyr::rename("DeviceID" = "API ID") %>%
    dplyr::rename("OrgID" = "Dashboard/API Organization ID") %>%
    dplyr::rename("ShortCode" = "Location short code") %>%
    dplyr::rename("SiteName" = "Deployed Site Location")

  numCol <- ncol(readMonitorTracking)
  readMonitorTracking <- readMonitorTracking %>%
    dplyr::filter(rowSums(is.na(.) | . == "") < numCol) %>%
    dplyr::filter(nchar(ShortCode) >= 3) %>%
    dplyr::filter(grepl("public", `Data sharing setting`))

  return(readMonitorTracking)
}

 #' Read monthly question info CSV
 #'
 #' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @return Data.frame of the question info.
 #' @concept role:helper
get_monthly_question_info <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  questionInfo <- file.path(root_folder, "CSV", "Imports", "MonthlyUpdateQuestion.csv") %>%
    read.csv()

  return(questionInfo)
}

# Functions for processing responses and personnel data ####
## For weekly personnel ####
get_merge_personnel_sensor_list <- function(sensorType = NULL) {
  # Explanation:
  # Each personnel have a site short code assigned to them
  # Each site can have multiple sensors.
  # This function merge these two aspect, so that the personnel
  #   can be sent multiple emails if needed
  # The function will return a tibble, with some rows info duplicated

  participants <- get_weekly_personnel_list()
  sitesInfo <- get_monitor_sites()

  merger <- dplyr::full_join(x = participants, y = sitesInfo, by = c("SiteShortCode" = "ShortCode"))

  if (!is.null(sensorType)) {
    return(merger %>% dplyr::filter(grepl(sensorType, Type)))
  }
  return(merger)
}

# Updated: 20 Jan 2025
 #' Read processed weekly responses
 #'
 #' @param responseFileName Character file name under `CSV/Qualtrics/Weekly`.
 #' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @return A tibble of responses.
 #' @concept role:helper
get_processed_responses_list <- function(responseFileName, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  fullresData <-
    read.csv(file.path(root_folder, "CSV", "Qualtrics", "Weekly", responseFileName)) %>%
    tibble::as_tibble()

  return(fullresData)
}

# Updated: 20 Jan 2025
 #' Read question descriptions (weekly)
 #'
 #' @param questionDescFileName Character file name under `CSV/Qualtrics/Weekly`.
 #' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @return A tibble with question descriptions and split columns.
 #' @concept role:helper
get_question_descriptions <- function(questionDescFileName, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  questionDescData <-
    read.csv(file.path(root_folder, "CSV", "Qualtrics", "Weekly", questionDescFileName)) %>%
    tibble::as_tibble()

  questionDescData <- questionDescData %>%
    tidyr::separate(sub, c("DeviceID", "DeviceType", "Site"), sep = ",", remove = TRUE)

  return(questionDescData)
}

# Updated: 1/7/2024
 #' Get personnel who have not responded (weekly)
 #'
 #' @param responseFileName Character file name of weekly responses.
 #' @param root_folder Character root folder path.
 #' @return A tibble of personnel who have not responded.
 #' @concept role:helper
get_unresponsed_personnel_list <- function(responseFileName, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  responses <- get_processed_responses_list(responseFileName, root_folder = root_folder)

  personnel_sensor_list <-
    get_merge_personnel_sensor_list() %>%
    dplyr::filter(!is.na(Email))

  unresponsed_personnel <- personnel_sensor_list %>%
    dplyr::filter(!Email %in% responses$RecipientEmail)

  return(unresponsed_personnel)
}

# Updated: 1/7/2024
 #' Get personnel who have responded (weekly)
 #'
 #' @param responseFileName Character file name of weekly responses.
 #' @param root_folder Character root folder path.
 #' @return A tibble of personnel who have responded.
 #' @concept role:helper
get_responsed_personnel_list <- function(responseFileName, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  responses <- get_processed_responses_list(responseFileName, root_folder = root_folder)

  personnel_sensor_list <-
    get_merge_personnel_sensor_list() %>%
    dplyr::filter(!is.na(Email))

  responsed_personnel <- personnel_sensor_list %>%
    dplyr::filter(Email %in% responses$RecipientEmail)

  return(responsed_personnel)
}

## For monthly personnel ####
 #' Read processed monthly responses
 #'
 #' @param responseFileName Character file name under `CSV/Qualtrics/Monthly`.
 #' @param root_folder Character root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @return A tibble of responses.
 #' @concept role:helper
get_monthly_responses_list <- function(responseFileName, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  fullresData <-
    read.csv(file.path(root_folder, "CSV", "Qualtrics", "Monthly", responseFileName)) %>%
    tibble::as_tibble()

  return(fullresData)
}

 #' Get analysts who have not responded (monthly)
 #'
 #' @param responseFileName Character file name of monthly responses.
 #' @param root_folder Character root folder path.
 #' @return A tibble of analysts who have not responded.
 #' @concept role:helper
get_unresponsed_analyst_list <- function(responseFileName, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  responses <- get_monthly_responses_list(responseFileName, root_folder = root_folder)

  personnel_sensor_list <-
    get_main_personnel_list(role = "Analyst", root_folder = root_folder) %>%
    dplyr::filter(!is.na(Email))

  unresponsed_personnel <- personnel_sensor_list %>%
    dplyr::filter(!Email %in% responses$RecipientEmail)

  return(unresponsed_personnel)
}
#tmp <- get_unresponsed_analyst_list("Qualtrics_Monthly_Response_monthOf_2024-08-01.csv")

 #' Get analysts who have responded (monthly)
 #'
 #' @param responseFileName Character file name of monthly responses.
 #' @param root_folder Character root folder path.
 #' @return A tibble of analysts who have responded.
 #' @concept role:helper
get_responsed_analyst_list <- function(responseFileName, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  responses <- get_monthly_responses_list(responseFileName, root_folder = root_folder)

  personnel_sensor_list <-
    get_main_personnel_list(role = "Analyst", root_folder = root_folder) %>%
    dplyr::filter(!is.na(Email))

  responsed_personnel <- personnel_sensor_list %>%
    dplyr::filter(Email %in% responses$RecipientEmail)

  return(responsed_personnel)
}

 #' Read and split monthly question descriptions
 #'
 #' @param questionDescFileName Character file name under `CSV/Qualtrics/Monthly`.
 #' @param splitLikeUnresponsed Logical toggle for alternative split format.
 #' @param root_folder Character root folder path.
 #' @return A tibble with separated columns.
 #' @concept role:helper
get_monthly_question_descriptions <- function(questionDescFileName, splitLikeUnresponsed = FALSE, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  questionDescData <-
    read.csv(file.path(root_folder, "CSV", "Qualtrics", "Monthly", questionDescFileName)) %>%
    tibble::as_tibble()

  if (!splitLikeUnresponsed) {
    questionDescData <- questionDescData %>%
      tidyr::separate(sub, c("DeviceID", "DeviceType", "Site"), sep = ",", remove = TRUE)
  } else {
    questionDescData <- questionDescData %>%
      tidyr::separate(sub, c("ErrorDate", "DeviceID", "Site", "Reason"), sep = ", ", remove = TRUE)
  }
  return(questionDescData)
}
