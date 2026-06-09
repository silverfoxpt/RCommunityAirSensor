#' Get weekly Qualtrics log
#'
#' Read the weekly Qualtrics log CSV from the configured upload folder.
#'
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble with the weekly log contents.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_weekly_log <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  path <- file.path(root_folder, "CSV", "Exports", "QualtricsWeeklyLog.csv")
  data <- utils::read.csv(path)
  tibble::as_tibble(data)
}

#' Get Qualtrics template update log
#'
#' Read the Qualtrics update log CSV from the configured upload folder.
#'
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble with the update log contents.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_update_log <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  path <- file.path(root_folder, "CSV", "Exports", "QualtricsUpdateLog.csv")
  tibble::as_tibble(utils::read.csv(path))
}

#' Read weekly personnel list from timeshift workbook
#'
#' Loads the "SitesAndHosts" sheet from the monthly CAMN monitor tracking workbook
#' and returns a tibble of contact names, emails, and site short codes. The
#' reporting month can be overridden with the `date` argument.
#'
#' @param date Date. Date used to determine the monthly timeshift filename. Defaults to `Sys.Date()`.
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble with columns `Name`, `Email`, `SiteShortCode`, `FirstName`, `LastName`.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_weekly_personnel_list <- function(date = Sys.Date(), root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  date_suffix <- lubridate::floor_date(date, unit = "month")
  timeshift_filename <- sprintf("CAMNMonitorTracking_%s.xlsx", date_suffix)
  timeshift_file <- file.path(root_folder, "CSV", "QATimeshift", timeshift_filename)

  df <- readxl::read_xlsx(path = timeshift_file,
                          sheet = "SitesAndHosts",
                          range = "A2:G100",
                          .name_repair = "unique_quiet")

  tib <- tibble::as_tibble(df)
  tib <- dplyr::rename(tib, Name = `Host contact person`, Email = Email, SiteShortCode = `Short code`)
  tib <- dplyr::select(tib, Name, Email, SiteShortCode)

  # Split first/last name; keep simple behavior from original code
  tib <- tib %>%
    dplyr::mutate(
      FirstName = stringr::word(Name, 1),
      LastName = stringr::word(Name, 2)
    )

  tib <- dplyr::filter(tib, nchar(SiteShortCode) >= 3)
  tib
}

#' Read main personnel list
#'
#' Loads the main personnel CSV and optionally filters by role.
#'
#' @param role Character or NULL. Role(s) to filter on. If NULL, returns all participants.
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble of participants; filtered by `role` when provided.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_main_personnel_list <- function(role = NULL, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  path <- file.path(root_folder, "CSV", "Imports", "MainPersonnel.csv")
  participants <- tibble::as_tibble(utils::read.csv(path))

  if (is.null(role)) return(participants)
  if (is.list(role)) role <- unlist(role)

  dplyr::filter(participants, Role %in% role)
}

#' Get monthly Qualtrics log
#'
#' Read the monthly Qualtrics log CSV from the configured upload folder.
#'
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble with the monthly log contents.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_monthly_log <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  path <- file.path(root_folder, "CSV", "Exports", "QualtricsMonthlyLog.csv")
  tibble::as_tibble(utils::read.csv(path))
}

#' Read monthly question shortlist
#'
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble with the monthly question shortlist.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_monthly_question_shortlist <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  path <- file.path(root_folder, "CSV", "Imports", "MonthlyUpdateQuestion.csv")
  tibble::as_tibble(utils::read.csv(path))
}

#' Read weekly question shortlist
#'
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble with the weekly question shortlist.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_weekly_question_shortlist <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  path <- file.path(root_folder, "CSV", "Imports", "WeeklyUpdateQuestion.csv")
  tibble::as_tibble(utils::read.csv(path))
}

#' Get first SaveData value from weekly log
#'
#' @param logFile A data.frame or tibble representing the log.
#' @param originDate Value to match in `OriginDate` column.
#' @param neededAction Value to match in `Action` column.
#' @return The first `SaveData` value matching the filters, or `NULL` if none.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_first_save_data_from_weekly_log <- function(logFile, originDate, neededAction) {
  info <- dplyr::filter(logFile, OriginDate == originDate & Action == neededAction)
  saveData <- if (nrow(info) >= 1) dplyr::pull(dplyr::slice(info, 1), "SaveData") else NULL
  saveData
}

#' Alias for monthly log lookup
#'
#' Wrapper around `get_first_save_data_from_weekly_log` for monthly logs.
#'
#' @param logFile A data.frame or tibble representing the log.
#' @param originDate Value to match in `OriginDate` column.
#' @param neededAction Value to match in `Action` column.
#' @return The matched `SaveData` value or `NULL`.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_first_save_data_from_monthly_log <- function(logFile, originDate, neededAction) {
  get_first_save_data_from_weekly_log(logFile, originDate, neededAction)
}

#' Check if entry exists in log
#'
#' @param logFile A data.frame or tibble representing the log.
#' @param originDate Value to match in `OriginDate` column.
#' @param neededAction Value to match in `Action` column.
#' @return Logical; `TRUE` if at least one matching row exists.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
check_exist_in_log <- function(logFile, originDate, neededAction) {
  nrow(dplyr::filter(logFile, OriginDate == originDate & Action == neededAction)) > 0
}

#' Check if entry does not exist in log
#'
#' @inheritParams check_exist_in_log
#' @return Logical; `TRUE` if no matching row exists.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
check_not_exist_in_log <- function(logFile, originDate, neededAction) {
  nrow(dplyr::filter(logFile, OriginDate == originDate & Action == neededAction)) <= 0
}

#' Append a row to the weekly Qualtrics log
#'
#' @param originDate Value to write to `OriginDate` column.
#' @param neededAction Value to write to `Action` column.
#' @param saveData Value to write to `SaveData` column.
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return NULL, called for side effect.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
write_to_weekly_log <- function(originDate, neededAction, saveData, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  df <- data.frame(OriginDate = originDate, Action = neededAction, SaveData = saveData, stringsAsFactors = FALSE)
  utils::write.table(df,
                      file = file.path(root_folder, "CSV", "Exports", "QualtricsWeeklyLog.csv"),
                      sep = ",",
                      col.names = FALSE,
                      row.names = FALSE,
                      append = TRUE)
  invisible(NULL)
}

#' Append a row to the monthly Qualtrics log
#'
#' @inheritParams write_to_weekly_log
#' @return NULL, called for side effect.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
write_to_monthly_log <- function(originDate, neededAction, saveData, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  df <- data.frame(OriginDate = originDate, Action = neededAction, SaveData = saveData, stringsAsFactors = FALSE)
  utils::write.table(df,
                      file = file.path(root_folder, "CSV", "Exports", "QualtricsMonthlyLog.csv"),
                      sep = ",",
                      col.names = FALSE,
                      row.names = FALSE,
                      append = TRUE)
  invisible(NULL)
}

#' Append a row to the Qualtrics template update log
#'
#' @inheritParams write_to_weekly_log
#' @return NULL, called for side effect.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
write_to_weekly_template_update_log <- function(originDate, neededAction, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  df <- data.frame(OriginDate = originDate, Action = neededAction, stringsAsFactors = FALSE)
  utils::write.table(df,
                      file = file.path(root_folder, "CSV", "Exports", "QualtricsUpdateLog.csv"),
                      sep = ",",
                      col.names = FALSE,
                      row.names = FALSE,
                      append = TRUE)
  invisible(NULL)
}

#' Alias for weekly template update log writer
#'
#' @inheritParams write_to_weekly_log
#' @return NULL, called for side effect.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
write_to_monthly_template_update_log <- function(originDate, neededAction, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  write_to_weekly_template_update_log(originDate, neededAction, root_folder = root_folder)
}

#' Read unresolved monitor list
#'
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble with unresolved monitors (all columns read as character).
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_unresolved_monitor_log <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  path <- file.path(root_folder, "CSV", "Imports", "UnresolvedMonitor.csv")
  tibble::as_tibble(utils::read.csv(path, colClasses = "character"))
}

#' Consolidate unresolved monitors from survey data
#'
#' Examines `myData$Monitors` for indicators (asterisks) that signal unresolved
#' issues and appends corresponding rows to `unresolvedList` with a reason code.
#'
#' @param myData A list-like object with a `Monitors` data.frame/tibble.
#' @param unresolvedList A tibble to which new unresolved rows will be appended.
#' @return The updated `unresolvedList` tibble.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
concentrate_unresolved_monitor_qualtrics <- function(myData, unresolvedList) {
  current_date <- Sys.Date()
  start_of_current_month <- lubridate::floor_date(current_date, unit = "month")

  monitors <- myData$Monitors

  purpleTrackingSummary <- dplyr::filter(monitors, grepl("PA", Label))
  purpleTrackingSummary <- dplyr::rename(purpleTrackingSummary, `Tracking Needed` = PATQuestion1)

  purpleHealthSummary <- dplyr::filter(monitors, grepl("PA", Label))
  purpleHealthSummary <- dplyr::mutate(purpleHealthSummary,
                                      `Maintenance Needed` = dplyr::if_else(
                                        rowSums(dplyr::across(dplyr::starts_with("PAH"), ~ grepl("\\*", .))) > 0,
                                        "Needs follow-up (*)",
                                        "No"))
  purpleHealthSummary <- dplyr::select(purpleHealthSummary, DeviceID, `Maintenance Needed`)

  purpleDataSummary <- dplyr::filter(monitors, grepl("PA", Label))
  purpleDataSummary <- dplyr::mutate(purpleDataSummary,
                                     `Data Follow-up Needed` = dplyr::if_else(
                                       rowSums(dplyr::across(dplyr::starts_with("PAD"), ~ grepl("\\*", .))) > 0,
                                       "Needs follow-up (*)",
                                       "No"))
  purpleDataSummary <- dplyr::select(purpleDataSummary, DeviceID, `Data Follow-up Needed`)

  paMerger <- dplyr::full_join(purpleTrackingSummary, purpleHealthSummary, by = "DeviceID")
  paMerger <- dplyr::full_join(paMerger, purpleDataSummary, by = "DeviceID")
  paMerger <- dplyr::rename(paMerger, ID = DeviceID)

  clarityTrackingSummary <- dplyr::filter(monitors, grepl("CN", Label))
  clarityTrackingSummary <- dplyr::rename(clarityTrackingSummary, `Tracking Summary` = CTQuestion1)

  clarityHealthSummary <- dplyr::filter(monitors, grepl("CN", Label))
  clarityHealthSummary <- dplyr::mutate(clarityHealthSummary,
                                       `Maintenance Needed` = dplyr::if_else(
                                         rowSums(dplyr::across(dplyr::starts_with("CH"), ~ grepl("\\*", .))) > 0,
                                         "Needs follow-up (*)",
                                         "No"))
  clarityHealthSummary <- dplyr::select(clarityHealthSummary, DeviceID, `Maintenance Needed`)

  clarityDataSummary <- dplyr::filter(monitors, grepl("CN", Label))
  clarityDataSummary <- dplyr::mutate(clarityDataSummary,
                                     `Data Follow-up Needed` = dplyr::if_else(
                                       rowSums(dplyr::across(dplyr::starts_with("CD"), ~ grepl("\\*", .))) > 0,
                                       "Needs follow-up (*)",
                                       "No"))
  clarityDataSummary <- dplyr::select(clarityDataSummary, DeviceID, `Data Follow-up Needed`)

  clMerger <- dplyr::full_join(clarityTrackingSummary, clarityHealthSummary, by = "DeviceID")
  clMerger <- dplyr::full_join(clMerger, clarityDataSummary, by = "DeviceID")
  clMerger <- dplyr::rename(clMerger, ID = DeviceID)
  clMerger <- dplyr::rename(clMerger, `Tracking Needed` = `Tracking Summary`)

  add_unresolved_rows <- function(df, column_name, reason) {
    df_filtered <- dplyr::filter(df, grepl("\\*", .data[[column_name]]))
    if (nrow(df_filtered) == 0) return()

    new_rows <- data.frame(
      OriginDate = as.character(start_of_current_month),
      DeviceID = df_filtered$ID,
      SiteName = df_filtered$SiteName,
      Reason = reason,
      Resolved = as.character("No"),
      stringsAsFactors = FALSE
    )
    unresolvedList <- dplyr::bind_rows(unresolvedList, tibble::as_tibble(new_rows))
    unresolvedList
  }

  unresolvedList <- add_unresolved_rows(paMerger, "Tracking Needed", "TrackingFail")
  unresolvedList <- add_unresolved_rows(paMerger, "Maintenance Needed", "HealthFail")
  unresolvedList <- add_unresolved_rows(paMerger, "Data Follow-up Needed", "DataArchiveFail")

  unresolvedList <- add_unresolved_rows(clMerger, "Tracking Needed", "TrackingFail")
  unresolvedList <- add_unresolved_rows(clMerger, "Maintenance Needed", "HealthFail")
  unresolvedList <- add_unresolved_rows(clMerger, "Data Follow-up Needed", "DataArchiveFail")

  unresolvedList
}

#' Create mailing list and add contacts from a personnel list
#'
#' Uses existing `create_mailing_list()` and `add_mailing_contact()` helpers to
#' create a mailing list in Qualtrics and add the provided contacts. This is a
#' thin wrapper and assumes those helper functions handle API authentication.
#'
#' @param qualtrics_api_key Character. API key or token for Qualtrics access.
#' @param directoryID Character. Qualtrics directory ID where the mailing list will be created.
#' @param mailingListName Character. Name for the new mailing list.
#' @param participantList A tibble/data.frame with at least columns `FirstName`, `LastName`, and `Email`.
#' @return The mailing list id returned by `create_mailing_list()`.
#' @details The function performs a simple deduplication by `Email` before adding contacts.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_and_add_contact_from_personnel_list <- function(qualtrics_api_key, directoryID, mailingListName, participantList) {
  participantList <- dplyr::distinct(tibble::as_tibble(participantList), Email, .keep_all = TRUE)

  mailingId <- create_mailing_list(qualtrics_api_key, directoryID, mailingListName)

  purrr::pwalk(.l = list(participantList$FirstName, participantList$LastName, participantList$Email),
               .f = function(x, y, z) add_mailing_contact(qualtrics_api_key, directoryID, mailingId, x, y, z))

  # Wait briefly for Qualtrics to process the batch add; callers may prefer
  # to replace with a polling loop that confirms completion.
  Sys.sleep(30)

  mailingId
}

#' Read monitor sites from timeshift workbook
#'
#' Loads the "MonitorStatus" sheet from the monthly CAMN monitor tracking workbook
#' and performs basic filtering to return only active public sites with valid short codes.
#'
#' @param date Date. Date used to determine the monthly timeshift filename. Defaults to `Sys.Date()`.
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble with monitor site information.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_monitor_sites <- function(date = Sys.Date(), root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  date_suffix <- lubridate::floor_date(date, unit = "month")
  timeshift_filename <- sprintf("CAMNMonitorTracking_%s.xlsx", date_suffix)
  timeshift_file <- file.path(root_folder, "CSV", "QATimeshift", timeshift_filename)

  df <- readxl::read_xlsx(path = timeshift_file,
                          sheet = "MonitorStatus",
                          range = "A2:J100",
                          .name_repair = "unique_quiet")
  tib <- tibble::as_tibble(df)
  tib <- dplyr::rename(tib,
                       DeviceID = `API ID`,
                       OrgID = `Dashboard/API Organization ID`,
                       ShortCode = `Location short code`,
                       SiteName = `Deployed Site Location`)

  numCol <- ncol(tib)
  tib <- dplyr::filter(tib, rowSums(is.na(tib) | tib == "") < numCol)
  tib <- dplyr::filter(tib, nchar(ShortCode) >= 3)
  tib <- dplyr::filter(tib, grepl("public", `Data sharing setting`))
  tib
}

#' Read monthly question info CSV
#'
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A data.frame with the question info.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_monthly_question_info <- function(root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  path <- file.path(root_folder, "CSV", "Imports", "MonthlyUpdateQuestion.csv")
  utils::read.csv(path)
}

#' Merge personnel list with sensor/site information
#'
#' Joins weekly personnel (hosts) with monitor site information so personnel are
#' associated with sensors at their site. Optional `sensorType` filters the
#' returned rows.
#'
#' @param sensorType Character or NULL. Optional pattern to filter the `Type` column.
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @param date Date. Date used to determine the monthly timeshift filename. Defaults to `Sys.Date()`.
#' @return A tibble with merged personnel and site/sensor information.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_merge_personnel_sensor_list <- function(sensorType = NULL,
                                           root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER"),
                                           date = Sys.Date()) {
  # Explanation:
  # Each personnel have a site short code assigned to them
  # Each site can have multiple sensors.
  # This function merge these two aspect, so that the personnel
  #   can be sent multiple emails if needed
  # The function will return a tibble, with some rows info duplicated
  participants <- get_weekly_personnel_list(date = date, root_folder = root_folder)
  sitesInfo <- get_monitor_sites(date = date, root_folder = root_folder)

  merger <- dplyr::full_join(x = participants, y = sitesInfo, by = c("SiteShortCode" = "ShortCode"))
  if (!is.null(sensorType)) {
    dplyr::filter(merger, grepl(sensorType, Type))
  } else {
    merger
  }
}

#' Read processed weekly responses
#'
#' @param responseFileName Character. CSV filename within `CSV/Qualtrics/Weekly/`.
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble with processed responses.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_processed_responses_list <- function(responseFileName, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  path <- file.path(root_folder, "CSV", "Qualtrics", "Weekly", responseFileName)
  tibble::as_tibble(utils::read.csv(path))
}

#' Read and parse question descriptions for weekly surveys
#'
#' @param questionDescFileName Character. Filename in `CSV/Qualtrics/Weekly/`.
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble with separated `DeviceID`, `DeviceType`, and `Site` columns.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_question_descriptions <- function(questionDescFileName, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  path <- file.path(root_folder, "CSV", "Qualtrics", "Weekly", questionDescFileName)
  questionDescData <- tibble::as_tibble(utils::read.csv(path))
  tidyr::separate(questionDescData, sub, c("DeviceID", "DeviceType", "Site"), sep = ",", remove = TRUE)
}

#' Get personnel who did not respond (weekly)
#'
#' @param responseFileName Character. Filename in `CSV/Qualtrics/Weekly/` with responses.
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble of personnel (with sensor rows) who did not respond.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_unresponsed_personnel_list <- function(responseFileName, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  responses <- get_processed_responses_list(responseFileName, root_folder = root_folder)
  personnel_sensor_list <- get_merge_personnel_sensor_list(root_folder = root_folder) %>% dplyr::filter(!is.na(Email))
  dplyr::filter(personnel_sensor_list, !Email %in% responses$RecipientEmail)
}

#' Get personnel who responded (weekly)
#'
#' @inheritParams get_unresponsed_personnel_list
#' @return A tibble of personnel (with sensor rows) who responded.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_responsed_personnel_list <- function(responseFileName, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  responses <- get_processed_responses_list(responseFileName, root_folder = root_folder)
  personnel_sensor_list <- get_merge_personnel_sensor_list(root_folder = root_folder) %>% dplyr::filter(!is.na(Email))
  dplyr::filter(personnel_sensor_list, Email %in% responses$RecipientEmail)
}

#' Read processed monthly responses
#'
#' @param responseFileName Character. CSV filename within `CSV/Qualtrics/Monthly/`.
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble with processed monthly responses.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_monthly_responses_list <- function(responseFileName, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  path <- file.path(root_folder, "CSV", "Qualtrics", "Monthly", responseFileName)
  tibble::as_tibble(utils::read.csv(path))
}

#' Get analysts who did not respond (monthly)
#'
#' @param responseFileName Character. Filename in `CSV/Qualtrics/Monthly/` with responses.
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble of analysts who did not respond.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_unresponsed_analyst_list <- function(responseFileName, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  responses <- get_monthly_responses_list(responseFileName, root_folder = root_folder)
  personnel_sensor_list <- get_main_personnel_list(role = "Analyst", root_folder = root_folder) %>% dplyr::filter(!is.na(Email))
  dplyr::filter(personnel_sensor_list, !Email %in% responses$RecipientEmail)
}

#' Get analysts who responded (monthly)
#'
#' @inheritParams get_unresponsed_analyst_list
#' @return A tibble of analysts who responded.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_responsed_analyst_list <- function(responseFileName, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  responses <- get_monthly_responses_list(responseFileName, root_folder = root_folder)
  personnel_sensor_list <- get_main_personnel_list(role = "Analyst", root_folder = root_folder) %>% dplyr::filter(!is.na(Email))
  dplyr::filter(personnel_sensor_list, Email %in% responses$RecipientEmail)
}

#' Read and parse monthly question descriptions
#'
#' @param questionDescFileName Character. Filename in `CSV/Qualtrics/Monthly/`.
#' @param splitLikeUnresponsed Logical. If TRUE, parse `sub` column with a different pattern.
#' @param root_folder Character. Root folder path. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return A tibble with separated columns depending on `splitLikeUnresponsed`.
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_monthly_question_descriptions <- function(questionDescFileName, splitLikeUnresponsed = FALSE, root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  path <- file.path(root_folder, "CSV", "Qualtrics", "Monthly", questionDescFileName)
  questionDescData <- tibble::as_tibble(utils::read.csv(path))

  if (!splitLikeUnresponsed) {
    tidyr::separate(questionDescData, sub, c("DeviceID", "DeviceType", "Site"), sep = ",", remove = TRUE)
  } else {
    tidyr::separate(questionDescData, sub, c("ErrorDate", "DeviceID", "Site", "Reason"), sep = ", ", remove = TRUE)
  }
}
