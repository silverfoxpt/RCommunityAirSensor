# IMPORTANT: ALL FILES IN THIS FOLDER ARE SUBJECTED TO CHANGE ONCE THE FOLDERS' PATHS CHANGE
# BE CAREFUL WHEN USING! ####

# Weekly CSV files' functions ####
get_weekly_log <- function() {
  logfile <- read.csv(file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Exports", "QualtricsWeeklyLog.csv")) %>%
    as_tibble()
  return(logfile)
}

get_update_log <- function() {
  logfile <- read.csv(file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Exports", "QualtricsUpdateLog.csv")) %>%
    as_tibble()
  return(logfile)
}

get_weekly_personnel_list <- function() {
  # Deduct full path
  date_suffix <- lubridate::floor_date(Sys.Date(), unit = "month")
  timeshift_filename <- sprintf("CAMNMonitorTracking_%s.xlsx", date_suffix)
  timeshift_file <- file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"),
                              "CSV", "QATimeshift",
                              timeshift_filename)

  # Read excel file
  readMonitorTracking <-
    readxl::read_xlsx(
      path = timeshift_file,
      sheet = "SitesAndHosts-InDevelopment",
      range = "A10:J26"
    ) %>%
    as_tibble() %>%
    dplyr::rename("Name" = "Host contact person") %>%
    dplyr::rename("Email" = "Email") %>%
    dplyr::rename("SiteShortCode" = "files, tracking sheet") %>%
    #dplyr::rename("SiteName" = "Standard Dashboard/map location name") %>%
    dplyr::select(Name, Email, SiteShortCode) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(FirstName = strsplit(Name, split = " ")[[1]][1]) %>%
    dplyr::mutate(LastName = strsplit(Name, split = " ")[[1]][2]) %>%
    dplyr::ungroup()

  readMonitorTracking <- readMonitorTracking %>%
    dplyr::filter(nchar(SiteShortCode) >= 3)

  return(readMonitorTracking)
}

get_main_personnel_list <- function(role = NULL) {
  participants <- read.csv(file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Imports", "MainPersonel.csv")) %>%
    as_tibble()

  if (is.null(role)) {
    return(participants)
  }

  # Convert list to vector if role is a list
  if (is.list(role)) {
    role <- unlist(role)
  }

  # Filter participants based on the role
  return(participants %>% dplyr::filter(Role %in% role))
}

# Monthly CSV files' functions ####
get_monthly_log <- function() {
  logfile <- read.csv(file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Exports", "QualtricsMonthlyLog.csv")) %>%
    as_tibble()
  return(logfile)
}
#tmp <- get_monthly_log()

get_monthly_question_shortlist <- function() {
  logfile <- read.csv(file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Imports", "MonthlyUpdateQuestion.csv")) %>%
    as_tibble()
  return(logfile)
}
#tmp <- get_monthly_question_shortlist()

get_weekly_question_shortlist <- function() {
  logfile <- read.csv(file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Imports", "WeeklyUpdateQuestion.csv")) %>%
    as_tibble()
  return(logfile)
}
#tmp <- get_weekly_question_shortlist()

get_first_save_data_from_weekly_log <- function(logFile, originDate, neededAction) {
  info      <- logFile %>% dplyr::filter(OriginDate == originDate & Action == neededAction)
  saveData  <- info %>% dplyr::slice(1) %>% dplyr::pull("SaveData")
  return(saveData)
}

get_first_save_data_from_monthly_log <- function(logFile, originDate, neededAction) {
  return(get_first_save_data_from_weekly_log(logFile, originDate, neededAction)) #nice
}

check_exist_in_log <- function(logFile, originDate, neededAction) {
  if (logFile %>% dplyr::filter(OriginDate == originDate & Action == neededAction) %>% nrow > 0) {
    return(TRUE)
  }
  return(FALSE)
}

check_not_exist_in_log <- function(logFile, originDate, neededAction) {
  if (logFile %>% dplyr::filter(OriginDate == originDate & Action == neededAction) %>% nrow <= 0) {
    return(TRUE)
  }
  return(FALSE)
}

write_to_weekly_log <- function(originDate, neededAction, saveData) {
  write.table(
    data.frame(
      OriginDate = c(originDate),
      Action = c(neededAction),
      SaveData = c(saveData)
    ),
    file = file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Exports", "QualtricsWeeklyLog.csv"),
    sep = ",",
    col.names = FALSE,
    row.names = FALSE,
    append = TRUE
  )
}

write_to_monthly_log <- function(originDate, neededAction, saveData) {
  write.table(
    data.frame(
      OriginDate = c(originDate),
      Action = c(neededAction),
      SaveData = c(saveData)
    ),
    file = file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Exports", "QualtricsMonthlyLog.csv"),
    sep = ",",
    col.names = FALSE,
    row.names = FALSE,
    append = TRUE
  )
}

write_to_weekly_template_update_log <- function(originDate, neededAction) {
  write.table(
    data.frame(
      OriginDate = c(originDate),
      Action = c(neededAction)
    ),
    file = file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Exports", "QualtricsUpdateLog.csv"),
    sep = ",",
    col.names = FALSE,
    row.names = FALSE,
    append = TRUE
  )
}

write_to_monthly_template_update_log <- function(originDate, neededAction) {
  write_to_weekly_template_update_log(originDate, neededAction)
}

## For unresolved monitor only ####
get_unresolved_monitor_log <- function() {
  logfile <- read.csv(file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Imports", "UnresolvedMonitor.csv"),
                      colClasses = "character") %>%
    as_tibble()
  return(logfile)
}
#tmp2 <- get_unresolved_monitor_log()

concentrate_unresolved_monitor_qualtrics <- function(myData, unresolvedList) {
  current_date <- Sys.Date()
  start_of_current_month <- floor_date(current_date, unit = "month")

  # For purpleAir
  purpleTrackingSummary <- myData$Monitors %>%
    dplyr::filter(grepl("PA", Label)) %>%
    #dplyr::select(DeviceID, PATQuestion3) %>%
    dplyr::rename(`Tracking Needed` = PATQuestion1)

  purpleHealthSummary <- myData$Monitors %>%
    dplyr::filter(grepl("PA", Label)) %>%
    dplyr::mutate(`Maintenance Needed` = if_else(
      rowSums(across(starts_with("PAH"), ~ grepl("\\*", .))) > 0,
      "Needs follow-up (*)",
      "No"
    )) %>%
    dplyr::select(DeviceID, `Maintenance Needed`)

  purpleDataSummary <- myData$Monitors %>%
    dplyr::filter(grepl("PA", Label)) %>%
    dplyr::mutate(`Data Follow-up Needed` = if_else(
      rowSums(across(starts_with("PAD"), ~ grepl("\\*", .))) > 0,
      "Needs follow-up (*)",
      "No"
    )) %>%
    dplyr::select(DeviceID, `Data Follow-up Needed`)

  paMerger <- purpleTrackingSummary %>%
    dplyr::full_join(purpleHealthSummary, by = "DeviceID") %>%
    dplyr::full_join(purpleDataSummary, by = "DeviceID") %>%
    dplyr::rename(ID = DeviceID)

  # For Clarity
  clarityTrackingSummary <- myData$Monitors %>%
    dplyr::filter(grepl("CN", Label)) %>%
    #dplyr::select(DeviceID, CTQuestion3) %>%
    dplyr::rename(`Tracking Summary` = CTQuestion1)

  clarityHealthSummary <- myData$Monitors %>%
    dplyr::filter(grepl("CN", Label)) %>%
    dplyr::mutate(`Maintenance Needed` = if_else(
      rowSums(across(starts_with("CH"), ~ grepl("\\*", .))) > 0,
      "Needs follow-up (*)",
      "No"
    )) %>%
    dplyr::select(DeviceID, `Maintenance Needed`)

  clarityDataSummary <- myData$Monitors %>%
    dplyr::filter(grepl("CN", Label)) %>%
    dplyr::mutate(`Data Follow-up Needed` = if_else(
      rowSums(across(starts_with("CD"), ~ grepl("\\*", .))) > 0,
      "Needs follow-up (*)",
      "No"
    )) %>%
    dplyr::select(DeviceID, `Data Follow-up Needed`)

  clMerger <- clarityTrackingSummary %>%
    dplyr::full_join(clarityHealthSummary, by = "DeviceID") %>%
    dplyr::full_join(clarityDataSummary, by = "DeviceID") %>%
    dplyr::rename(ID = DeviceID) %>%
    dplyr::rename(`Tracking Needed` = `Tracking Summary`) # Based on Dr. Stuart modifications

  # Create a helper function to filter and add rows
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

  # Use the helper function for each condition
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
create_and_add_contact_from_personnel_list <- function(qualtricsKey, directoryID, mailingListName, participantList) {
  # Filter by email
  participantList <- participantList %>% distinct(Email, .keep_all = TRUE)

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
get_monitor_sites <- function() {
  # Deduct full path
  date_suffix <- lubridate::floor_date(Sys.Date(), unit = "month")
  timeshift_filename <- sprintf("CAMNMonitorTracking_%s.xlsx", date_suffix)
  timeshift_file <- file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"),
                              "CSV", "QATimeshift",
                              timeshift_filename)

  # Read excel file
  readMonitorTracking <-
    readxl::read_xlsx(
      path = timeshift_file,
      sheet = "MonitorStatus",
      range = "A10:J100"
    ) %>%
    as_tibble() %>%
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

get_monthly_question_info <- function() {
  questionInfo <- file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Imports", "MonthlyUpdateQuestion.csv") %>%
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
get_processed_responses_list <- function(responseFileName) {
  fullresData <-
    read.csv(file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Qualtrics", "Weekly", responseFileName)) %>%
    as_tibble()

  return(fullresData)
}

# Updated: 20 Jan 2025
get_question_descriptions <- function(questionDescFileName) {
  # get desc
  questionDescData <-
    read.csv(file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Qualtrics", "Weekly", questionDescFileName)) %>%
    as_tibble()

  # split the sub-question into Device ID, Device Type and Site
  questionDescData <- questionDescData %>%
    tidyr::separate(sub, c("DeviceID", "DeviceType", "Site"), sep = ",", remove = T)

  return(questionDescData)
}

# Updated: 1/7/2024
get_unresponsed_personnel_list <- function(responseFileName) {
  # get responses
  responses <- get_processed_responses_list(responseFileName)

  # get personnel list
  personnel_sensor_list <-
    get_merge_personnel_sensor_list() %>%
    dplyr::filter(! is.na(Email)) # if no email assigned to sensor, ignore

  unresponsed_personnel <- personnel_sensor_list %>%
    dplyr::filter(! Email %in% responses$RecipientEmail) # filter by email

  return(unresponsed_personnel)
}

# Updated: 1/7/2024
get_responsed_personnel_list <- function(responseFileName) {
  # get responses
  responses <- get_processed_responses_list(responseFileName)

  # get personnel list
  personnel_sensor_list <-
    get_merge_personnel_sensor_list() %>%
    dplyr::filter(! is.na(Email)) # if no email assigned to sensor, ignore

  responsed_personnel <- personnel_sensor_list %>%
    dplyr::filter(Email %in% responses$RecipientEmail) # filter by email

  return(responsed_personnel)
}

## For monthly personnel ####
get_monthly_responses_list <- function(responseFileName) {
  fullresData <-
    read.csv(file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Qualtrics", "Monthly", responseFileName)) %>%
    as_tibble()

  return(fullresData)
}

get_unresponsed_analyst_list <- function(responseFileName) {
  # get responses
  responses <- get_monthly_responses_list(responseFileName)

  # get Analyst personnel list
  personnel_sensor_list <-
    get_main_personnel_list(role = "Analyst") %>%
    dplyr::filter(! is.na(Email)) # if no email assigned to sensor, ignore

  unresponsed_personnel <- personnel_sensor_list %>%
    dplyr::filter(! Email %in% responses$RecipientEmail) # filter by email

  return(unresponsed_personnel)
}
#tmp <- get_unresponsed_analyst_list("Qualtrics_Monthly_Response_monthOf_2024-08-01.csv")

get_responsed_analyst_list <- function(responseFileName) {
  # get responses
  responses <- get_monthly_responses_list(responseFileName)

  # get Analyst personnel list
  personnel_sensor_list <-
    get_main_personnel_list(role = "Analyst") %>%
    dplyr::filter(! is.na(Email)) # if no email assigned to sensor, ignore

  responsed_personnel <- personnel_sensor_list %>%
    dplyr::filter(Email %in% responses$RecipientEmail) # filter by email

  return(responsed_personnel)
}

get_monthly_question_descriptions <- function(questionDescFileName, splitLikeUnresponsed = F) {
  # get desc
  questionDescData <-
    read.csv(file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Qualtrics", "Monthly", questionDescFileName)) %>%
    as_tibble()

  # split the sub-question into Device ID, Device Type and Site
  if (!splitLikeUnresponsed) {
    questionDescData <- questionDescData %>%
      tidyr::separate(sub, c("DeviceID", "DeviceType", "Site"), sep = ",", remove = T)
  }
  else {
    questionDescData <- questionDescData %>%
      tidyr::separate(sub, c("ErrorDate", "DeviceID", "Site", "Reason"), sep = ", ", remove = T)
  }
  return(questionDescData)
}
