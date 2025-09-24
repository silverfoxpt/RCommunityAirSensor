copy_original_QAMonitorFile <- function() {
  # Define the file path and subfolder
  base_file <- file.path(Sys.getenv("BOX_RECORDS_ROOT_FOLDER"), "CAMNMonitorTracking.xlsx") 
  new_folder <- file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "QATimeshift")
  
  # Create the subfolder if it doesn't exist
  if (!dir.exists(new_folder)) {
    dir.create(new_folder, recursive = TRUE)
  }
  
  # Create a new file name with the current date
  date_suffix <- lubridate::floor_date(Sys.Date(), unit = "month")
  new_file_name <- sprintf("CAMNMonitorTracking_%s.xlsx", date_suffix)
  new_file_path <- file.path(new_folder, new_file_name)
  
  # Copy the file to the subfolder with the new name
  fs::file_copy(base_file, new_file_path, overwrite = TRUE)
}

update_list_unresolved_monitor_qualtrics <- function(qualtKey, surveyId, unresolveQuestionID) {
  # Update list of unresolved sensor - question
  unresolveQues <- get_single_qualtrics_question(qualtKey, surveyId, unresolveQuestionID)
  result <- unresolveQues$result
  
  questionRows <- get_unresolved_monitor_log() %>%
    dplyr::filter(Resolved == "No") %>%
    dplyr::mutate(Info = paste(OriginDate,DeviceID,SiteName,Reason, sep = ", ")) %>%
    dplyr::pull(Info)
  
  IdAndLogicNewInfo <- list(
    QuestionRows = questionRows,
    QuestionLogics = list() # No logic need this time!
  )
  
  modifiedResult <- custom_configure_matrix_question_qualtrics(
    result,
    IdAndLogicNewInfo[["QuestionRows"]],
    IdAndLogicNewInfo[["QuestionLogics"]],
    list("Yes", "No"), # TODO: Maybe custom configure this too for the future?
    questionText = NA,
    changeAnswer = FALSE
  )
  
  modify_qualtrics_question(qualtKey, modifiedResult, surveyId, unresolveQuestionID) # Strong typing!!!
  print(paste("Updated question: Unresolved List - ", unresolveQuestionID, sep = ""))
}

qualtrics_update_monthly_template_survey <- function() {
  # Installation and loading
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, glue, httr2, jsonlite, lubridate, qualtRics, readr, readxl)
  
  sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsUtil.R") 
  source(file = sourcePath, echo = FALSE)
  
  sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsSecondaryUtils.R") 
  source(file = sourcePath, echo = FALSE)
  
  sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsFlowNLogicUtils.R") 
  source(file = sourcePath, echo = FALSE)
  
  #readRenviron(file.path(getwd(), ".Rqualtrics"))
  copy_original_QAMonitorFile()
  
  # import env vars
  qualtKey <- Sys.getenv("QUALTRICS_API_KEY")
  surveyId <- Sys.getenv("QUALTRICS_MONTHLY_TEMPLATE_ID")
  
  # get question ids and sensor types from CSV
  questionShort <- get_monthly_question_shortlist()
  
  listID <- questionShort[["QuestionID"]]
  listType <- questionShort[["SensorType"]]
  
  # Update - no display logic, no changing answers
  purrr::pwalk(
    .l = list(listID, listType),
    .f = \(x, y) custom_update_matrix_question_qualtrics(qualtKey, surveyId, x, y, applyEmailLogic = FALSE, applyNewAnswer = FALSE, DEBUG = T)
  )
  
  update_list_unresolved_monitor_qualtrics(qualtKey, surveyId, "QID66")
  update_list_unresolved_monitor_qualtrics(qualtKey, surveyId, "QID67")
  
  # log to file - monthly
  write_to_monthly_template_update_log(Sys.Date(), "UpdateMonthlyTemplate")
}
qualtrics_update_monthly_template_survey()

# Finish testing: 17 Mar 2025
