qualtrics_retrieve_weekly_response <- function() {
  # Installation and loading
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, glue, httr2, jsonlite, lubridate, qualtRics)
  
  sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsUtil.R") 
  source(file = sourcePath, echo = FALSE)
  
  sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsSecondaryUtils.R") 
  source(file = sourcePath, echo = FALSE)
  
  # get date of this week
  current_date <- Sys.Date()
  start_of_current_week <- floor_date(current_date, unit = "week") 
  
  # Check if Log has already been collected
  logfile <- get_weekly_log()
  
  # check if survey hasn't been sent
  if (check_not_exist_in_log(logfile, start_of_current_week, "SendSurvey")) {
    return()
  }
  
  # get info
  qualtKey <- Sys.getenv("QUALTRICS_API_KEY")
  
  # retrieve survey id from logfile, through "CreateSurvey" tag
  surveyId <- get_first_save_data_from_weekly_log(logfile, start_of_current_week, "CreateSurvey")
  
  # Retrieve responses
  # login with qualtRics 
  qualtrics_api_credentials(api_key = qualtKey, 
                            base_url = "usf.az1.qualtrics.com",
                            install = TRUE, overwrite = TRUE)
  readRenviron("~/.Renviron")
  
  # fetch survey
  mySurveyResponse <- fetch_survey(surveyID = surveyId, verbose = FALSE)
  surveyQuestionDescription <- extract_colmap(mySurveyResponse)
  
  # merge the RecipientEmail columns (there may be more than 1!)
  recipientEmailColNames <- grep("^RecipientEmail", names(mySurveyResponse), value = TRUE)
  if (length(recipientEmailColNames) > 1) {
    mySurveyResponse <- mySurveyResponse %>%
      dplyr::select(-recipientEmailColNames[2:length(recipientEmailColNames)]) %>% #delete the unneeded cols
      dplyr::rename(RecipientEmail := !!recipientEmailColNames[1])
  }
  
  # filter so that only LATEST response are kept for EACH email
  mySurveyResponse <- mySurveyResponse %>%
    dplyr::arrange(RecipientEmail, desc(RecordedDate)) %>%
    dplyr::distinct(RecipientEmail, .keep_all = TRUE)

  # save to main CSV files - this will OVERWRITE past write
  mainFilename <- paste("Qualtrics_Weekly_Response_weekOf_", start_of_current_week, ".csv", sep = "")
  write.csv(
    mySurveyResponse,
    file = file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"),
                     "CSV", "Qualtrics", "Weekly", mainFilename)
  )
  
  # save subfile for question description
  subFilename <- paste("QuestionDescription_weekOf_", start_of_current_week, ".csv", sep = "")
  write.csv(
    surveyQuestionDescription,
    file = file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"),
                     "CSV", "Qualtrics", "Weekly", subFilename)
  )

  # Save to log - Weekly Action Log
  write_to_weekly_log(start_of_current_week, "RetrieveResponse", mainFilename)
  write_to_weekly_log(start_of_current_week, "RetrieveQuestionDesc", subFilename)
}
qualtrics_retrieve_weekly_response()

# Tested: 20 Jan 2025

