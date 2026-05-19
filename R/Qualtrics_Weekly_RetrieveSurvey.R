
#' Retrieve latest Qualtrics weekly survey responses and save to CSV
#'
#' This function retrieves the most-recent response per recipient for a weekly
#' Qualtrics survey (identified from the weekly action log), saves the full
#' response CSV and a question description CSV to the specified upload folder,
#' and records the retrieval in the weekly action log.
#'
#' @details
#' **Run:**
#' 1. Validate parameters and upload folder
#' 2. Read the weekly action log and determine the week start
#' 3. Skip processing if the survey has not been sent that week
#' 4. Locate the survey ID from the weekly log ("CreateSurvey" entry)
#' 5. Fetch responses from Qualtrics using the survey ID
#' 6. Normalize RecipientEmail columns so only one remains
#' 7. Keep only the latest response per recipient
#' 8. Save responses and question descriptions to CSV
#' 9. Record actions in the weekly action log
#'
#' **Data processing details:**
#' - When multiple RecipientEmail columns exist, only the first is kept and
#'   the remaining RecipientEmail columns are removed.
#' - The function keeps the most recent RecordedDate per RecipientEmail.
#'
#' @param current_date Date used to determine the start of the week. Defaults to `Sys.Date()`.
#' @param upload_root_folder Character path to root upload folder. Defaults to the `UPLOAD_ROOT_FOLDER` environment variable.
#' @param qualtrics_api_key Character API key for Qualtrics. Defaults to the `QUALTRICS_API_KEY` environment variable.
#' @param qualtrics_base_url Character base URL for Qualtrics API. Defaults to "usf.az1.qualtrics.com".
#'
#' @return NULL (called for side effects: files written and weekly log updated).
#'
#' @section Error handling:
#' The function stops with an informative message if the `upload_root_folder` does
#' not exist, if the weekly log cannot be read, or if the survey id cannot be
#' located for the given week. The function does not attempt to set Qualtrics
#' credentials; credentials should be configured by the user (for example using
#' `qualtRics::qualtrics_api_credentials()` or by setting the appropriate
#' environment variables) before calling this function.
#'
#' @examples
#' \dontrun{
#' qualtrics_retrieve_weekly_response()
#' qualtrics_retrieve_weekly_response(current_date = as.Date("2025-01-20"),
#'                                    upload_root_folder = "C:/data/uploads")
#' }
#'
#' @seealso
#' \code{\link{get_weekly_log}}, \code{\link{get_first_save_data_from_weekly_log}}, \code{\link{write_to_weekly_log}}
#'
#' @export
#' @concept role:download
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
#' @concept addCheckSetupFolder:true
qualtrics_retrieve_weekly_response <- function(
  current_date = Sys.Date(),
  upload_root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER"),
  qualtrics_api_key = Sys.getenv("QUALTRICS_API_KEY"),
  qualtrics_base_url = "usf.az1.qualtrics.com"
) {
  # Validate inputs
  if (is.null(upload_root_folder) || identical(upload_root_folder, "")) {
    stop("upload_root_folder must be provided either via parameter or UPLOAD_ROOT_FOLDER environment variable")
  }
  if (!dir.exists(upload_root_folder)) {
    stop(sprintf("upload_root_folder does not exist: %s", upload_root_folder))
  }

  # determine week start
  start_of_current_week <- lubridate::floor_date(current_date, unit = "week")

  # Check if Log has already been collected
  logfile <- get_weekly_log()
  if (is.null(logfile)) {
    stop("Unable to read weekly log via get_weekly_log()")
  }

  # check if survey hasn't been sent
  if (check_not_exist_in_log(logfile, start_of_current_week, "SendSurvey")) {
    return(invisible(NULL))
  }

  # retrieve survey id from logfile, through "CreateSurvey" tag
  surveyId <- get_first_save_data_from_weekly_log(logfile, start_of_current_week, "CreateSurvey")
  if (is.null(surveyId) || identical(surveyId, "")) {
    stop("No survey id found in weekly log for the week starting ", as.character(start_of_current_week))
  }

  # NOTE: This function does not set Qualtrics credentials. The user should
  # configure credentials externally (e.g. via qualtRics::qualtrics_api_credentials()).
  # If api key/base url provided, set environment variables for qualtRics
  if (!is.null(qualtrics_api_key) && nzchar(qualtrics_api_key)) {
    Sys.setenv(QUALTRICS_API_KEY = qualtrics_api_key)
  }
  if (!is.null(qualtrics_base_url) && nzchar(qualtrics_base_url)) {
    Sys.setenv(QUALTRICS_BASE_URL = qualtrics_base_url)
  }

  # fetch survey
  mySurveyResponse <- qualtRics::fetch_survey(surveyID = surveyId, verbose = FALSE)
  surveyQuestionDescription <- qualtRics::extract_colmap(mySurveyResponse)

  # merge the RecipientEmail columns (there may be more than 1!)
  recipientEmailColNames <- grep("^RecipientEmail", names(mySurveyResponse), value = TRUE)
  if (length(recipientEmailColNames) > 1) {
    mySurveyResponse <- mySurveyResponse %>%
      dplyr::select(-recipientEmailColNames[2:length(recipientEmailColNames)]) %>% #delete the unneeded cols
      dplyr::rename(RecipientEmail := !!recipientEmailColNames[1])
  }

  # filter so that only LATEST response are kept for EACH email
  mySurveyResponse <- mySurveyResponse %>%
    dplyr::arrange(RecipientEmail, dplyr::desc(RecordedDate)) %>%
    dplyr::distinct(RecipientEmail, .keep_all = TRUE)

  # save to main CSV files - this will overwrite past write
  mainFilename <- paste0("Qualtrics_Weekly_Response_weekOf_", start_of_current_week, ".csv")
  write.csv(
    mySurveyResponse,
    file = file.path(upload_root_folder, "CSV", "Qualtrics", "Weekly", mainFilename)
  )

  # save subfile for question description
  subFilename <- paste0("QuestionDescription_weekOf_", start_of_current_week, ".csv")
  write.csv(
    surveyQuestionDescription,
    file = file.path(upload_root_folder, "CSV", "Qualtrics", "Weekly", subFilename)
  )

  # Save to log - Weekly Action Log
  write_to_weekly_log(start_of_current_week, "RetrieveResponse", mainFilename)
  write_to_weekly_log(start_of_current_week, "RetrieveQuestionDesc", subFilename)

  invisible(NULL)
}

