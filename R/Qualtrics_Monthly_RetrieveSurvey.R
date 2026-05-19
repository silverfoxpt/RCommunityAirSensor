 #' Retrieve Monthly Survey Responses from Qualtrics API
 #'
 #' Download the latest monthly responses for a Qualtrics survey, keep only the
 #' most recent response per recipient, and save both the response table and a
 #' question description file to the specified upload folder.
 #'
 #' @details
 #' **Run:**
 #' 1. Validates required parameters and environment values
 #' 2. Determines the start of the current month from `current_date`
 #' 3. Looks up the survey id from the monthly log unless `surveyId` is provided
 #' 4. Retrieves survey responses via the `qualtRics` package
 #' 5. Keeps only the latest response per `RecipientEmail`
 #' 6. Writes response and question description CSVs to the upload folder
 #'
 #' **Data processing details:**
 #' - If multiple `RecipientEmail*` columns are present, extra columns are removed
 #'   and the first `RecipientEmail` column is used.
 #'
 #' @param upload_root_folder Character. Root folder where CSV files will be saved.
 #'   Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
 #' @param qualtrics_api_key Character. API key for Qualtrics. Defaults to
 #'   `Sys.getenv("QUALTRICS_API_KEY")`.
 #' @param surveyId Character or NULL. If provided, this survey id will be used
 #'   instead of looking it up from the monthly log.
 #' @param current_date Date. Date used to determine the target month. Defaults
 #'   to `Sys.Date()`.
 #'
 #' @return A named list with paths to the written files (invisibly). NULL is
 #'   returned invisibly on early exit (e.g. when survey has not been sent).
 #'
 #' @section Error handling:
 #' The function stops with a clear message if `upload_root_folder` or
 #' `qualtrics_api_key` is missing or empty. It assumes helper functions used
 #' below (e.g. `get_monthly_log`, `get_first_save_data_from_monthly_log`,
 #' `check_not_exist_in_log`, `write_to_monthly_log`) exist in
 #' the package and will error if they are not available.
 #'
 #' @examples
 #' \dontrun{
 #' qualtrics_retrieve_monthly_response(
 #'   upload_root_folder = "C:/project/uploads",
 #'   qualtrics_api_key = Sys.getenv("QUALTRICS_API_KEY")
 #' )
 #' }
 #'
 #' @seealso `get_monthly_log`, `write_to_monthly_log`
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
 qualtrics_retrieve_monthly_response <- function(
   upload_root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER"),
   qualtrics_api_key = Sys.getenv("QUALTRICS_API_KEY"),
   surveyId = NULL,
   current_date = Sys.Date()
 ) {
   # Basic validation
   if (is.null(upload_root_folder) || identical(upload_root_folder, "") ) {
     stop("`upload_root_folder` must be provided or set via UPLOAD_ROOT_FOLDER env var")
   }
   if (is.null(qualtrics_api_key) || identical(qualtrics_api_key, "")) {
     stop("`qualtrics_api_key` must be provided or set via QUALTRICS_API_KEY env var")
   }

   start_of_current_month <- lubridate::floor_date(current_date, unit = "month")

   # Check logs and workflow state using package helpers
   logfile <- get_monthly_log()

   # If the survey hasn't been sent, nothing to retrieve
   if (check_not_exist_in_log(logfile, start_of_current_month, "SendSurvey")) {
     return(invisible(NULL))
   }

   # Get survey id from log when not provided
   if (is.null(surveyId)) {
     surveyId <- get_first_save_data_from_monthly_log(logfile, start_of_current_month, "CreateSurvey")
   }

   # Set up qualtrics credentials for the request (no package install).
   qualtRics::qualtrics_api_credentials(api_key = qualtrics_api_key,
                                        base_url = "usf.az1.qualtrics.com",
                                        install = FALSE)

   # Fetch survey responses
   mySurveyResponse <- qualtRics::fetch_survey(surveyID = surveyId, verbose = FALSE)
   surveyQuestionDescription <- qualtRics::extract_colmap(mySurveyResponse)

   # Merge multiple RecipientEmail* columns if present
   recipientEmailColNames <- grep("^RecipientEmail", names(mySurveyResponse), value = TRUE)
   if (length(recipientEmailColNames) > 1) {
     mySurveyResponse <- mySurveyResponse %>%
       dplyr::select(-recipientEmailColNames[2:length(recipientEmailColNames)]) %>%
       dplyr::rename(RecipientEmail = !!rlang::sym(recipientEmailColNames[1]))
   }

   # Keep only the latest response for each RecipientEmail
   mySurveyResponse <- mySurveyResponse %>%
     dplyr::arrange(RecipientEmail, dplyr::desc(RecordedDate)) %>%
     dplyr::distinct(RecipientEmail, .keep_all = TRUE)

   # Prepare output paths
   mainFilename <- paste0("Qualtrics_Monthly_Response_monthOf_", start_of_current_month, ".csv")
   mainPath <- file.path(upload_root_folder, "CSV", "Qualtrics", "Monthly", mainFilename)
   dir.create(dirname(mainPath), recursive = TRUE, showWarnings = FALSE)
   utils::write.csv(mySurveyResponse, file = mainPath, row.names = FALSE)

   subFilename <- paste0("QuestionDescription_monthOf_", start_of_current_month, ".csv")
   subPath <- file.path(upload_root_folder, "CSV", "Qualtrics", "Monthly", subFilename)
   utils::write.csv(surveyQuestionDescription, file = subPath, row.names = FALSE)

   # Record actions in monthly log
   write_to_monthly_log(start_of_current_month, "RetrieveResponse", mainFilename)
   write_to_monthly_log(start_of_current_month, "RetrieveQuestionDesc", subFilename)

   invisible(list(main = mainPath, question_desc = subPath))
 }
