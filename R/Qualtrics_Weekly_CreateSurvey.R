 #' Create Weekly Qualtrics Survey from Template
 #'
 #' Creates a new Qualtrics survey from a stored QSF template for the current week.
 #'
 #' @details
 #' **Run:**
 #' 1. Validate required parameters and API key
 #' 2. Determine the start date of the current week
 #' 3. Check the weekly log to avoid duplicate survey creation
 #' 4. Retrieve the QSF template and create a new survey
 #' 5. Record creation in the weekly log and activate/publish the survey
 #'
 #' @param current_date Date object used to determine the start of the week. Defaults to Sys.Date().
 #' @param qualtrics_api_key Character API key for Qualtrics. Defaults to Sys.getenv("QUALTRICS_API_KEY").
 #' @param template_id Character ID of the QSF template. Defaults to Sys.getenv("QUALTRICS_WEEKLY_TEMPLATE_ID").
 #' @param directory_id Character directory id for Qualtrics library. Defaults to Sys.getenv("QUALTRICS_DIRECTORY_ID").
 #' @param library_id Character library id for Qualtrics. Defaults to Sys.getenv("QUALTRICS_LIBRARY_ID").
 #' @param message_id Character message id used for mailing (optional). Defaults to Sys.getenv("QUALTRICS_MESSAGE_ID").
 #'
 #' @return Invisibly returns the new survey_id (character) if created, or NULL if no action was taken.
 #'
 #' @section Error handling:
 #' The function stops with a clear message when required parameters or API key are missing. If the survey already exists for the target week,
 #' the function returns invisibly with NULL to avoid duplicate creation.
 #'
 #' @examples
 #' \dontrun{
 #' qualtrics_create_weekly_survey()
 #' qualtrics_create_weekly_survey(current_date = as.Date("2025-01-20"))
 #' }
 #'
 #' @seealso
 #' \code{get_weekly_log}, \code{get_survey_qsf}, \code{create_survey_qsf}, \code{write_to_weekly_log}
 #'
 #' @export
 #' @concept role:report
 #' @concept removedDependencies:true
 #' @concept removedRawFunctionCalls:true
 #' @concept removedSensitiveInfo:true
 #' @concept cleanupParameters:true
 #' @concept cleanupComments:true
 #' @concept cleanupDependenciesNamespace:true
 #' @concept addRoxygenComments:true
 qualtrics_create_weekly_survey <- function(
   current_date = Sys.Date(),
   qualtrics_api_key = Sys.getenv("QUALTRICS_API_KEY"),
   template_id = Sys.getenv("QUALTRICS_WEEKLY_TEMPLATE_ID"),
   directory_id = Sys.getenv("QUALTRICS_DIRECTORY_ID"),
   library_id = Sys.getenv("QUALTRICS_LIBRARY_ID"),
   message_id = Sys.getenv("QUALTRICS_MESSAGE_ID")
 ) {
   # Validate required inputs
   if (is.null(qualtrics_api_key) || identical(qualtrics_api_key, "")) {
     stop("qualtrics_api_key is required. Set QUALTRICS_API_KEY or pass qualtrics_api_key.")
   }
   if (is.null(template_id) || identical(template_id, "")) {
     stop("template_id is required. Set QUALTRICS_WEEKLY_TEMPLATE_ID or pass template_id.")
   }

   # compute start of current week (uses lubridate)
   start_of_current_week <- lubridate::floor_date(current_date, unit = "week")

   # Retrieve weekly log and check for existing entry
   logfile <- get_weekly_log()
   if (check_exist_in_log(logfile, start_of_current_week, "CreateSurvey")) {
     return(invisible(NULL))
   }

   # Retrieve QSF template and create survey
   qsfData <- get_survey_qsf(
     qualtrics_api_key,
     template_id,
     paste("Weekly Community Sensor Health Check survey - Week of ", start_of_current_week, sep = "")
   )

   newSurveyId <- create_survey_qsf(
     qualtrics_api_key,
     qsfData
   )

   # Record creation and activate/publish
   write_to_weekly_log(start_of_current_week, 'CreateSurvey', newSurveyId)
   set_qualtrics_survey_active(qualtrics_api_key, newSurveyId)
   publishInfo <- publish_survey(qualtrics_api_key, newSurveyId)

   invisible(newSurveyId)
 }
