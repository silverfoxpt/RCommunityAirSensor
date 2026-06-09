#' Create monthly QA survey in Qualtrics from a QSF template
#'
#' Creates a new monthly Quality Assurance survey in Qualtrics using a stored
#' QSF template, records the creation in the monthly log, activates the survey,
#' and publishes it. This is a high-level workflow intended to be called by
#' scheduled package tasks; it does not perform interactive package-level
#' dependency loading.
#'
#' @details
#' **Run:**
#' 1. **Validation**: Checks required API and template identifiers are provided
#' 2. **Log check**: Verifies whether a survey for the target month already
#'    exists in the monthly log to avoid duplicate creation
#' 3. **Template fetch**: Retrieves the QSF template from Qualtrics
#' 4. **Survey creation**: Creates a new survey from the template
#' 5. **Logging**: Writes the created survey ID to the monthly log
#' 6. **Activation & publish**: Activates and publishes the newly created survey
#'
#' **Notes:** This function assumes helper functions such as
#' `get_monthly_log()`, `check_exist_in_log()`, `get_survey_qsf()`,
#' `create_survey_qsf()`, `write_to_monthly_log()`, `set_qualtrics_survey_active()`,
#' and `publish_survey()` are available in the package.
#'
#' @param qualtrics_api_key Character. Qualtrics API key. Defaults to
#'   `Sys.getenv("QUALTRICS_API_KEY")`.
#' @param template_id Character. Qualtrics QSF template id. Defaults to
#'   `Sys.getenv("QUALTRICS_MONTHLY_TEMPLATE_ID")`.
#' @param directory_id Character. (Optional) Qualtrics directory id. Defaults
#'   to `Sys.getenv("QUALTRICS_DIRECTORY_ID")`.
#' @param library_id Character. (Optional) Qualtrics library id. Defaults to
#'   `Sys.getenv("QUALTRICS_LIBRARY_ID")`.
#' @param message_id Character. (Optional) Qualtrics message id. Defaults to
#'   `Sys.getenv("QUALTRICS_MESSAGE_ID")`.
#' @param current_date Date. Reference date to derive the target month. Defaults
#'   to `Sys.Date()`.
#'
#' @return Invisibly returns the new survey id (character) when a survey is
#' created. Returns NULL invisibly if a survey for the target month already
#' exists.
#'
#' @section Error handling:
#' The function stops with a clear error message if required parameters such as
#' `qualtrics_api_key` or `template_id` are missing or empty. It also assumes
#' that the helper functions mentioned above handle their own API errors and
#' raise informative errors on failure.
#'
#' @examples
#' \dontrun{
#' # Typical usage (requires QUALTRICS_* environment variables to be set):
#' qualtrics_create_monthly_survey()
#' }
#'
#' @seealso
#' \code{\link{get_survey_qsf}}, \code{\link{create_survey_qsf}}, \code{\link{publish_survey}}
#'
#' @export
#' @concept role:qualtrics_monthly
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
#' @concept addCheckSetupFolder:true
qualtrics_create_monthly_survey <- function(
  qualtrics_api_key = Sys.getenv("QUALTRICS_API_KEY"),
  template_id = Sys.getenv("QUALTRICS_MONTHLY_TEMPLATE_ID"),
  directory_id = Sys.getenv("QUALTRICS_DIRECTORY_ID"),
  library_id = Sys.getenv("QUALTRICS_LIBRARY_ID"),
  message_id = Sys.getenv("QUALTRICS_MESSAGE_ID"),
  current_date = Sys.Date()
) {
  # Validate required parameters
  if (is.null(qualtrics_api_key) || identical(qualtrics_api_key, "") || is.na(qualtrics_api_key)) {
    stop("`qualtrics_api_key` is required and was not provided. Set QUALTRICS_API_KEY or pass the value to the function.")
  }
  if (is.null(template_id) || identical(template_id, "") || is.na(template_id)) {
    stop("`template_id` is required and was not provided. Set QUALTRICS_MONTHLY_TEMPLATE_ID or pass the value to the function.")
  }

  # Derive start of the current month
  start_of_current_month <- lubridate::floor_date(as.Date(current_date), unit = "month")

  # Check existing monthly log to avoid duplicate creation
  logfile <- get_monthly_log()
  if (check_exist_in_log(logfile, start_of_current_month, "CreateSurvey")) {
    return(invisible(NULL))
  }

  # Fetch QSF template and create the survey
  qsf_name <- paste0("Monthly Community Sensor Data Quality Assurance survey - Month of ", start_of_current_month)
  qsfData <- get_survey_qsf(qualtrics_api_key, template_id, qsf_name)

  newSurveyId <- create_survey_qsf(qualtrics_api_key, qsfData)

  # Record creation in monthly log
  write_to_monthly_log(start_of_current_month, "CreateSurvey", newSurveyId)

  # Activate and publish
  set_qualtrics_survey_active(qualtrics_api_key, newSurveyId)
  publish_survey(qualtrics_api_key, newSurveyId)

  invisible(newSurveyId)
}

# Finish testing: 17 Mar 2025
