
#' Send weekly Qualtrics survey to personnel mailing list
#'
#' This function prepares and sends the weekly Qualtrics survey created earlier
#' for the target week. It will:
#' - check the weekly log for an existing `CreateSurvey` entry for the week and
#'   skip if not present (survey not created yet)
#' - skip if a `SendSurvey` entry already exists for the week
#' - build a mailing list from the personnel list and distribute the survey
#'
#' @details
#' **Run:**
#' 1. **Validation**: Verifies required credentials are provided (or available via environment variables).
#' 2. **Log checks**: Ensures the survey was previously created and not already sent this week.
#' 3. **Participant retrieval**: Loads weekly personnel list (via `get_weekly_personnel_list()`) if not provided.
#' 4. **Mailing list creation**: Calls `create_and_add_contact_from_personnel_list()` to build contacts.
#' 5. **Distribution**: Calls `distribute_qualtrics_survey()` to send the message and logs the action.
#'
#' **Data processing details:**
#' - The function relies on package-local helper functions for reading/writing the weekly log and
#'   for constructing the personnel list. It does not itself persist copies of participant data.
#'
#' @param current_date Date used to determine the current week. Defaults to `Sys.Date()`.
#' @param qualtrics_api_key Qualtrics API key. Defaults to `Sys.getenv("QUALTRICS_API_KEY")`.
#' @param directory_id Qualtrics directory ID for contact lists. Defaults to `Sys.getenv("QUALTRICS_DIRECTORY_ID")`.
#' @param library_id Qualtrics library ID that contains the survey. Defaults to `Sys.getenv("QUALTRICS_LIBRARY_ID")`.
#' @param message_id Qualtrics message/template ID to use for distribution. Defaults to `Sys.getenv("QUALTRICS_MESSAGE_ID")`.
#' @param participants Optional participant data frame. If NULL, `get_weekly_personnel_list()` will be used.
#'
#' @return Invisibly returns the `surveyId` of the distributed survey when sent, otherwise `NULL` if skipped.
#'
#' @section Error handling:
#' - The function will stop with an informative error if required credentials are missing.
#' - If helper functions (log or personnel retrieval) return unexpected values, the function will stop.
#'
#' @examples
#' \dontrun{
#' # Use environment variables (recommended):
#' qualtrics_send_weekly_survey()
#'
#' # Or pass explicit credentials (useful in non-interactive contexts):
#' qualtrics_send_weekly_survey(
#'   qualtrics_api_key = "<REDACTED_API_KEY>",
#'   directory_id = "<REDACTED_DIRECTORY_ID>",
#'   library_id = "<REDACTED_LIBRARY_ID>",
#'   message_id = "<REDACTED_MESSAGE_ID>"
#' )
#' }
#'
#' @seealso
#' \code{\link{get_weekly_personnel_list}}, \code{\link{create_and_add_contact_from_personnel_list}},
#' \code{\link{distribute_qualtrics_survey}}, \code{\link{write_to_weekly_log}}
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
#' @concept addCheckSetupFolder:true
qualtrics_send_weekly_survey <- function(
  current_date = Sys.Date(),
  qualtrics_api_key = Sys.getenv("QUALTRICS_API_KEY"),
  directory_id = Sys.getenv("QUALTRICS_DIRECTORY_ID"),
  library_id = Sys.getenv("QUALTRICS_LIBRARY_ID"),
  message_id = Sys.getenv("QUALTRICS_MESSAGE_ID"),
  participants = NULL
) {
  # Basic validation of credentials
  if (is.null(qualtrics_api_key) || identical(qualtrics_api_key, "")) {
    stop("`qualtrics_api_key` must be provided via argument or QUALTRICS_API_KEY environment variable.")
  }
  if (is.null(directory_id) || identical(directory_id, "")) {
    stop("`directory_id` must be provided via argument or QUALTRICS_DIRECTORY_ID environment variable.")
  }
  if (is.null(library_id) || identical(library_id, "")) {
    stop("`library_id` must be provided via argument or QUALTRICS_LIBRARY_ID environment variable.")
  }
  if (is.null(message_id) || identical(message_id, "")) {
    stop("`message_id` must be provided via argument or QUALTRICS_MESSAGE_ID environment variable.")
  }

  # Determine the start of the current week (Monday by default for floor_date)
  start_of_current_week <- lubridate::floor_date(current_date, unit = "week")

  # Check logs to see whether prerequisites are present
  logfile <- get_weekly_log()

  # If the survey create entry does not exist for this week, do nothing
  if (check_not_exist_in_log(logfile, start_of_current_week, "CreateSurvey")) {
    return(invisible(NULL))
  }

  # If the survey was already sent this week, do nothing
  if (check_exist_in_log(logfile, start_of_current_week, "SendSurvey")) {
    return(invisible(NULL))
  }

  # retrieve survey id from logfile (from "CreateSurvey" tag)
  surveyId <- get_first_save_data_from_weekly_log(logfile, start_of_current_week, "CreateSurvey")

  # Get participants if not provided
  if (is.null(participants)) {
    participants <- get_weekly_personnel_list()
  }

  # Build mailing list and distribute
  mailingId <- create_and_add_contact_from_personnel_list(
    qualtrics_api_key,
    directory_id,
    paste("Weekly mailing list - ", start_of_current_week, sep = ""),
    participants
  )

  distribute_qualtrics_survey(
    qualtrics_api_key,
    library_id,
    message_id,
    mailingId,
    paste("Weekly monitor health check survey - week of ", start_of_current_week, sep = ""),
    surveyId,
    isMultiple = TRUE
  )

  # Save to weekly log
  write_to_weekly_log(start_of_current_week, "SendSurvey", surveyId)

  invisible(surveyId)
}
