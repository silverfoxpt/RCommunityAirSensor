
#' Update the Qualtrics weekly template survey
#'
#' Updates the weekly Qualtrics template survey by refreshing the configured
#' matrix questions for PurpleAir and Clarity sensor groups and writing a
#' completion entry to the weekly update log.
#'
#' @details
#' **Run:**
#' 1. **Validation**: Checks that the API key, survey ID, and question inputs are available.
#' 2. **Question updates**: Iterates through each question ID and sensor type pair.
#' 3. **API mutation**: Calls `custom_update_matrix_question_qualtrics()` for each pair.
#' 4. **Logging**: Records completion through `write_to_weekly_template_update_log()`.
#'
#' **Data processing details:**
#' - Uses six configured question IDs, grouped into three PurpleAir questions and three Clarity questions by default.
#' - Preserves the current question structure and delegates the Qualtrics update logic to the helper function.
#'
#' @param qualtrics_api_key Character. Qualtrics API key. Defaults to `Sys.getenv("QUALTRICS_API_KEY")`.
#' @param survey_id Character. Qualtrics weekly template survey ID. Defaults to `Sys.getenv("QUALTRICS_WEEKLY_TEMPLATE_ID")`.
#' @param question_ids Character vector of Qualtrics question IDs to update. Defaults to the weekly template question IDs from environment variables.
#' @param sensor_types Character vector of sensor types aligned with `question_ids`. Defaults to three `"PurpleAir"` values followed by three `"Clarity"` values.
#' @param log_date Date used when writing the completion log entry. Defaults to `Sys.Date()`.
#' @param action_label Character action label written to the log. Defaults to `"UpdateWeeklyTemplate"`.
#' @param log_root_folder Character root folder used for the update log. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @return NULL. Called for side effects (Qualtrics API updates and logging).
#' @section Error handling:
#' Stops with clear messages if the API key or survey ID is missing, if the question and sensor vectors are empty or different lengths, or if any question ID is missing.
#' @examples
#' \dontrun{
#' qualtrics_update_weekly_template_survey()
#' }
#' @seealso
#' \code{\link{custom_update_matrix_question_qualtrics}},
#' \code{\link{write_to_weekly_template_update_log}}
#' @export
#' @concept role:process
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
qualtrics_update_weekly_template_survey <- function(
  qualtrics_api_key = Sys.getenv("QUALTRICS_API_KEY"),
  survey_id = Sys.getenv("QUALTRICS_WEEKLY_TEMPLATE_ID"),
  question_ids = c(
    Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_PA_Q1_ID"),
    Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_PA_Q2_ID"),
    Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_PA_Q3_ID"),
    Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_CL_Q1_ID"),
    Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_CL_Q2_ID"),
    Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_CL_Q3_ID")
  ),
  sensor_types = c(rep("PurpleAir", 3), rep("Clarity", 3)),
  log_date = Sys.Date(),
  action_label = "UpdateWeeklyTemplate",
  log_root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")
) {
  question_ids <- as.character(question_ids)
  sensor_types <- as.character(sensor_types)

  if (!nzchar(qualtrics_api_key)) {
    stop("qualtrics_api_key is required and must be a non-empty string")
  }

  if (!nzchar(survey_id)) {
    stop("survey_id is required and must be a non-empty string")
  }

  if (length(question_ids) == 0L) {
    stop("question_ids must contain at least one Qualtrics question ID")
  }

  if (length(question_ids) != length(sensor_types)) {
    stop("question_ids and sensor_types must have the same length")
  }

  if (any(is.na(question_ids) | !nzchar(question_ids))) {
    stop("question_ids must not contain empty values")
  }

  if (any(is.na(sensor_types) | !nzchar(sensor_types))) {
    stop("sensor_types must not contain empty values")
  }

  purrr::walk2(
    .x = question_ids,
    .y = sensor_types,
    .f = function(question_id, sensor_type) {
      custom_update_matrix_question_qualtrics(
        qualtrics_api_key,
        survey_id,
        question_id,
        sensor_type
      )
    }
  )

  write_to_weekly_template_update_log(
    originDate = log_date,
    neededAction = action_label,
    root_folder = log_root_folder
  )

  invisible(NULL)
}
