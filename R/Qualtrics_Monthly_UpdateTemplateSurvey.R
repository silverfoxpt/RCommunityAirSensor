#' Copy the original QA Monitor tracking workbook for monthly QA
#'
#' Copies the `CAMNMonitorTracking.xlsx` workbook from a records location
#' into a QA-specific upload folder and appends the current month to the
#' filename. This function only performs file operations and does not
#' attempt to install or load packages; dependencies are namespace-qualified.
#'
#' @details
#' Performs minimal validation: checks that the source file exists and
#' creates the destination folder if necessary.
#'
#' @param base_file Character. Full path to the source workbook. Defaults to
#'   file.path(Sys.getenv("RECORDS_ROOT_FOLDER"), "CAMNMonitorTracking.xlsx").
#' @param new_folder Character. Destination folder where the dated copy will
#'   be placed. Defaults to file.path(Sys.getenv("UPLOAD_ROOT_FOLDER"),
#'   "CSV", "QATimeshift").
#' @param date Date. Date used to compute the month suffix. Defaults to
#'   Sys.Date().
#' @return NULL. Called for side effects (file copy).
#' @section Error handling:
#' Stops with informative messages when the source file does not exist or the
#' copy operation fails.
#' @examples
#' \dontrun{
#' copy_original_QAMonitorFile()
#' }
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
copy_original_QAMonitorFile <- function(
  base_file = file.path(Sys.getenv("RECORDS_ROOT_FOLDER"), "CAMNMonitorTracking.xlsx"),
  new_folder = file.path(Sys.getenv("UPLOAD_ROOT_FOLDER"), "CSV", "QATimeshift"),
  date = Sys.Date()
) {
  if (!file.exists(base_file)) {
    stop("Source file not found: ", base_file)
  }

  if (!dir.exists(new_folder)) {
    dir.create(new_folder, recursive = TRUE, showWarnings = FALSE)
  }

  date_suffix <- lubridate::floor_date(date, unit = "month")
  new_file_name <- sprintf("CAMNMonitorTracking_%s.xlsx", format(date_suffix, "%Y-%m-%d"))
  new_file_path <- file.path(new_folder, new_file_name)

  res <- tryCatch(
    {
      fs::file_copy(base_file, new_file_path, overwrite = TRUE)
      TRUE
    },
    error = function(e) {
      stop("Failed copying file: ", conditionMessage(e))
    }
  )

  invisible(res)
}

#' Update an unresolved-monitor matrix question in a Qualtrics survey
#'
#' Fetches the existing question payload, constructs rows from the unresolved
#' monitor log, and updates the question using the project's Qualtrics helper
#' utilities. This function keeps the existing answers and display logic by
#' default.
#'
#' @param qualtKey Character. Qualtrics API key. Default reads
#'   Sys.getenv("QUALTRICS_API_KEY").
#' @param surveyId Character. Qualtrics survey ID. Default reads
#'   Sys.getenv("QUALTRICS_MONTHLY_TEMPLATE_ID").
#' @param unresolveQuestionID Character. Qualtrics question ID to update (e.g.
#'   "QID66").
#' @return NULL. Called for side effects (API update).
#' @details
#' The function expects helper utilities to exist in the package namespace:
#' `get_single_qualtrics_question()`, `get_unresolved_monitor_log()`,
#' `custom_configure_matrix_question_qualtrics()` and `modify_qualtrics_question()`.
#' @examples
#' \dontrun{
#' update_list_unresolved_monitor_qualtrics("<KEY>", "<SURVEY>", "QID66")
#' }
#' @export
#' @concept role:qualtrics_monthly
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
update_list_unresolved_monitor_qualtrics <- function(
  qualtKey = Sys.getenv("QUALTRICS_API_KEY"),
  surveyId = Sys.getenv("QUALTRICS_MONTHLY_TEMPLATE_ID"),
  unresolveQuestionID
) {
  if (missing(unresolveQuestionID) || !nzchar(unresolveQuestionID)) {
    stop("unresolveQuestionID is required and must be a non-empty string")
  }

  unresolveQues <- get_single_qualtrics_question(qualtKey, surveyId, unresolveQuestionID)
  result <- unresolveQues$result

  questionRows <- get_unresolved_monitor_log() %>%
    dplyr::filter(Resolved == "No") %>%
    dplyr::mutate(Info = paste(OriginDate, DeviceID, SiteName, Reason, sep = ", ")) %>%
    dplyr::pull(Info)

  IdAndLogicNewInfo <- list(
    QuestionRows = questionRows,
    QuestionLogics = list()
  )

  modifiedResult <- custom_configure_matrix_question_qualtrics(
    result,
    IdAndLogicNewInfo[["QuestionRows"]],
    IdAndLogicNewInfo[["QuestionLogics"]],
    list("Yes", "No"),
    questionText = NA,
    changeAnswer = FALSE
  )

  modify_qualtrics_question(qualtKey, modifiedResult, surveyId, unresolveQuestionID)
  message("Updated question: Unresolved List - ", unresolveQuestionID)
  invisible(TRUE)
}

#' Update the Qualtrics monthly template survey
#'
#' High-level workflow to update matrix questions in the monthly Qualtrics
#' template survey. The function reads a shortlist of question IDs and sensor
#' types, updates matrix questions without altering display logic or answers,
#' updates unresolved monitor list questions, copies a QA backup of the
#' monitor-tracking workbook, and logs the update.
#'
#' @details
#' **Run:**
#' 1. **Validation**: Validates required parameters or reads them from the
#'    environment.
#' 2. **QA backup**: Copies the original monitor tracking workbook via
#'    `copy_original_QAMonitorFile()`.
#' 3. **Shortlist import**: Loads the monthly question shortlist using
#'    `get_monthly_question_shortlist()` (or accepts an override).
#' 4. **Question updates**: Updates matrix questions using
#'    `custom_update_matrix_question_qualtrics()` and unresolved-monitor
#'    questions using `update_list_unresolved_monitor_qualtrics()`.
#' 5. **Logging**: Writes an entry via `write_to_monthly_template_update_log()`.
#'
#' **Data processing details:**
#' - Expects `get_monthly_question_shortlist()` to return a tibble with
#'   columns `QuestionID` and `SensorType`.
#'
#' @param qualtKey Character. Qualtrics API key. Defaults to
#'   Sys.getenv("QUALTRICS_API_KEY").
#' @param surveyId Character. Qualtrics survey ID. Defaults to
#'   Sys.getenv("QUALTRICS_MONTHLY_TEMPLATE_ID").
#' @param questionShort Optional tibble. If provided, used instead of calling
#'   `get_monthly_question_shortlist()`.
#' @return NULL. Called for side effects (API updates and logging).
#' @section Error handling:
#' Stops with clear messages if required helper functions are missing or if
#' the question shortlist does not contain required columns.
#' @examples
#' \dontrun{
#' qualtrics_update_monthly_template_survey()
#' }
#' @seealso
#' \code{\link{copy_original_QAMonitorFile}},
#' \code{\link{update_list_unresolved_monitor_qualtrics}}
#' @export
#' @concept role:qualtrics_monthly
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
qualtrics_update_monthly_template_survey <- function(
  qualtKey = Sys.getenv("QUALTRICS_API_KEY"),
  surveyId = Sys.getenv("QUALTRICS_MONTHLY_TEMPLATE_ID"),
  questionShort = NULL
) {
  # QA backup
  copy_original_QAMonitorFile()

  if (is.null(questionShort)) {
    questionShort <- get_monthly_question_shortlist()
  }

  if (!all(c("QuestionID", "SensorType") %in% names(questionShort))) {
    stop("questionShort must contain columns: 'QuestionID' and 'SensorType'")
  }

  listID <- questionShort[["QuestionID"]]
  listType <- questionShort[["SensorType"]]

  # Update - preserve existing display logic and answers
  purrr::pwalk(
    .l = list(listID, listType),
    .f = function(x, y) {
      custom_update_matrix_question_qualtrics(
        qualtKey, surveyId, x, y,
        applyEmailLogic = FALSE,
        applyNewAnswer = FALSE,
        DEBUG = TRUE
      )
    }
  )

  update_list_unresolved_monitor_qualtrics(qualtKey, surveyId, "QID66")
  update_list_unresolved_monitor_qualtrics(qualtKey, surveyId, "QID67")

  write_to_monthly_template_update_log(Sys.Date(), "UpdateMonthlyTemplate")

  invisible(TRUE)
}
