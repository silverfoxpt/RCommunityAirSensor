#' Create a new Qualtrics survey
#'
#' Create a new survey resource in Qualtrics using the survey-definitions endpoint.
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param surveyName Character string with the desired survey name.
#' @param base_url Character string for Qualtrics base URL. Defaults to the USF datacenter.
#'
#' @return Parsed JSON response from Qualtrics as a list.
#'
#' @examples
#' \dontrun{
#' create_survey("<REDACTED_API_KEY>", "My Survey")
#' }
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_survey <- function(qualtrics_api_key, surveyName, base_url = "https://usf.az1.qualtrics.com") {
  body <- list(
    SurveyName = surveyName,
    Language = "EN",
    ProjectCategory = "CORE"
  )

  req <- glue::glue("{base_url}/API/v3/survey-definitions") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtrics_api_key) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(body)

  response <- httr2::req_perform(req)
  httr2::resp_body_json(response)
}

#' Get Qualtrics survey definition
#'
#' Retrieve the survey definition for a given survey ID.
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param surveyID Survey identifier.
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return Parsed JSON response as a list.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_qualtrics_survey <- function(qualtrics_api_key, surveyID, base_url = "https://usf.az1.qualtrics.com") {
  req <- glue::glue("{base_url}/API/v3/survey-definitions/{surveyID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtrics_api_key) %>%
    httr2::req_method("GET")

  response <- httr2::req_perform(req)
  httr2::resp_body_json(response)
}

#' Get all questions for a Qualtrics survey
#'
#' Retrieves all question elements for a survey definition.
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param surveyID Survey identifier.
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return Parsed JSON response as a list.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_all_survey_questions <- function(qualtrics_api_key, surveyID, base_url = "https://usf.az1.qualtrics.com") {
  req <- glue::glue("{base_url}/API/v3/survey-definitions/{surveyID}/questions") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtrics_api_key) %>%
    httr2::req_method("GET")

  response <- httr2::req_perform(req)
  httr2::resp_body_json(response)
}

#' Extract question text and export tag for a survey
#'
#' Returns a two-column tibble with `QuestionText` and `DataExportTag` for
#' all questions in the survey. HTML tags are removed from question text.
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param surveyID Survey identifier.
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return A tibble with columns `QuestionText` and `DataExportTag`.
#'
#' @examples
#' \dontrun{
#' get_question_info_qualtrics("<REDACTED_API_KEY>", "SV_XXXX")
#' }
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_question_info_qualtrics <- function(qualtrics_api_key, surveyID, base_url = "https://usf.az1.qualtrics.com") {
  questions <- get_all_survey_questions(qualtrics_api_key, surveyID, base_url = base_url)

  qData <- purrr::map(
    .x = questions$result$elements,
    .f = function(x) list(QuestionText = x[["QuestionText"]], DataExportTag = x[["DataExportTag"]])
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(QuestionText = stringr::str_remove_all(QuestionText, "<[^>]+>"))

  qData
}

#' Extract question IDs from a survey
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param surveyID Survey identifier.
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return A character vector of question IDs.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
extract_question_ids <- function(qualtrics_api_key, surveyID, base_url = "https://usf.az1.qualtrics.com") {
  questions <- get_all_survey_questions(qualtrics_api_key, surveyID, base_url = base_url)
  question_elements <- questions$result$elements

  purrr::map_chr(.x = question_elements, .f = function(x) x$QuestionID)
}

#' Delete a Qualtrics survey question
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param surveyID Survey identifier.
#' @param questionID Question identifier to delete.
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return Parsed response invisibly.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
delete_survey_question <- function(qualtrics_api_key, surveyID, questionID, base_url = "https://usf.az1.qualtrics.com") {
  req <- glue::glue("{base_url}/API/v3/survey-definitions/{surveyID}/questions/{questionID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtrics_api_key) %>%
    httr2::req_method("DELETE")

  response <- httr2::req_perform(req)
  invisible(httr2::resp_body_json(response))
}

#' Get a single Qualtrics question
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param surveyID Survey identifier.
#' @param questionID Question identifier.
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return Parsed JSON response as a list.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_single_qualtrics_question <- function(qualtrics_api_key, surveyID, questionID, base_url = "https://usf.az1.qualtrics.com") {
  req <- glue::glue("{base_url}/API/v3/survey-definitions/{surveyID}/questions/{questionID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtrics_api_key) %>%
    httr2::req_method("GET")

  response <- httr2::req_perform(req)
  httr2::resp_body_json(response)
}

#' Publish a Qualtrics survey version
#'
#' Marks a new survey version as published.
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param surveyID Survey identifier.
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return Parsed JSON response as a list.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
publish_survey <- function(qualtrics_api_key, surveyID, base_url = "https://usf.az1.qualtrics.com") {
  jsonBody <- list(Description = "Version", Published = TRUE)

  req <- glue::glue("{base_url}/API/v3/survey-definitions/{surveyID}/versions") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtrics_api_key) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(jsonBody)

  response <- httr2::req_perform(req)
  httr2::resp_body_json(response)
}

#' Distribute a Qualtrics survey via mailing list
#'
#' Schedule a distribution using mailing lists, message/library references and a
#' survey link. Times are computed relative to the current system time. The
#' `base_url` parameter allows using different data centers.
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param libraryID Library identifier for the message template.
#' @param messageID Message identifier inside the library.
#' @param mailingID Mailing list identifier to receive the distribution.
#' @param qualtricsEmailSubject Email subject base text.
#' @param surveyID Survey identifier to distribute.
#' @param isMultiple Logical indicating whether distribution is "Multiple" or "Individual".
#' @param from_name Sender display name (defaults to "Qualtrics").
#' @param from_email Sender email address used in the header.
#' @param reply_to_email Reply-to address used in the header.
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return Parsed JSON response from the distribution request.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
distribute_qualtrics_survey <- function(
  qualtrics_api_key,
  libraryID,
  messageID,
  mailingID,
  qualtricsEmailSubject,
  surveyID,
  isMultiple = FALSE,
  from_name = "Qualtrics",
  from_email = "noreply@qualtrics.com",
  reply_to_email = "noreply@qualtrics.com",
  base_url = "https://usf.az1.qualtrics.com"
) {
  header_subject <- paste0(qualtricsEmailSubject, " - month of ", strftime(Sys.time(), "%m-%Y", tz = "UTC"))

  body <- list(
    message = list(libraryId = libraryID, messageId = messageID),
    recipients = list(mailingListId = mailingID),
    header = list(fromName = from_name, replyToEmail = reply_to_email, fromEmail = from_email, subject = header_subject),
    surveyLink = list(
      surveyId = surveyID,
      expirationDate = strftime(Sys.time() + months(1), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") %>% as.character(),
      type = if (isMultiple) "Multiple" else "Individual"
    ),
    #safeguards
    sendDate = strftime(Sys.time() + lubridate::minutes(2), "%Y-%m-%dT%H:%M:%SZ", tz="UTC") %>% as.character()
  ) 
  
  req <- glue::glue("https://usf.az1.qualtrics.com/API/v3/distributions") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtrics_api_key) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(body)

  response <- httr2::req_perform(req)
  httr2::resp_body_json(response)
}

#' Create a new question in a Qualtrics survey
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param jsonBody A list representing the question payload (see Qualtrics API docs).
#' @param surveyID Survey identifier.
#' @param base_url Character string for Qualtrics base URL. Defaults to the IAD datacenter used historically.
#'
#' @return Parsed JSON response invisibly.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_new_qualtrics_question <- function(qualtrics_api_key, jsonBody, surveyID, base_url = "https://iad1.qualtrics.com") {
  req <- glue::glue("{base_url}/API/v3/survey-definitions/{surveyID}/questions") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtrics_api_key) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(jsonBody)

  response <- httr2::req_perform(req)
  invisible(httr2::resp_body_json(response))
}

#' Modify an existing Qualtrics question
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param jsonBody A list representing the updated question payload.
#' @param surveyID Survey identifier.
#' @param questionID Question identifier to modify.
#' @param base_url Character string for Qualtrics base URL. Defaults to IAD datacenter.
#'
#' @return Parsed JSON response invisibly.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
modify_qualtrics_question <- function(qualtrics_api_key, jsonBody, surveyID, questionID, base_url = "https://iad1.qualtrics.com") {
  req <- glue::glue("{base_url}/API/v3/survey-definitions/{surveyID}/questions/{questionID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtrics_api_key) %>%
    httr2::req_method("PUT") %>%
    httr2::req_body_json(jsonBody)

  response <- httr2::req_perform(req)
  invisible(httr2::resp_body_json(response))
}

#' Build a multiple-choice question body for the Qualtrics API
#'
#' Helper that assembles the JSON-like list structure for a multiple-choice
#' question payload.
#'
#' @param questionOptions Character vector of choice labels.
#' @param questionText Character string with the question prompt.
#' @param allowMultipleChoice Logical; if TRUE uses multiple-answer selector.
#'
#' @return A list suitable for `httr2::req_body_json()` when creating a question.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_multiple_choice_body <- function(questionOptions, questionText, allowMultipleChoice = FALSE) {
  Order <- as.character(seq_along(questionOptions))

  Choice <- setNames(purrr::map(questionOptions, function(x) list(Display = x)), Order)

  body <- list(
    ChoiceOrder = Order,
    Choices = Choice,
    Configuration = list(
      QuestionDescriptionOption = "UseText",
      TextPosition = "inline",
      ChoiceColumnWidth = 25,
      RepeatHeaders = "none",
      WhiteSpace = "ON",
      LabelPosition = "BELOW",
      NumColumns = 1,
      MobileFirst = TRUE
    ),
    Language = list(),
    QuestionDescription = "Descript",
    QuestionText = questionText,
    QuestionType = "MC",
    Selector = if (allowMultipleChoice) "MAVR" else "SAVR",
    SubSelector = "TX",
    Validation = list(Settings = list())
  )

  body
}

#' Build a text box question body
#'
#' @param questionText Character string with the question prompt.
#' @param allowMultipleLineTextbox Logical; if TRUE uses multi-line selector.
#'
#' @return A list suitable for Qualtrics question creation.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_text_box_body <- function(questionText, allowMultipleLineTextbox = FALSE) {
  body <- list(
    Configuration = list(
      QuestionDescriptionOption = "UseText",
      TextPosition = "inline",
      ChoiceColumnWidth = 25,
      RepeatHeaders = "none",
      WhiteSpace = "ON",
      LabelPosition = "BELOW",
      NumColumns = 1,
      MobileFirst = TRUE
    ),
    DataExportTag = "RANDOMDATAEXPORTTAG",
    DefaultChoices = FALSE,
    QuestionDescription = "Inspector name",
    QuestionText = questionText,
    QuestionType = "TE",
    Selector = if (allowMultipleLineTextbox) "ML" else "SL",
    Validation = list(Settings = list())
  )

  body
}

#' Build a text form question body
#'
#' @param questionOptions Character vector of labels for the form fields.
#' @param questionText Character string with the question prompt.
#'
#' @return A list suitable for Qualtrics question creation.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_text_form_body <- function(questionOptions, questionText) {
  Order <- as.character(seq_along(questionOptions))

  Choice <- setNames(purrr::map(questionOptions, function(x) list(Display = x)), Order)

  body <- list(
    ChoiceOrder = Order,
    Choices = Choice,
    Configuration = list(
      QuestionDescriptionOption = "UseText",
      TextPosition = "inline",
      ChoiceColumnWidth = 25,
      RepeatHeaders = "none",
      WhiteSpace = "ON",
      LabelPosition = "BELOW",
      NumColumns = 1,
      MobileFirst = TRUE
    ),
    DataExportTag = "RANDOMDATAEXPORTTAG2",
    DefaultChoices = FALSE,
    QuestionDescription = "Inspector name",
    QuestionText = questionText,
    QuestionType = "TE",
    Selector = "FORM",
    Validation = list(Settings = list())
  )

  body
}

#' Build a matrix question body
#'
#' @param questionStatements Character vector of row statements.
#' @param questionText Character string with the question prompt.
#' @param questionColumns Character vector of column labels.
#' @param allowMultipleAnswer Logical; if TRUE allows multiple selections per row.
#'
#' @return A list suitable for Qualtrics question creation.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_matrix_body <- function(questionStatements, questionText, questionColumns, allowMultipleAnswer = FALSE) {
  Order <- as.character(seq_along(questionColumns))

  Choice <- setNames(purrr::map(questionColumns, function(x) list(Display = x)), Order)

  StatementOrder <- as.character(seq_along(questionStatements))
  Statement <- setNames(purrr::map(questionStatements, function(x) list(Display = x)), StatementOrder)

  body <- list(
    ChoiceOrder = Order,
    Choices = Choice,
    AnswerOrder = StatementOrder,
    Answers = Statement,

    Configuration = list(
      QuestionDescriptionOption = "UseText",
      TextPosition = "inline",
      ChoiceColumnWidth = 25,
      RepeatHeaders = "none",
      WhiteSpace = "ON",
      LabelPosition = "BELOW",
      NumColumns = 1,
      MobileFirst = TRUE
    ),
    ChoiceDataExportTags = FALSE,
    DataExportTag = "RANDOMDATAEXPORTTAG",
    DefaultChoices = FALSE,
    Language = list(),
    QuestionDescription = "Inspector name",
    QuestionText = questionText,
    QuestionType = "Matrix",
    Selector = "Likert",
    SubSelector = if (allowMultipleAnswer) "MultipleAnswer" else "SingleAnswer",
    Validation = list(Settings = list())
  )

  body
}

#' Get survey QSF (full survey definition)
#'
#' Downloads the full QSF representation of a survey and replaces the SurveyName
#' entry with `newSurveyName` in the returned object (useful for cloning).
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param surveyID Survey identifier.
#' @param newSurveyName Character string to replace `SurveyEntry$SurveyName`.
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return A list representing the QSF (survey definition) with modified name.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_survey_qsf <- function(qualtrics_api_key, surveyID, newSurveyName, base_url = "https://usf.az1.qualtrics.com") {
  req <- glue::glue("{base_url}/API/v3/survey-definitions/{surveyID}") %>%
    httr2::request() %>%
    httr2::req_headers(
      "X-API-TOKEN" = qualtrics_api_key,
      "Content-Type" = "application/json"
    ) %>%
    httr2::req_method("GET") %>%
    httr2::req_url_query(format = "qsf")

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)

  qsf_copy_data <- resp_body$result
  qsf_copy_data$SurveyEntry$SurveyName <- newSurveyName

  qsf_copy_data
}

#' Create a survey from QSF data
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param qsfData A list representing the QSF payload (as returned by `get_survey_qsf`).
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return The new survey ID as a character string.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_survey_qsf <- function(qualtrics_api_key, qsfData, base_url = "https://usf.az1.qualtrics.com") {
  req <- glue::glue("{base_url}/API/v3/survey-definitions") %>%
    httr2::request() %>%
    httr2::req_headers(
      "X-API-TOKEN" = qualtrics_api_key,
      "Content-Type" = "application/json"
    ) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(qsfData)

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)
  resp_body$result$SurveyID
}

#' Add a contact to a Qualtrics mailing list
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param directoryID Directory identifier that contains the mailing list.
#' @param mailingListID Mailing list identifier.
#' @param firstname Contact first name.
#' @param lastname Contact last name.
#' @param mail Contact email address.
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return Parsed response invisibly.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
add_mailing_contact <- function(qualtrics_api_key, directoryID, mailingListID, firstname, lastname, mail, base_url = "https://usf.az1.qualtrics.com") {
  jsonBody <- list(firstName = firstname, lastName = lastname, email = mail)

  req <- glue::glue("{base_url}/API/v3/directories/{directoryID}/mailinglists/{mailingListID}/contacts") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtrics_api_key) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(jsonBody)

  response <- httr2::req_perform(req)
  invisible(httr2::resp_body_json(response))
}

#' Create a mailing list in a Qualtrics directory
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param directoryID Directory identifier.
#' @param listName Desired mailing list name.
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return The newly created mailing list id as a character string.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_mailing_list <- function(qualtrics_api_key, directoryID, listName, base_url = "https://usf.az1.qualtrics.com") {
  jsonBody <- list(name = listName)

  req <- glue::glue("{base_url}/API/v3/directories/{directoryID}/mailinglists") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtrics_api_key) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(jsonBody)

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)

  resp_body$result$id
}

#' Delete a mailing list
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param directoryID Directory identifier.
#' @param mailingID Mailing list identifier to delete.
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return Parsed response invisibly.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
delete_mailing_list <- function(qualtrics_api_key, directoryID, mailingID, base_url = "https://usf.az1.qualtrics.com") {
  req <- glue::glue("{base_url}/API/v3/directories/{directoryID}/mailinglists/{mailingID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtrics_api_key) %>%
    httr2::req_method("DELETE")

  response <- httr2::req_perform(req)
  invisible(httr2::resp_body_json(response))
}

#' Update a survey flow element
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param surveyID Survey identifier.
#' @param flowID Flow element identifier.
#' @param jsonBody A list payload for the flow element update.
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return Parsed response invisibly.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
update_survey_flow_element <- function(qualtrics_api_key, surveyID, flowID, jsonBody, base_url = "https://usf.az1.qualtrics.com") {
  req <- glue::glue("{base_url}/API/v3/survey-definitions/{surveyID}/flow/{flowID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtrics_api_key) %>%
    httr2::req_method("PUT") %>%
    httr2::req_body_json(jsonBody)

  response <- httr2::req_perform(req)
  invisible(httr2::resp_body_json(response))
}

#' Get survey flow
#'
#' Retrieve the flow object (survey structure) for a given survey.
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param surveyID Survey identifier.
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return Parsed JSON response as a list.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
get_survey_flow <- function(qualtrics_api_key, surveyID, base_url = "https://usf.az1.qualtrics.com") {
  req <- glue::glue("{base_url}/API/v3/survey-definitions/{surveyID}/flow") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtrics_api_key) %>%
    httr2::req_method("GET")

  response <- httr2::req_perform(req)
  httr2::resp_body_json(response)
}

#' Set a survey active
#'
#' Toggle a survey's `isActive` flag to TRUE.
#'
#' @param qualtrics_api_key API token for Qualtrics.
#' @param surveyID Survey identifier.
#' @param base_url Character string for Qualtrics base URL.
#'
#' @return Parsed response invisibly.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
set_qualtrics_survey_active <- function(qualtrics_api_key, surveyID, base_url = "https://usf.az1.qualtrics.com") {
  data <- list(isActive = TRUE)

  req <- glue::glue("{base_url}/API/v3/surveys/{surveyID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtrics_api_key) %>%
    httr2::req_method("PUT") %>%
    httr2::req_body_json(data)

  response <- httr2::req_perform(req)
  invisible(httr2::resp_body_json(response))
}
