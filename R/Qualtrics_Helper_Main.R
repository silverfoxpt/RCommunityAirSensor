# Qualtrics Survey functions ####
## Survey related ####
#' Create a new Qualtrics survey
#'
#' Wrapper for the Qualtrics API to create a new survey definition.
#'
#' @param qualtricsKey Character API token.
#' @param surveyName Character name for the new survey.
#' @return Parsed JSON response from the Qualtrics API as a list.
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_survey <- function(qualtricsKey, surveyName) {
  body <- list(
    SurveyName = surveyName,
    Language = "EN",
    ProjectCategory = "CORE"
  )
  
  req <- httr2::request("https://usf.az1.qualtrics.com/API/v3/survey-definitions") %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
    httr2::req_method("POST") %>% 
    httr2::req_body_json(body) 
  
  response <- httr2::req_perform(req) 
  resp_body <- httr2::resp_body_json(response) 
  return(resp_body)
}

#' Get Qualtrics survey definition
#'
#' @param qualtricsKey Character API token.
#' @param surveyID Character survey id.
#' @return Parsed JSON response as a list.
#' @concept role:helper
get_qualtrics_survey <- function(qualtricsKey, surveyID) {
  req <- glue::glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%
    httr2::req_method("GET")

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)
  return(resp_body)
}
## Question related ####
#' Get all questions for a Qualtrics survey
#'
#' @param qualtricsKey Character API token.
#' @param surveyID Character survey id.
#' @return Parsed JSON response as a list.
#' @concept role:helper
get_all_survey_questions <- function(qualtricsKey, surveyID) {
  req <- glue::glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}/questions") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%
    httr2::req_method("GET")

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)
  return(resp_body)
}

#' Extract question text and export tags
#'
#' Returns a tibble with `QuestionText` and `DataExportTag` for each question
#' in the provided survey.
#'
#' @param qualtricsKey Character API token.
#' @param surveyID Character survey id.
#' @return A tibble with `QuestionText` and `DataExportTag`.
#' @concept role:helper
get_question_info_qualtrics <- function(qualtricsKey, surveyID) {
  questions <- get_all_survey_questions(qualtricsKey, surveyID)
  qData <- questions$result$elements %>%
    purrr::map(.f = \(x) list(QuestionText = x[["QuestionText"]], DataExportTag = x[["DataExportTag"]])) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(QuestionText = stringr::str_remove_all(QuestionText, "<[^>]+>"))
  return(qData)
}

#' Extract question IDs from survey definition
#'
#' @param qualtricsKey Character API token.
#' @param surveyID Character survey id.
#' @return Character vector of question IDs.
#' @concept role:helper
extract_question_ids <- function(qualtricsKey, surveyID) {
  questions <- get_all_survey_questions(qualtricsKey, surveyID)
  question_elements <- questions$result$elements

  return(
    purrr::map(
      .x = question_elements,
      .f = function(x) x$QuestionID
    )
  )
}

#' Delete a survey question
#'
#' @param qualtricsKey Character API token.
#' @param surveyID Character survey id.
#' @param questionID Character question id to delete.
#' @return Parsed response from API (invisible).
#' @concept role:helper
delete_survey_question <- function(qualtricsKey, surveyID, questionID) {
  req <- glue::glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}/questions/{questionID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%
    httr2::req_method("DELETE")

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)
  invisible(resp_body)
}

# Get survey question
#' Get a single question from a survey
#'
#' @param qualtricsKey Character API token.
#' @param surveyID Character survey id.
#' @param questionID Character question id.
#' @return Parsed JSON response as a list.
#' @concept role:helper
get_single_qualtrics_question <- function(qualtricsKey, surveyID, questionID) {
  req <- glue::glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}/questions/{questionID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%
    httr2::req_method("GET")

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)
  return(resp_body)
}

## Distribution related ####
#' Publish a survey version
#'
#' @param qualtricsKey Character API token.
#' @param surveyID Character survey id.
#' @return Parsed API response as a list.
#' @concept role:helper
publish_survey <- function(qualtricsKey, surveyID) {
  jsonBody <- list(
    Description = "Version",
    Published = TRUE
  )
  
  req <- glue::glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}/versions") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
    httr2::req_method("POST") %>%
    httr2::req_body_json(jsonBody)
  
  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response) #get response's body
  return(resp_body)
}
# Anonymous link as needed: https://usf.az1.qualtrics.com/jfe/form/{surveyID}

# Distribute survey
#' Distribute a survey via mailing list
#'
#' Sends a distribution request to Qualtrics. Time and subject defaults use
#' the current system time; these are preserved for backward compatibility.
#'
#' @param qualtricsKey Character API token.
#' @param libraryID Character library id.
#' @param messageID Character message id.
#' @param mailingID Character mailing list id.
#' @param qualtricsEmailSubject Character email subject base.
#' @param surveyID Character survey id.
#' @param isMultiple Logical whether distribution type is 'Multiple'.
#' @return Parsed API response as a list.
#' @concept role:helper
distribute_qualtrics_survey <- 
  function(qualtricsKey, libraryID, messageID, mailingID, qualtricsEmailSubject, surveyID, isMultiple) {
    body <- list(
      message = list(
        libraryId = libraryID,
        messageId = messageID
      ),
      recipients = list(
        mailingListId = mailingID
      ),
      header = list(
        fromName = "Qualtrics",
        replyToEmail = "noreply@qualtrics.com",
        fromEmail = "noreply@qualtrics.com",
        subject = 
          paste(qualtricsEmailSubject, " - month of ", 
                strftime(Sys.time(), "%m-%Y", tz="UTC") %>% as.character(), 
                sep = '')
      ),
      surveyLink = list(
        surveyId = surveyID,
        expirationDate = strftime(Sys.time() + lubridate::months(1), "%Y-%m-%dT%H:%M:%SZ", tz="UTC") %>% as.character(),
        type = if (isMultiple) "Multiple" else "Individual"
      ),
      #safeguards
      sendDate = strftime(Sys.time() + lubridate::minutes(2), "%Y-%m-%dT%H:%M:%SZ", tz="UTC") %>% as.character()
    ) 
    
    req <- glue::glue("https://usf.az1.qualtrics.com/API/v3/distributions") %>%
      httr2::request() %>%
      httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
      httr2::req_method("POST") %>%
      httr2::req_body_json(body) 
    
    response <- httr2::req_perform(req) # add , verbosity = 2 when need to debug!
    resp_body <- httr2::resp_body_json(response)
  }

# Question creation functions ####
#' Create a new question in a survey
#'
#' @param qualtricsKey Character API token.
#' @param jsonBody List body for the question JSON.
#' @param surveyID Character survey id.
#' @return Parsed API response as a list.
#' @concept role:helper
create_new_qualtrics_question <- function(qualtricsKey, jsonBody, surveyID) {
  req <- glue::glue("https://iad1.qualtrics.com/API/v3/survey-definitions/{surveyID}/questions") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(jsonBody)

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)
  invisible(resp_body)
}

#' Modify an existing question
#'
#' @param qualtricsKey Character API token.
#' @param jsonBody List body for the question JSON.
#' @param surveyID Character survey id.
#' @param questionID Character question id.
#' @return Parsed API response as a list.
#' @concept role:helper
modify_qualtrics_question <- function(qualtricsKey, jsonBody, surveyID, questionID) {
  req <- glue::glue("https://iad1.qualtrics.com/API/v3/survey-definitions/{surveyID}/questions/{questionID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%
    httr2::req_method("PUT") %>%
    httr2::req_body_json(jsonBody)

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)
  invisible(resp_body)
}

#' Create body for multiple choice question
#'
#' @param questionOptions Character vector of choices.
#' @param questionText Character question text.
#' @param allowMultipleChoice Logical whether multiple choices are allowed.
#' @return A list ready to be serialized to JSON for the Qualtrics API.
#' @concept role:helper
create_multiple_choice_body <- function(questionOptions, questionText, allowMultipleChoice) {
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
    Validation = list(
      Settings = list()
    )
  )
  return(body)
}

#' Create body for a text box question
#'
#' @param questionText Character question text.
#' @param allowMultipleLineTextbox Logical whether multi-line input is allowed.
#' @return A list ready for the Qualtrics API.
#' @concept role:helper
create_text_box_body <- function(questionText, allowMultipleLineTextbox) {
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
    #SL for single line, ML for multiple line
    Selector = if (allowMultipleLineTextbox) "ML" else "SL", 
    Validation = list(
      Settings = list()
    )
  )
  return(body)
}

#' Create body for a text form question
#'
#' @param questionOptions Character vector for form fields.
#' @param questionText Character question text.
#' @return A list ready for the Qualtrics API.
#' @concept role:helper
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
    Validation = list(
      Settings = list()
    )
  )
  return(body)
}

#' Create body for a matrix question
#'
#' @param questionStatements Character vector of statements (rows).
#' @param questionText Character question text.
#' @param questionColumns Character vector of column choices.
#' @param allowMultipleAnswer Logical whether multiple answers per row allowed.
#' @return A list ready for the Qualtrics API.
#' @concept role:helper
create_matrix_body <- function(questionStatements, questionText, questionColumns, allowMultipleAnswer) {
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
    Validation = list(
      Settings = list()
    )
  )
  return(body)
}

# Subtopic - Creation through qsf file ####
#' Get QSF (full survey JSON) for a survey
#'
#' @param qualtricsKey Character API token.
#' @param surveyID Character survey id.
#' @param newSurveyName Character new survey name to assign to the copy data.
#' @return The `result` field of the QSF response as a list.
#' @concept role:helper
get_survey_qsf <- function(qualtricsKey, surveyID, newSurveyName) {
  req <- glue::glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}") %>%
    httr2::request() %>%
    httr2::req_headers(
      "X-API-TOKEN" = qualtricsKey,
      "Content-Type" = "application/json"
    ) %>%
    httr2::req_method("GET") %>%
    httr2::req_url_query(format = "qsf")

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)

  qsf_copy_data <- resp_body$result
  qsf_copy_data$SurveyEntry$SurveyName <- newSurveyName

  return(qsf_copy_data)
}

#' Create a new survey from QSF data
#'
#' @param qualtricsKey Character API token.
#' @param qsfData List representing the QSF JSON.
#' @return New survey ID as character.
#' @concept role:helper
create_survey_qsf <- function(qualtricsKey, qsfData) {
  req <- glue::glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions") %>%
    httr2::request() %>%
    httr2::req_headers(
      "X-API-TOKEN" = qualtricsKey,
      "Content-Type" = "application/json"
    ) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(qsfData)

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)
  newSurveyId <- resp_body$result$SurveyID

  return(newSurveyId)
}

# Mailing list functions ####
#' Add a contact to a mailing list
#'
#' @param qualtricsKey Character API token.
#' @param directoryID Character directory id.
#' @param mailingListID Character mailing list id.
#' @param firstname Character first name.
#' @param lastname Character last name.
#' @param mail Character email address.
#' @return Parsed API response as a list (invisible).
#' @concept role:helper
add_mailing_contact <- function(qualtricsKey, directoryID, mailingListID, firstname, lastname, mail) {
  jsonBody <- list(
    firstName = firstname,
    lastName = lastname,
    email = mail
  )
  
  req <- glue::glue("https://usf.az1.qualtrics.com/API/v3/directories/{directoryID}/mailinglists/{mailingListID}/contacts") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
    httr2::req_method("POST") %>%
    httr2::req_body_json(jsonBody)
  
  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)
  invisible(resp_body)
}

#' Create a mailing list
#'
#' @param qualtricsKey Character API token.
#' @param directoryID Character directory id.
#' @param listName Character list name.
#' @return Mailing list id (character).
#' @concept role:helper
create_mailing_list <- function(qualtricsKey, directoryID, listName) {
  jsonBody <- list(
    name = listName
  )
  
  req <- glue::glue("https://usf.az1.qualtrics.com/API/v3/directories/{directoryID}/mailinglists") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
    httr2::req_method("POST") %>%
    httr2::req_body_json(jsonBody)
  
  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)

  return(resp_body$result$id)
}

#' Delete a mailing list
#'
#' @param qualtricsKey Character API token.
#' @param directoryID Character directory id.
#' @param mailingID Character mailing list id.
#' @return Parsed API response as a list (invisible).
#' @concept role:helper
delete_mailing_list <- function(qualtricsKey, directoryID, mailingID) {
  req <- glue::glue("https://usf.az1.qualtrics.com/API/v3/directories/{directoryID}/mailinglists/{mailingID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%
    httr2::req_method("DELETE")

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)
  invisible(resp_body)
}

# Survey flow functions ####
#' Update a survey flow element
#'
#' @param qualtricsKey Character API token.
#' @param surveyID Character survey id.
#' @param flowID Character flow id.
#' @param jsonBody List body for the flow element.
#' @return Parsed API response as a list (invisible).
#' @concept role:helper
update_survey_flow_element <- function(qualtricsKey, surveyID, flowID, jsonBody) {
  req <- glue::glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}/flow/{flowID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%
    httr2::req_method("PUT") %>%
    httr2::req_body_json(jsonBody)

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)
  invisible(resp_body)
}

#' Get survey flow
#'
#' @param qualtricsKey Character API token.
#' @param surveyID Character survey id.
#' @return Parsed API response as a list.
#' @concept role:helper
get_survey_flow <- function(qualtricsKey, surveyID) {
  req <- glue::glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}/flow") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%
    httr2::req_method("GET")

  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)
  return(resp_body)
}

# Others ####
#' Set survey active flag
#'
#' @param qualtricsKey Character API token.
#' @param surveyID Character survey id.
#' @return Parsed API response as a list (invisible).
#' @concept role:helper
set_qualtrics_survey_active <- function(qualtricsKey, surveyID) {
  data <- list(
    isActive = TRUE
  )
  req <- glue::glue("https://usf.az1.qualtrics.com/API/v3/surveys/{surveyID}") %>%
    httr2::request() %>%
    httr2::req_headers(
      "X-API-TOKEN" = qualtricsKey
    ) %>%   
    httr2::req_method("PUT") %>%
    httr2::req_body_json(data) 
  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)
  invisible(resp_body)
}
