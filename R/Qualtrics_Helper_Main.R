# Qualtrics Survey functions ####
## Survey related ####
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

get_qualtrics_survey <- function(qualtricsKey, surveyID) {
  req <- glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
    httr2::req_method("GET")
  
  response <- httr2::req_perform(req) 
  resp_body <- httr2::resp_body_json(response) 
  return(resp_body)
}
## Question related ####
get_all_survey_questions <- function(qualtricsKey, surveyID) {
  req <- glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}/questions") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
    httr2::req_method("GET") 
  
  response <- httr2::req_perform(req) 
  resp_body <- httr2::resp_body_json(response) 
  return(resp_body)
}

get_question_info_qualtrics <- function(qualtricsKey, surveyID) {
  questions <- get_all_survey_questions(qualtricsKey,surveyID)
  qData <- questions$result$elements %>%
    purrr::map(.f = \(x) list(QuestionText = x[["QuestionText"]], DataExportTag = x[["DataExportTag"]])) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(QuestionText = str_remove_all(QuestionText, "<[^>]+>"))
  return(qData)
}

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

delete_survey_question <- function(qualtricsKey, surveyID, questionID) {
  req <- glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}/questions/{questionID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
    httr2::req_method("DELETE") 
  
  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response) 
}

# Get survey question
get_single_qualtrics_question <- function(qualtricsKey, surveyID, questionID) {
  req <- glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}/questions/{questionID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
    httr2::req_method("GET")
  
  response <- httr2::req_perform(req) 
  resp_body <- httr2::resp_body_json(response) #get response's body
  return(resp_body)
}

## Distribution related ####
publish_survey <- function(qualtricsKey, surveyID) {
  jsonBody <- list(
    Description = "Version",
    Published = TRUE
  )
  
  req <- glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}/versions") %>%
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
        expirationDate = strftime(Sys.time() + months(1), "%Y-%m-%dT%H:%M:%SZ", tz="UTC") %>% as.character(),
        type = if (isMultiple) "Multiple" else "Individual"
      ),
      #safeguards
      sendDate = strftime(Sys.time() + minutes(2), "%Y-%m-%dT%H:%M:%SZ", tz="UTC") %>% as.character()
    ) 
    
    req <- glue("https://usf.az1.qualtrics.com/API/v3/distributions") %>%
      httr2::request() %>%
      httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
      httr2::req_method("POST") %>%
      httr2::req_body_json(body) 
    
    response <- httr2::req_perform(req) # add , verbosity = 2 when need to debug!
    resp_body <- httr2::resp_body_json(response)
  }

# Question creation functions ####
create_new_qualtrics_question <- function(qualtricsKey, jsonBody, surveyID) {
  req <- glue("https://iad1.qualtrics.com/API/v3/survey-definitions/{surveyID}/questions") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
    httr2::req_method("POST") %>% 
    httr2::req_body_json(jsonBody) 
  
  response <- httr2::req_perform(req) 
  resp_body <- httr2::resp_body_json(response) #get response's body
}

modify_qualtrics_question <- function(qualtricsKey, jsonBody, surveyID, questionID) {
  req <- glue("https://iad1.qualtrics.com/API/v3/survey-definitions/{surveyID}/questions/{questionID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
    httr2::req_method("PUT") %>% 
    httr2::req_body_json(jsonBody) 
  
  response <- httr2::req_perform(req) 
  resp_body <- httr2::resp_body_json(response) #get response's body
}

create_multiple_choice_body <- function(questionOptions, questionText, allowMultipleChoice) {
  Order <- 
    purrr::map(.x = 1:length(questionOptions), 
               .f = as.character) %>%
    unlist() 
  
  Choice <-
    setNames(purrr::map(questionOptions, function(x) lst("Display" := !!x)), 
             Order) 
  
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
    #MAVR for multiple choices, vice versa
    Selector = if(allowMultipleChoice) "MAVR" else "SAVR", 
    SubSelector = "TX",
    Validation = list(
      Settings = list()
    )
  )
  return(body);
}

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

create_text_form_body <- function(questionOptions, questionText) {
  Order <- 
    purrr::map(.x = 1:length(questionOptions), 
               .f = as.character) %>%
    unlist() 
  
  Choice <-
    setNames(purrr::map(questionOptions, function(x) lst("Display" := !!x)), 
             Order) 
  
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
    Selector = "FORM", #FORM for text form
    Validation = list(
      Settings = list()
    )
  )
  return(body)
}

create_matrix_body <- function(questionStatements, questionText, questionColumns, allowMultipleAnswer) {
  Order <- 
    purrr::map(.x = 1:length(questionColumns), 
               .f = as.character) %>%
    unlist() 
  
  Choice <-
    setNames(purrr::map(questionColumns, function(x) lst("Display" := !!x)), 
             Order) 
  
  StatementOrder <- 
    purrr::map(.x = 1:length(questionStatements), 
               .f = as.character) %>%
    unlist()
  
  Statement <-
    setNames(purrr::map(questionStatements, function(x) lst("Display" := !!x)), 
             StatementOrder) 
  
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
get_survey_qsf <- function(qualtricsKey, surveyID, newSurveyName) {
  req <- glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}") %>%
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

create_survey_qsf <- function(qualtricsKey, qsfData) {
  req <- glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions") %>%
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
add_mailing_contact <- function(qualtricsKey, directoryID, mailingListID, firstname, lastname, mail) {
  jsonBody <- list(
    firstName = firstname,
    lastName = lastname,
    email = mail
  )
  
  req <- glue("https://usf.az1.qualtrics.com/API/v3/directories/{directoryID}/mailinglists/{mailingListID}/contacts") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
    httr2::req_method("POST") %>%
    httr2::req_body_json(jsonBody)
  
  response <- httr2::req_perform(req) 
  resp_body <- httr2::resp_body_json(response)
}

create_mailing_list <- function(qualtricsKey, directoryID, listName) {
  jsonBody <- list(
    name = listName
  )
  
  req <- glue("https://usf.az1.qualtrics.com/API/v3/directories/{directoryID}/mailinglists") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
    httr2::req_method("POST") %>%
    httr2::req_body_json(jsonBody)
  
  response <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(response)
  
  return(resp_body$result$id)
}

delete_mailing_list <- function(qualtricsKey, directoryID, mailingID) {
  req <- glue("https://usf.az1.qualtrics.com/API/v3/directories/{directoryID}/mailinglists/{mailingID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
    httr2::req_method("DELETE") 
  
  response <- httr2::req_perform(req) %>% print()
  resp_body <- httr2::resp_body_json(response) 
}

# Survey flow functions ####
update_survey_flow_element <- function(qualtricsKey, surveyID, flowID, jsonBody) {
  req <- glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}/flow/{flowID}") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
    httr2::req_method("PUT") %>%
    httr2::req_body_json(jsonBody)
  
  response <- httr2::req_perform(req)  #, verbosity = 2
  resp_body <- httr2::resp_body_json(response) 
}

get_survey_flow <- function(qualtricsKey, surveyID) {
  req <- glue("https://usf.az1.qualtrics.com/API/v3/survey-definitions/{surveyID}/flow") %>%
    httr2::request() %>%
    httr2::req_headers("X-API-TOKEN" = qualtricsKey) %>%   #set headers for API_KEY
    httr2::req_method("GET")
  
  response <- httr2::req_perform(req) 
  resp_body <- httr2::resp_body_json(response) 
  return(resp_body)
}

# Others ####
set_qualtrics_survey_active <- function(qualtricsKey, surveyID) {
  data <- list(
    isActive = TRUE
  )
  req <- glue("https://usf.az1.qualtrics.com/API/v3/surveys/{surveyID}") %>%
    httr2::request() %>%
    httr2::req_headers(
      "X-API-TOKEN" = qualtricsKey
    ) %>%   
    httr2::req_method("PUT") %>%
    httr2::req_body_json(data) 
  response <- httr2::req_perform(req) 
}
