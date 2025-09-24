# Question Display Logics ####
create_display_logic_qualtrics <- function(logicType, leftOperand, operator, rightOperand, eqType) {
  res <- list(
    "0" = list(
      "0" = list(
        LogicType = logicType,
        LeftOperand = leftOperand,
        Operator = operator,
        RightOperand = rightOperand,
        Type = eqType
      ),
      Type = "If"
    ),
    Type = "BooleanExpression",
    inPage = FALSE
  )
  return(res)
}

create_email_logic_qualtrics <- function(emailAddress) {
  if (is.na(emailAddress)) {
    emailAddress <- "0"
  }

  # shortcut to create display logic for emails
  return(create_display_logic_qualtrics("EmbeddedField", "RecipientEmail", "EqualTo", emailAddress, "Expression"))
}

# Helper function for generating email logic based on weekly personnel list
generate_ID_and_email_logics_lists_qualtrics <- function(sensorType = NULL, getEmailLogics = TRUE) {
  merger <- get_merge_personnel_sensor_list(sensorType) %>%
    dplyr::mutate(QuestionRows = paste(DeviceID, Type, SiteName, sep = ", "))

  res <- list(
    QuestionRows = merger[["QuestionRows"]],
    QuestionLogics = if (getEmailLogics) purrr::map(.x = merger[["Email"]], .f = create_email_logic_qualtrics) else list()
  )
  return(res)
}

# Survey flow Display Logics ####
# single conditions
create_flow_logic_qualtrics <- function(logicType, leftOperand, operator, rightOperand, eqType) {
  res <- list(
    "0" = list(
      "0" = list(
        LogicType = logicType,
        LeftOperand = leftOperand,
        Operator = operator,
        RightOperand = rightOperand,
        Type = eqType
      ),
      Type = "If"
    ),
    Type = "BooleanExpression"
  )
  return(res)
}

create_flow_email_logic_qualtrics <- function(emailAddress) {
  if (is.na(emailAddress)) {
    emailAddress <- "0"
  }

  # shortcut to create display logic for emails
  return(create_flow_logic_qualtrics("EmbeddedField", "RecipientEmail", "EqualTo", emailAddress, "Expression"))
}


# multiple conditions
# TODO: Please, someone, clean this up
create_multiple_flow_logic_qualtrics <- function(logicTypes, leftOperands, operators, rightOperands, eqTypes, conjunctions) {
  # add NA at start to first logic's conjunction
  conjunctions <- c(NA, conjunctions)

  # function to generate sub-list of logic
  create_logic_sub_list <- function(logicType, leftOperand, operator, rightOperand, eqType, conjunction) {
    sublist <- list(
      LogicType = logicType,
      LeftOperand = leftOperand,
      Operator = operator,
      RightOperand = rightOperand,
      Type = eqType
    )
    if (!is.na(conjunction)) {
      sublist[["Conjunction"]] = conjunction
    }
    return(sublist)
  }

  # create the list of logics
  listLogics <-
    purrr::pmap(
      .l = list(logicTypes, leftOperands, operators, rightOperands, eqTypes, conjunctions),
      .f = \(x, y, z, m, n, p) create_logic_sub_list(x, y, z, m, n, p)
    )

  # set names and type
  names(listLogics) <- map_chr(0:(length(logicTypes) - 1), as.character)
  listLogics[["Type"]] <- "If"

  # create final list
  res <- list(
    "0" = listLogics,
    Type = "BooleanExpression"
  )
  return(res)
}

create_multiple_flow_email_logic_qualtrics <- function(emailAddresses) {
  # if an email is NA, set it to "0"
  emailAddresses <-
    purrr::map(
      .x = emailAddresses,
      .f = \(x) {if (is.na(x)) "0" else x}
    )

  # shortcut to create display logic for emails
  return(create_multiple_flow_logic_qualtrics(
    rep("EmbeddedField", length(emailAddresses)),
    rep("RecipientEmail", length(emailAddresses)),
    rep("EqualTo", length(emailAddresses)),
    emailAddresses,
    rep("Expression", length(emailAddresses)),
    rep("Or", length(emailAddresses)-1)
  ))
}

create_branch_flow_body_qualtrics <- function(description, flowID, logic, subflow = list()) {
  return(list(
    Type = "Branch",
    FlowID = flowID,
    Description = description,
    BranchLogic = logic,
    Flow = subflow
  ))
}

create_block_flow_body_qualtrics <- function(blockID, flowID) {
  return(list(
    Type = "Block",
    ID = blockID,
    FlowID = flowID
  ))
}

# Others ####
custom_configure_matrix_question_qualtrics <- function(questionConfig, choices, choicesDisplayLogics, answers, questionText = NA, changeAnswer = TRUE) {
  # configure question choices
  QuestionChoices <- list()
  for (idx in seq_along(choices)) {
    question <- list(Display = choices[[idx]])

    if (length(choicesDisplayLogics) > 0 && length(choicesDisplayLogics[[idx]]) > 0) {
      question[["DisplayLogic"]] <- choicesDisplayLogics[[idx]]
    }
    QuestionChoices[[as.character(idx)]] <- question
  }

  # configure question choice order
  QuestionChoiceOrder <- as.list(as.character(seq_along(choices)))

  # configure answers
  QuestionAnswers <- list()
  for (idx in seq_along(answers)) {
    answer <- list(Display = answers[[idx]])
    QuestionAnswers[[as.character(idx)]] <- answer
  }

  # configure question answer order
  QuestionAnswerOrder <- as.list(as.character(seq_along(answers)))

  # config
  questionConfig[["Choices"]] <- QuestionChoices
  questionConfig[["ChoiceOrder"]] <- QuestionChoiceOrder

  if (changeAnswer) {
    questionConfig[["Answers"]] <- QuestionAnswers
    questionConfig[["AnswerOrder"]] <- QuestionAnswerOrder
  }

  if (!is.na(questionText)) {
    questionConfig[["QuestionText"]] <- questionText
    questionConfig[["QuestionText_Unsafe"]] <- questionText # :'D
  }

  return(questionConfig)
}

custom_update_matrix_question_qualtrics <-
  function(qualtricsKey, surveyID, questionID, sensorType = NULL, newQuestionText = NA, applyEmailLogic = TRUE, applyNewAnswer = TRUE, DEBUG = F) {
  # pull current question from qualtrics
  question <- get_single_qualtrics_question(qualtricsKey, surveyID, questionID)
  result <- question$result

  # update question
  questionNewInfo <- generate_ID_and_email_logics_lists_qualtrics(sensorType, getEmailLogics = applyEmailLogic)
  modifiedResult <-
    custom_configure_matrix_question_qualtrics(
      result,
      questionNewInfo[["QuestionRows"]],
      questionNewInfo[["QuestionLogics"]],
      list("Yes", "No"), # TODO: Maybe custom configure this too for the future?
      newQuestionText,
      applyNewAnswer
    )

  # push update to qualtrics
  modify_qualtrics_question(qualtricsKey, modifiedResult, surveyID, questionID)
  if (DEBUG) {
    print(paste("Updated question:", questionID))
  }
}


