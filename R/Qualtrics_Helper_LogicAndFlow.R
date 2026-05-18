# Question Display Logics ####
 #' Create display logic element for Qualtrics
#'
#' Helper to create a single display logic BooleanExpression used in Qualtrics
#'
#' @param logicType Character logic type (e.g. "EmbeddedField").
#' @param leftOperand Character left operand name.
#' @param operator Character comparison operator.
#' @param rightOperand Character right operand value.
#' @param eqType Character expression type.
#' @return A list representing a Qualtrics display logic BooleanExpression.
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
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

 #' Create display logic for recipient email
#'
#' Shortcut wrapper to create an email-specific display logic where the
#' left operand is `RecipientEmail`.
#'
#' @param emailAddress Character email address. If NA, will be coerced to "0".
#' @return A list representing the display logic for email matching.
#' @concept role:helper
create_email_logic_qualtrics <- function(emailAddress) {
  if (is.na(emailAddress)) {
    emailAddress <- "0"
  }

  return(create_display_logic_qualtrics("EmbeddedField", "RecipientEmail", "EqualTo", emailAddress, "Expression"))
}

# Helper function for generating email logic based on weekly personnel list
 #' Generate ID and email logic lists
#'
#' Produces the QuestionRows vector and (optionally) a list of email display
#' logic entries for use when updating Qualtrics questions or flows.
#'
#' @param sensorType Optional character filter applied to the sensors.
#' @param getEmailLogics Logical; if TRUE returns the email logic list.
#' @return A list with `QuestionRows` and `QuestionLogics`.
#' @concept role:helper
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
 #' Create a survey flow logic element for Qualtrics
#'
#' Creates a BooleanExpression list suitable for Qualtrics survey flow rules.
#'
#' @param logicType Character logic type.
#' @param leftOperand Character left operand.
#' @param operator Character operator.
#' @param rightOperand Character right operand.
#' @param eqType Character expression type.
#' @return A list representing a survey flow BooleanExpression.
#' @concept role:helper
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

 #' Create survey flow logic for recipient email
#'
#' @param emailAddress Character email address. NA becomes "0".
#' @return A list representing the flow logic for email comparison.
#' @concept role:helper
create_flow_email_logic_qualtrics <- function(emailAddress) {
  if (is.na(emailAddress)) {
    emailAddress <- "0"
  }

  return(create_flow_logic_qualtrics("EmbeddedField", "RecipientEmail", "EqualTo", emailAddress, "Expression"))
}


# multiple conditions
# TODO: Please, someone, clean this up
 #' Create multiple-condition survey flow logic
#'
#' Helper to build a Qualtrics BooleanExpression containing multiple
#' condition rows and optional conjunctions.
#'
#' @param logicTypes Character vector of logic types.
#' @param leftOperands Character vector of left operands.
#' @param operators Character vector of operators.
#' @param rightOperands Character vector of right operands.
#' @param eqTypes Character vector of expression types.
#' @param conjunctions Character vector of conjunctions (length = n-1).
#' @return A list representing the multi-condition BooleanExpression.
#' @concept role:helper
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
  names(listLogics) <- purrr::map_chr(0:(length(logicTypes) - 1), as.character)
  listLogics[["Type"]] <- "If"

  # create final list
  res <- list(
    "0" = listLogics,
    Type = "BooleanExpression"
  )
  return(res)
}

 #' Create multiple email flow logic
#'
#' Build a multi-condition email matching boolean expression for Qualtrics.
#'
#' @param emailAddresses Character vector of email addresses (NAs allowed).
#' @return A list representing the multi-email BooleanExpression.
#' @concept role:helper
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

 #' Create a branch flow element body
#'
#' @param description Character description of the branch.
#' @param flowID Character/ID for the flow.
#' @param logic List representing the branch logic.
#' @param subflow Optional subflow list.
#' @return A list representing a Qualtrics Branch flow element.
#' @concept role:helper
create_branch_flow_body_qualtrics <- function(description, flowID, logic, subflow = list()) {
  return(list(
    Type = "Branch",
    FlowID = flowID,
    Description = description,
    BranchLogic = logic,
    Flow = subflow
  ))
}

 #' Create a block flow element body
#'
#' @param blockID Block ID.
#' @param flowID Flow ID.
#' @return A list representing a Qualtrics Block flow element.
#' @concept role:helper
create_block_flow_body_qualtrics <- function(blockID, flowID) {
  return(list(
    Type = "Block",
    ID = blockID,
    FlowID = flowID
  ))
}

# Others ####
 #' Configure a matrix question structure for Qualtrics
#'
#' Helper that prepares the `Choices`, `ChoiceOrder`, `Answers`, and
#' `AnswerOrder` elements of a matrix-style question configuration.
#'
#' @param questionConfig List representing existing question configuration.
#' @param choices Character vector of choice labels.
#' @param choicesDisplayLogics List of display logics per choice (may be empty).
#' @param answers Character vector of answer labels.
#' @param questionText Optional question text.
#' @param changeAnswer Logical whether to set Answers/AnswerOrder.
#' @return Modified `questionConfig` list.
#' @concept role:helper
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

 #' Update a matrix question in Qualtrics with new rows and logics
#'
#' Pulls the question JSON, updates rows/answers/display logic, and
#' pushes the updated question back to Qualtrics.
#'
#' @param qualtricsKey Character API token.
#' @param surveyID Character survey id.
#' @param questionID Character question id.
#' @param sensorType Optional character sensor filter passed to personnel merge.
#' @param newQuestionText Optional character to set as new question text.
#' @param applyEmailLogic Logical whether to apply email display logic.
#' @param applyNewAnswer Logical whether to replace Answers/AnswerOrder.
#' @param DEBUG Logical debug printing flag. Defaults to FALSE.
#' @return Invisibly returns NULL; side-effect updates Qualtrics question.
#' @concept role:helper
custom_update_matrix_question_qualtrics <-
  function(qualtricsKey, surveyID, questionID, sensorType = NULL, newQuestionText = NA, applyEmailLogic = TRUE, applyNewAnswer = TRUE, DEBUG = FALSE) {
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
      list("Yes", "No"),
      newQuestionText,
      applyNewAnswer
    )

  # push update to qualtrics
  modify_qualtrics_question(qualtricsKey, modifiedResult, surveyID, questionID)
  if (DEBUG) {
    message(paste("Updated question:", questionID))
  }
  invisible(NULL)
}


