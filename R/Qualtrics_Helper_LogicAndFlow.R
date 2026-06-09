#' Create display logic structure for Qualtrics
#'
#' Creates a nested list structure representing a single display logic
#' expression used by the Qualtrics API. This is a lightweight helper used
#' when constructing question display logic or flow branch conditions.
#'
#' @param logicType Character. The logic type (e.g. "EmbeddedField").
#' @param leftOperand Character. Left operand name used in the expression.
#' @param operator Character. Comparison operator (e.g. "EqualTo").
#' @param rightOperand Character. Right-hand value to compare against.
#' @param eqType Character. Expression type (commonly "Expression").
#'
#' @return A nested list representing Qualtrics display logic.
#'
#' @details
#' The returned list follows the Qualtrics API structure for a BooleanExpression
#' with a single If clause.
#'
#' @examples
#' create_display_logic_qualtrics("EmbeddedField", "RecipientEmail", "EqualTo", "me@example.com", "Expression")
#'
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
  res
}

#' Create display logic for a recipient email
#'
#' Helper that wraps `create_display_logic_qualtrics()` to produce an
#' EmbeddedField equality test against `RecipientEmail`. NA addresses are
#' converted to the placeholder "0" to match Qualtrics' expectations.
#'
#' @param emailAddress Character. Email address to match; NA becomes "0".
#'
#' @return A nested list representing the email display logic.
#'
#' @examples
#' create_email_logic_qualtrics(NA)
#'
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_email_logic_qualtrics <- function(emailAddress) {
  if (is.na(emailAddress)) {
    emailAddress <- "0"
  }

  create_display_logic_qualtrics("EmbeddedField", "RecipientEmail", "EqualTo", emailAddress, "Expression")
}


#' Generate question rows and email logic lists for Qualtrics
#'
#' Builds the display rows and optional email display logic list from the
#' personnel-sensor merge used by the package. This function expects
#' `get_merge_personnel_sensor_list()` to return a data.frame or tibble with
#' columns `DeviceID`, `Type`, `SiteName`, and `Email`.
#'
#' @param sensorType Optional character. Sensor type to filter when calling
#'   `get_merge_personnel_sensor_list()`.
#' @param getEmailLogics Logical. If TRUE, returns a `QuestionLogics` list
#'   populated by `create_email_logic_qualtrics()`.
#'
#' @return A list with elements `QuestionRows` (character vector) and
#'   `QuestionLogics` (list) when requested.
#'
#' @examples
#' \dontrun{
#' generate_ID_and_email_logics_lists_qualtrics("PM2.5")
#' }
#'
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
generate_ID_and_email_logics_lists_qualtrics <- function(sensorType = NULL, getEmailLogics = TRUE) {
  merger <- get_merge_personnel_sensor_list(sensorType) %>%
    dplyr::mutate(QuestionRows = paste(DeviceID, Type, SiteName, sep = ", "))

  res <- list(
    QuestionRows = merger[["QuestionRows"]],
    QuestionLogics = if (getEmailLogics) purrr::map(.x = merger[["Email"]], .f = create_email_logic_qualtrics) else list()
  )
  res
}

#' Create flow logic structure for Qualtrics flow
#'
#' Same structure as `create_display_logic_qualtrics()` but without the
#' `inPage` flag; used when constructing Flow branch logic objects.
#'
#' @param logicType Character. The logic type.
#' @param leftOperand Character. Left operand name.
#' @param operator Character. Operator name.
#' @param rightOperand Character. Right operand value.
#' @param eqType Character. Expression type.
#'
#' @return A list representing a BooleanExpression for use in Flow objects.
#'
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
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
  res
}

#' Create flow logic for recipient email
#'
#' Similar to `create_email_logic_qualtrics()` but returns the Flow-style
#' BooleanExpression used in Flow objects.
#'
#' @param emailAddress Character. Email address; NA becomes "0".
#'
#' @return A nested list for Flow branch logic.
#'
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_flow_email_logic_qualtrics <- function(emailAddress) {
  if (is.na(emailAddress)) {
    emailAddress <- "0"
  }

  create_flow_logic_qualtrics("EmbeddedField", "RecipientEmail", "EqualTo", emailAddress, "Expression")
}

#' Create multiple flow logic expressions
#'
#' Build a multi-clause BooleanExpression for use in Flow objects. Conjunctions
#' are inserted between subsequent expressions; the first clause has no
#' conjunction (NA).
#'
#' @param logicTypes Character vector of logic types.
#' @param leftOperands Character vector of left operands.
#' @param operators Character vector of operators.
#' @param rightOperands Character vector of right operand values.
#' @param eqTypes Character vector of expression types.
#' @param conjunctions Character vector of conjunctions (e.g. "Or", "And").
#'
#' @return A list matching Qualtrics' BooleanExpression structure.
#'
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
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
      sublist[["Conjunction"]] <- conjunction
    }
    sublist
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
  res
}

#' Create multiple flow email logic expressions
#'
#' Convert a vector of email addresses into a multi-clause BooleanExpression
#' where each clause checks `RecipientEmail` equality. NA addresses are
#' converted to the placeholder "0".
#'
#' @param emailAddresses Character vector of emails.
#'
#' @return A list suitable for use as Flow `BranchLogic` in Qualtrics.
#'
#' @examples
#' create_multiple_flow_email_logic_qualtrics(c("a@example.com", NA))
#'
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_multiple_flow_email_logic_qualtrics <- function(emailAddresses) {
  # if an email is NA, set it to "0"
  emailAddresses <-
    purrr::map(
      .x = emailAddresses,
      .f = \(x) { if (is.na(x)) "0" else x }
    )

  # shortcut to create display logic for emails
  create_multiple_flow_logic_qualtrics(
    rep("EmbeddedField", length(emailAddresses)),
    rep("RecipientEmail", length(emailAddresses)),
    rep("EqualTo", length(emailAddresses)),
    emailAddresses,
    rep("Expression", length(emailAddresses)),
    rep("Or", length(emailAddresses) - 1)
  )
}

#' Create a Qualtrics Branch flow body
#'
#' Construct a list representing a Qualtrics Flow Branch element.
#'
#' @param description Character. Branch description text.
#' @param flowID Character or numeric. Flow identifier.
#' @param logic List. Branch logic structure (from flow logic helpers).
#' @param subflow List. Optional nested flow elements.
#'
#' @return A list representing a Flow `Branch` element.
#'
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_branch_flow_body_qualtrics <- function(description, flowID, logic, subflow = list()) {
  list(
    Type = "Branch",
    FlowID = flowID,
    Description = description,
    BranchLogic = logic,
    Flow = subflow
  )
}

#' Create a Qualtrics Block flow body
#'
#' Construct a list representing a Qualtrics Flow Block element.
#'
#' @param blockID Character or numeric. Block identifier.
#' @param flowID Character or numeric. Flow identifier.
#'
#' @return A list representing a Flow `Block` element.
#'
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
create_block_flow_body_qualtrics <- function(blockID, flowID) {
  list(
    Type = "Block",
    ID = blockID,
    FlowID = flowID
  )
}

#' Configure a matrix question for Qualtrics
#'
#' Populate the `questionConfig` object with `Choices`, `ChoiceOrder`,
#' and optionally `Answers`/`AnswerOrder`. Display logic for individual
#' choices can be attached when provided.
#'
#' @param questionConfig List. Existing question configuration object returned
#'   by the Qualtrics API.
#' @param choices Character or list. Display texts for each choice.
#' @param choicesDisplayLogics List of display logic objects corresponding to
#'   each choice. Use `list()` for no display logic.
#' @param answers Character or list. Answer display texts.
#' @param questionText Character or NA. Optional question text to set.
#' @param changeAnswer Logical. If TRUE, `Answers` and `AnswerOrder` are set.
#'
#' @return The modified `questionConfig` list.
#'
#' @examples
#' \dontrun{
#' custom_configure_matrix_question_qualtrics(cfg, c("A","B"), list(), c("Yes","No"))
#' }
#'
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
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
    questionConfig[["QuestionText_Unsafe"]] <- questionText
  }

  questionConfig
}

#' Update a matrix question on Qualtrics
#'
#' Retrieve an existing matrix question, update its choices/answers and optional
#' display logic, then push the update back to Qualtrics.
#'
#' @param qualtrics_api_key Character. API key or key identifier used by the
#'   package's Qualtrics helper functions.
#' @param surveyID Character. Survey identifier.
#' @param questionID Character. Question identifier within the survey.
#' @param sensorType Optional character. Sensor type used to build question rows.
#' @param newQuestionText Character or NA. Optional replacement question text.
#' @param applyEmailLogic Logical. If TRUE, email display logic will be applied.
#' @param applyNewAnswer Logical. If TRUE, answers will be set/updated.
#' @param answer_choices Character vector. Answer choices to apply (defaults to Yes/No).
#' @param DEBUG Logical. If TRUE, prints a brief update message.
#'
#' @return NULL (called for side effects). Invisibly returns NULL on success.
#'
#' @examples
#' \dontrun{
#' custom_update_matrix_question_qualtrics(key, "SV_123", "QID1")
#' }
#'
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
custom_update_matrix_question_qualtrics <-
  function(qualtrics_api_key, surveyID, questionID, sensorType = NULL, newQuestionText = NA, applyEmailLogic = TRUE, applyNewAnswer = TRUE, answer_choices = c("Yes", "No"), DEBUG = FALSE) {
  # pull current question from qualtrics
  question <- get_single_qualtrics_question(qualtrics_api_key, surveyID, questionID)
  result <- question$result

  # update question
  questionNewInfo <- generate_ID_and_email_logics_lists_qualtrics(sensorType, getEmailLogics = applyEmailLogic)
  modifiedResult <-
    custom_configure_matrix_question_qualtrics(
      result,
      questionNewInfo[["QuestionRows"]],
      questionNewInfo[["QuestionLogics"]],
      as.list(answer_choices),
      newQuestionText,
      applyNewAnswer
    )

  # push update to qualtrics
  modify_qualtrics_question(qualtrics_api_key, modifiedResult, surveyID, questionID)
  if (DEBUG) {
    message(paste("Updated question:", questionID))
  }
  invisible(NULL)
}