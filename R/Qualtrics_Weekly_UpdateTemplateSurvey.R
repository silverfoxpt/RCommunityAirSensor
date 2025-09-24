qualtrics_update_weekly_template_survey <- function() {
  # Installation and loading
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, glue, httr2, jsonlite, lubridate, qualtRics)

  sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsUtil.R")
  source(file = sourcePath, echo = FALSE)

  sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsSecondaryUtils.R")
  source(file = sourcePath, echo = FALSE)

  sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsFlowNLogicUtils.R")
  source(file = sourcePath, echo = FALSE)

  # import env vars
  qualtKey <- Sys.getenv("QUALTRICS_API_KEY")
  surveyId <- Sys.getenv("QUALTRICS_WEEKLY_TEMPLATE_ID")

  # update all questions
  listID <- list(
    Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_PA_Q1_ID"),
    Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_PA_Q2_ID"),
    Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_PA_Q3_ID"),
    Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_CL_Q1_ID"),
    Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_CL_Q2_ID"),
    Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_CL_Q3_ID")
  )
  listType <- c(rep("PurpleAir", 3), rep("Clarity", 3))
  purrr::pwalk(
    .l = list(listID, listType),
    .f = \(x, y) custom_update_matrix_question_qualtrics(qualtKey, surveyId, x, y)
  )

  # # Update all flows - TODO: Refactor this...
  # # PurpleAir flow
  # update_survey_flow_element(
  #   qualtKey, surveyId, Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_PA_FLOWID_BRANCH"),
  #   create_branch_flow_body_qualtrics(
  #     "BranchPA",
  #     Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_PA_FLOWID_BRANCH"),
  #     create_multiple_flow_email_logic_qualtrics(
  #       get_merge_personnel_sensor_list("PurpleAir") %>%
  #         dplyr::filter(! is.na(Email)) %>%
  #         dplyr::distinct(Email) %>%
  #         dplyr::pull(Email)
  #     ),
  #     list(
  #       create_block_flow_body_qualtrics(
  #         Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_PA_BLOCKID"),
  #         Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_PA_FLOWID_BLOCK")
  #       )
  #     )
  #   )
  # )

  # # Clarity flow
  # update_survey_flow_element(
  #   qualtKey, surveyId, Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_CL_FLOWID_BRANCH"),
  #   create_branch_flow_body_qualtrics(
  #     "BranchCL",
  #     Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_CL_FLOWID_BRANCH"),
  #     create_multiple_flow_email_logic_qualtrics(
  #       get_merge_personnel_sensor_list("Clarity") %>%
  #         dplyr::filter(! is.na(Email)) %>%
  #         dplyr::distinct(Email) %>%
  #         dplyr::pull(Email)
  #     ),
  #     list(
  #       create_block_flow_body_qualtrics(
  #         Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_CL_BLOCKID"),
  #         Sys.getenv("WEEKLY_QUALTRICS_TEMPLATE_CL_FLOWID_BLOCK")
  #       )
  #     )
  #   )
  # )

  # log to file
  write_to_weekly_template_update_log(Sys.Date(), "UpdateWeeklyTemplate")
}

#readRenviron(file.path(getwd(), ".Rqualtrics"))
qualtrics_update_weekly_template_survey()

# Tested: 20 Jan 2025
