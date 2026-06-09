#' User Setup Helper for RCommunityAirSensor Package
#'
#' This file contains functions to help users set up their environment variables
#' and folder structures for the RCommunityAirSensor package.

# Helper function for input validation
validate_input <- function(prompt, allow_empty = FALSE, variable_name = "") {
  repeat {
    cat(prompt)
    input <- readline()
    input <- trimws(input)

    if (!allow_empty && (is.null(input) || input == "")) {
      cat("Error: This field cannot be empty. Please try again.\n\n")
      next
    }

    if (allow_empty && (is.null(input) || input == "")) {
      return("")
    }
 
    return(input)
  }
}

# Helper function to ask yes/no questions
ask_yes_no <- function(prompt) {
  repeat {
    cat(paste0(prompt, " (y/n): "))
    response <- tolower(trimws(readline()))

    if (response %in% c("y", "yes")) {
      return(TRUE)
    } else if (response %in% c("n", "no")) {
      return(FALSE)
    } else {
      cat("Please enter 'y' for yes or 'n' for no.\n")
    }
  }
}

#' Setup Helper Function
#'
#' Interactive setup function that walks users through the complete setup process
#' for the RCommunityAirSensor package. Sets up environment variables, creates
#' necessary folder structures, and initializes tracking files.
#'
#' @param testing Logical. If TRUE, saves environment variables to .RTestEnviron
#'   instead of .Renviron. Should only be used for package development. Default: FALSE.
#' @return NULL (invisible). Function is called for its side effects.
#' @export
#' @examples
#' \dontrun{
#' setup_helper()
#' setup_helper(testing = TRUE)  # For development only
#' }
setup_helper <- function(testing = FALSE) {
  cat("================================================================================\n")
  cat("           RCommunityAirSensor Package Setup Helper\n")
  cat("================================================================================\n\n")

  # Warning for testing mode
  if (testing) {
    cat("WARNING: TESTING MODE ENABLED \n")
    cat("This process will save all environment variables to .RTestEnviron instead\n")
    cat("of the normal .Renviron file. This should be used for package development ONLY!\n\n")
  }

  cat("Welcome to the setup process for the RCommunityAirSensor package.\n")
  cat("This setup is required but can be escaped and restarted without harm.\n")
  cat("Any already created files/sheets will be kept the same.\n\n")

  if (!ask_yes_no("Do you want to continue with the setup?")) {
    cat("Setup cancelled. You can restart this process anytime.\n")
    return(invisible(NULL))
  }

  cat("\n================================================================================\n")
  cat("                          Environment Variables Setup\n")
  cat("================================================================================\n\n")

  # Storage for environment variables
  env_vars <- character()

  # === Standalone Environment Variables ===
  cat("--- API Keys and Service Configuration ---\n\n")

  # Purple Air API
  if (ask_yes_no("Are you using Purple Air sensors?")) {
    purple_api <- validate_input("Enter your Purple Air API key: ", allow_empty = FALSE)
    env_vars <- c(env_vars, paste0("PURPLEAPI=", purple_api))
  }

  # Clarity API
  if (ask_yes_no("Are you using Clarity sensors?")) {
    clarity_api <- validate_input("Enter your Clarity API key: ", allow_empty = FALSE)
    env_vars <- c(env_vars, paste0("CLARITYAPI=", clarity_api))
  }

  # Qualtrics Configuration
  if (ask_yes_no("Do you want to use Qualtrics for QA processing?")) {
    cat("\nQualtrics Configuration:\n")

    # Base URL
    cat("Default Qualtrics Base URL is 'usf.az1.qualtrics.com'\n")
    if (ask_yes_no("Do you want to use a different base URL?")) {
      base_url <- validate_input("Enter your Qualtrics base URL: ", allow_empty = FALSE)
    } else {
      base_url <- "usf.az1.qualtrics.com"
    }
    env_vars <- c(env_vars, paste0("QUALTRICS_BASE_URL=", base_url))

    # Required Qualtrics variables
    qualtrics_api <- validate_input("Enter your Qualtrics API key: ", allow_empty = FALSE)
    library_id <- validate_input("Enter your Qualtrics Library ID: ", allow_empty = FALSE)
    message_id <- validate_input("Enter your Qualtrics Message ID: ", allow_empty = FALSE)
    reminder_message_id <- validate_input("Enter your Qualtrics Reminder Message ID: ", allow_empty = FALSE)
    mailing_list_id <- validate_input("Enter your Qualtrics Mailing List ID: ", allow_empty = FALSE)
    directory_id <- validate_input("Enter your Qualtrics Directory ID: ", allow_empty = FALSE)

    env_vars <- c(env_vars,
                  paste0("QUALTRICS_API_KEY=", qualtrics_api),
                  paste0("QUALTRICS_LIBRARY_ID=", library_id),
                  paste0("QUALTRICS_MESSAGE_ID=", message_id),
                  paste0("QUALTRICS_REMINDER_MESSAGE_ID=", reminder_message_id),
                  paste0("QUALTRICS_MAILINGLIST_ID=", mailing_list_id),
                  paste0("QUALTRICS_DIRECTORY_ID=", directory_id))

    # Optional Qualtrics templates
    if (ask_yes_no("Do you want to use the Qualtrics Weekly QA Template?")) {
      weekly_template <- validate_input("Enter your Qualtrics Weekly Template ID: ", allow_empty = FALSE)
      env_vars <- c(env_vars, paste0("QUALTRICS_WEEKLY_TEMPLATE_ID=", weekly_template))
    }

    if (ask_yes_no("Do you want to use the Qualtrics Monthly QA Template?")) {
      monthly_template <- validate_input("Enter your Qualtrics Monthly Template ID: ", allow_empty = FALSE)
      env_vars <- c(env_vars, paste0("QUALTRICS_MONTHLY_TEMPLATE_ID=", monthly_template))
    }

    # Add default Qualtrics Template Question IDs
    cat("\nAdding default Qualtrics Template Question IDs...\n")
    default_qualtrics_vars <- c(
      "WEEKLY_QUALTRICS_TEMPLATE_PA_Q1_ID=QID20",
      "WEEKLY_QUALTRICS_TEMPLATE_PA_Q2_ID=QID31",
      "WEEKLY_QUALTRICS_TEMPLATE_PA_Q3_ID=QID24",
      "WEEKLY_QUALTRICS_TEMPLATE_CL_Q1_ID=QID6",
      "WEEKLY_QUALTRICS_TEMPLATE_CL_Q2_ID=QID10",
      "WEEKLY_QUALTRICS_TEMPLATE_CL_Q3_ID=QID11"
    )
    env_vars <- c(env_vars, default_qualtrics_vars)
    cat("Default question IDs added.\n")
  }

  # SMTP API
  if (ask_yes_no("Are you using SMTP for sending emails?")) {
    smtp_api <- validate_input("Enter your SMTP API key: ", allow_empty = FALSE)
    env_vars <- c(env_vars, paste0("SMTP_API=", smtp_api))
  }

  # === File Creation Environment Variables ===
  cat("\n--- Folder Structure Configuration ---\n")
  cat("WARNING: The following environment variables will dictate where:\n")
  cat("   - The tracking Excel sheet for monitors will be created\n")
  cat("   - Root location for logs and download files will be stored\n\n")

  upload_root <- validate_input("Enter the UPLOAD_ROOT_FOLDER path: ", allow_empty = FALSE)
  records_root <- validate_input("Enter the RECORDS_ROOT_FOLDER path: ", allow_empty = FALSE)

  env_vars <- c(env_vars,
                paste0("UPLOAD_ROOT_FOLDER=", upload_root),
                paste0("RECORDS_ROOT_FOLDER=", records_root))

  # === Save Environment Variables ===
  cat("\n================================================================================\n")
  cat("                        Saving Environment Variables\n")
  cat("================================================================================\n\n")

  # Determine the file to save to
  if (testing) {
    env_file <- ".RTestEnviron"  # Saves to working directory for testing
  } else {
    env_file <- file.path(Sys.getenv("HOME"), ".Renviron")  # Home directory for production
  }

  tryCatch({
    # Write to environment file
    writeLines(env_vars, env_file)
    cat("Environment variables saved to:", env_file, "\n")
  }, error = function(e) {
    cat("Error saving environment variables:", e$message, "\n")
    return(invisible(NULL))
  })

  # === Create Folder Structures ===
  cat("\n================================================================================\n")
  cat("                        Creating Folder Structures\n")
  cat("================================================================================\n\n")

  # Create folder structure for UPLOAD_ROOT_FOLDER
  cat("Creating folder structure in UPLOAD_ROOT_FOLDER...\n")
  tryCatch({
    setup_folder_and_file_structure(upload_root)
    cat("[OK] Folder structure created successfully in:", upload_root, "\n")
  }, error = function(e) {
    cat("[ERROR] Error creating folder structure:", e$message, "\n")
  })

  # Create Excel file in RECORDS_ROOT_FOLDER
  cat("\nCreating Excel tracking file in RECORDS_ROOT_FOLDER...\n")
  tryCatch({
    setup_excel_file(records_root)
    cat("[OK] Excel tracking file created successfully in:", records_root, "\n")
  }, error = function(e) {
    cat("[ERROR] Error creating Excel file:", e$message, "\n")
  })

  # === Setup Complete ===
  cat("\n================================================================================\n")
  cat("                            Setup Complete!\n")
  cat("================================================================================\n\n")

  cat("Setup process completed successfully!\n\n")
  cat("Summary of what was created:\n")
  cat("   - Environment variables saved to:", env_file, "\n")
  cat("   - Folder structure created in:", upload_root, "\n")
  cat("   - Excel tracking file created in:", records_root, "\n\n")

  cat("IMPORTANT: Please restart R to load the environment variables properly.\n")
  cat("   You can do this by restarting your R session or RStudio.\n\n")

  cat("You can now start using the RCommunityAirSensor package!\n")

  invisible(NULL)
}

# Update: 12-Oct-2025
# - Added
