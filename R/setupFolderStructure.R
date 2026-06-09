#' Setup Standard Folder and File Structure
#'
#' Creates a standardized folder structure for air sensor data management and
#' initializes required CSV files with appropriate column headers.
#' If folders already exist, they will be ignored (no error thrown).
#' If CSV files already exist, they will not be overwritten.
#'
#' @param directory Character string. The base directory where the folder structure should be created.
#' @return NULL (invisible). Function is called for its side effects.
#' @export
#' @examples
#' \dontrun{
#' setup_folder_and_file_structure("C:/MyProject")
#' }
setup_folder_and_file_structure <- function(directory) {
  # Validate input
  if (!is.character(directory) || length(directory) != 1) {
    stop("directory must be a single character string")
  }

  # Define the folder structure
  folders <- c(
    "CSV",
    "CSV/Clarity",
    "CSV/Clarity-Reference",
    "CSV/Exports",
    "CSV/Imports",
    "CSV/Instant-Report",
    "CSV/PurpleAir",
    "CSV/QATimeshift",
    "CSV/Qualtrics",
    "CSV/Qualtrics/Monthly",
    "CSV/Qualtrics/Weekly"
  )

  # Create each folder if it doesn't exist
  for (folder in folders) {
    full_path <- file.path(directory, folder)
    if (!dir.exists(full_path)) {
      dir.create(full_path, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # Define required CSV files with their column structures
  csv_files <- list(
    list(directory = file.path(directory, "CSV/Exports"), filename = "ClarityLog.csv", columns = "OriginDate,Complete"),
    list(directory = file.path(directory, "CSV/Exports"), filename = "PurpleAirLog.csv", columns = "OriginDate,Complete"),
    list(directory = file.path(directory, "CSV/Exports"), filename = "QualtricsMonthlyLog.csv", columns = "OriginDate,Action,SaveData"),
    list(directory = file.path(directory, "CSV/Exports"), filename = "QualtricsUpdateLog.csv", columns = "OriginDate,Action"),
    list(directory = file.path(directory, "CSV/Exports"), filename = "QualtricsWeeklyLog.csv", columns = "OriginDate,Action,SaveData"),
    list(directory = file.path(directory, "CSV/Imports"), filename = "MainPersonnel.csv", columns = "FirstName,LastName,Email,Role"),
    list(directory = file.path(directory, "CSV/Imports"), filename = "MonthlyUpdateQuestion.csv", columns = "QuestionID,SensorType,QuestionTag,QuestionColumnName,NotNormalAnswer"),
    list(directory = file.path(directory, "CSV/Imports"), filename = "WeeklyUpdateQuestion.csv", columns = "QuestionID,SensorType,QuestionTag,QuestionColumnName,NotNormalAnswer"),
    list(directory = file.path(directory, "CSV/Imports"), filename = "UnresolvedMonitor.csv", columns = "OriginDate,DeviceID,SiteName,Reason,Resolved")
  )

  # Create each CSV file if it doesn't exist using the helper function
  for (csv_info in csv_files) {
    full_csv_path <- file.path(csv_info$directory, csv_info$filename)

    # Only create if file doesn't exist
    if (!file.exists(full_csv_path)) {
      tryCatch({
        create_csv_with_columns(csv_info$columns, csv_info$directory, csv_info$filename)
      }, error = function(e) {
        warning("Could not create CSV file: ", full_csv_path, " - ", e$message)
      })
    }
  }

  invisible(NULL)
}

#' Check if Standard Folder and File Structure Exists
#'
#' Checks if the exact standardized folder structure and required CSV files exist
#' in the specified directory. All folders and files must exist exactly as
#' specified for the function to return TRUE.
#'
#' @param directory Character string. The base directory to check for the folder structure.
#' @param debug Logical. If TRUE, prints the first missing folder, file, sheet, or column mismatch before returning FALSE.
#' @return Logical. TRUE if the exact folder structure and files exist, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' check_folder_and_file_structure("C:/MyProject")
#' }
check_folder_and_file_structure <- function(directory, debug = FALSE) {
  # Validate input
  if (!is.character(directory) || length(directory) != 1) {
    stop("directory must be a single character string")
  }

  if (!is.logical(debug) || length(debug) != 1 || is.na(debug)) {
    stop("debug must be a single logical value")
  }

  # Check if base directory exists
  if (!dir.exists(directory)) {
    return(FALSE)
  }

  # Define the expected folder structure
  required_folders <- c(
    "CSV",
    "CSV/Clarity",
    "CSV/Clarity-Reference",
    "CSV/Exports",
    "CSV/Imports",
    "CSV/Instant-Report",
    "CSV/PurpleAir",
    "CSV/QATimeshift",
    "CSV/Qualtrics",
    "CSV/Qualtrics/Monthly",
    "CSV/Qualtrics/Weekly"
  )

  # Check if all required folders exist
  for (folder in required_folders) {
    full_path <- file.path(directory, folder)
    if (!dir.exists(full_path)) {
      if (isTRUE(debug)) {
        message("Missing folder: ", full_path)
      }
      return(FALSE)
    }
  }

  # Define required CSV files with their expected column structures
  required_csv_files <- list(
    "CSV/Exports/ClarityLog.csv" = c("OriginDate", "Complete"),
    "CSV/Exports/PurpleAirLog.csv" = c("OriginDate", "Complete"),
    "CSV/Exports/QualtricsMonthlyLog.csv" = c("OriginDate", "Action", "SaveData"),
    "CSV/Exports/QualtricsUpdateLog.csv" = c("OriginDate", "Action"),
    "CSV/Exports/QualtricsWeeklyLog.csv" = c("OriginDate", "Action", "SaveData"),
    "CSV/Imports/MainPersonnel.csv" = c("FirstName", "LastName", "Email", "Role"),
    "CSV/Imports/MonthlyUpdateQuestion.csv" = c("QuestionID", "SensorType", "QuestionTag", "QuestionColumnName", "NotNormalAnswer"),
    "CSV/Imports/WeeklyUpdateQuestion.csv" = c("QuestionID", "SensorType", "QuestionTag", "QuestionColumnName", "NotNormalAnswer"),
    "CSV/Imports/UnresolvedMonitor.csv" = c("OriginDate", "DeviceID", "SiteName", "Reason", "Resolved")
  )

  # Check if all required CSV files exist and have correct column structure
  for (csv_path in names(required_csv_files)) {
    full_csv_path <- file.path(directory, csv_path)

    # Check if file exists
    if (!file.exists(full_csv_path)) {
      if (isTRUE(debug)) {
        message("Missing CSV file: ", full_csv_path)
      }
      return(FALSE)
    }

    # Check if file has correct column structure
    tryCatch({
      # Read only the header to check column names
      csv_data <- read.csv(full_csv_path, nrows = 0, stringsAsFactors = FALSE)
      actual_columns <- names(csv_data)
      expected_columns <- required_csv_files[[csv_path]]

      # Check if columns match exactly (order and names)
      if (!identical(actual_columns, expected_columns)) {
        if (isTRUE(debug)) {
          message("CSV column mismatch in: ", full_csv_path)
          message("Expected: ", paste(expected_columns, collapse = ", "))
          message("Actual:   ", paste(actual_columns, collapse = ", "))
        }
        return(FALSE)
      }
    }, error = function(e) {
      # If we can't read the file, consider it invalid
      if (isTRUE(debug)) {
        message("Could not read CSV file: ", full_csv_path)
        message("Error: ", e$message)
      }
      return(FALSE)
    })
  }

  return(TRUE)
}

#' Setup CAMN Monitor Tracking Excel File
#'
#' Creates a comprehensive Excel file for CAMN monitor tracking with multiple sheets
#' containing predefined column structures for monitoring air quality devices.
#'
#' @param directory Character string. Directory where the Excel file should be created.
#' @return Character string. Path to the created Excel file (invisible).
#' @export
#' @examples
#' \dontrun{
#' setup_excel_file("C:/MyProject")
#' }
setup_excel_file <- function(directory) {
  # Validate input
  if (!is.character(directory) || length(directory) != 1) {
    stop("directory must be a single character string")
  }

  # Create the main Excel file with MonitorStatus sheet
  monitor_status_columns <- "Label,Type,Hardware ID,API ID,Dashboard/API Organization ID,Location Short Code,Deployed Site Location,Data Sharing Setting,Owner"

  excel_file_path <- create_excel_with_columns(
    column_names_string = monitor_status_columns,
    directory = directory,
    filename = "CAMNMonitorTracking.xlsx",
    sheet_name = "MonitorStatus"
  )

  # Add ReferenceSiteData sheet
  reference_site_columns <- "Datasource ID,Site Name,Short Code,Collect PM2.5,Collect NO2"
  add_excel_new_sheet(excel_file_path, "ReferenceSiteData", reference_site_columns)

  # Add SitesAndHosts sheet
  sites_hosts_columns <- "Long Name of Location,Dashboard/Map Location Name,Short Code,Host Contact Person,Host Title,Email,Cellphone"
  add_excel_new_sheet(excel_file_path, "SitesAndHosts", sites_hosts_columns)

  message("CAMN Monitor Tracking Excel file setup complete: ", excel_file_path)
  invisible(excel_file_path)
}

#' Check CAMN Monitor Tracking Excel File Structure
#'
#' Validates the existence of the CAMNMonitorTracking.xlsx file and verifies that it contains
#' the required sheets with the correct column structures for CAMN monitor tracking.
#'
#' @param directory Character string. Directory where the Excel file should be located.
#' @param testing Logical. If TRUE, uses the alternate row offset used in tests.
#' @param debug Logical. If TRUE, prints the first missing sheet or column mismatch before returning FALSE.
#' @return Logical. TRUE if the Excel file exists with correct structure, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' check_excel_file("C:/MyProject")
#' }
check_excel_file <- function(directory, testing = FALSE, debug = FALSE) {
  # Check if openxlsx package is available
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required but not installed. Please install it using: install.packages('openxlsx')")
  }

  # Validate input
  if (!is.character(directory) || length(directory) != 1) {
    stop("directory must be a single character string")
  }

  if (!is.logical(testing) || length(testing) != 1 || is.na(testing)) {
    stop("testing must be a single logical value")
  }

  if (!is.logical(debug) || length(debug) != 1 || is.na(debug)) {
    stop("debug must be a single logical value")
  }

  # Check if directory exists
  if (!dir.exists(directory)) {
    return(FALSE)
  }

  # Define the expected Excel file path
  excel_file_path <- file.path(directory, "CAMNMonitorTracking.xlsx")

  # Check if Excel file exists
  if (!file.exists(excel_file_path)) {
    return(FALSE)
  }

  # Define expected sheets and their column structures
  expected_sheets <- list(
    "MonitorStatus" = c("Label", "Type", "Hardware ID", "API ID", "Dashboard/API Organization ID", "Location Short Code", "Deployed Site Location", "Data Sharing Setting", "Owner"),
    "ReferenceSiteData" = c("Datasource ID", "Site Name", "Short Code", "Collect PM2.5", "Collect NO2"),
    "SitesAndHosts" = c("Long Name of Location", "Dashboard/Map Location Name", "Short Code", "Host Contact Person", "Host Title", "Email", "Cellphone")
  )

  # Load and validate the Excel file structure
  tryCatch({
    # Load the workbook
    wb <- openxlsx::loadWorkbook(excel_file_path)

    # Get all sheet names in the workbook
    actual_sheets <- names(wb)

    # For testing only
    if (testing) {
        myStartRow <- 10
    } else {
        myStartRow <- 2
    }

    # Check if all expected sheets exist
    for (sheet_name in names(expected_sheets)) {
      if (!sheet_name %in% actual_sheets) {
        if (isTRUE(debug)) {
          message("Missing Excel sheet: ", sheet_name)
          message("Available sheets: ", paste(actual_sheets, collapse = ", "))
        }
        return(FALSE)
      }

      # Read the sheet to check column structure
      # Since columns start from row 2, we need to read from row 2
      sheet_data <- openxlsx::read.xlsx(wb, sheet = sheet_name, startRow = myStartRow, colNames = TRUE, rows = myStartRow:(myStartRow + 1))

      # Get actual column names (read.xlsx converts spaces to dots)
      actual_columns <- names(sheet_data)
      #print(actual_columns)
      expected_columns <- expected_sheets[[sheet_name]]
      #print(expected_columns)

      # Convert expected column names to match read.xlsx behavior (spaces to dots)
      expected_columns_converted <- gsub(" ", ".", expected_columns)

      # Check if columns match exactly (order and names)
      if (!identical(actual_columns, expected_columns_converted)) {
        if (isTRUE(debug)) {
          message("Excel column mismatch in sheet: ", sheet_name)
          message("Expected: ", paste(expected_columns, collapse = ", "))
          message("Actual:   ", paste(actual_columns, collapse = ", "))
        }
        return(FALSE)
      }
    }

    # If we get here, all validations passed
    return(TRUE)

  }, error = function(e) {
    # If we can't read the file or encounter any error, consider it invalid
    if (isTRUE(debug)) {
      message("Could not read Excel file: ", excel_file_path)
      message("Error: ", e$message)
    }
    return(FALSE)
  })
}

# ----------------------------------------- Helper Functions -----------------------------------------
#' Create CSV File with Column Names
#'
#' Creates a CSV file with specified column names from a comma-separated string.
#' The CSV file will have headers but no data rows.
#'
#' @param column_names_string Character string. Comma-separated column names (e.g., "Name,Age,City").
#' @param directory Character string. Directory where the CSV file should be created.
#' @param filename Character string. Name of the CSV file to create (default: "template.csv").
#' @return Character string. Path to the created CSV file (invisible).
#' @export
#' @examples
#' \dontrun{
#' create_csv_with_columns("Name,Age,City", "C:/MyProject", "people_template.csv")
#' create_csv_with_columns("Timestamp,PM2.5,Temperature", "C:/Data")
#' }
create_csv_with_columns <- function(column_names_string, directory, filename = "template.csv") {
  # Validate inputs
  if (!is.character(column_names_string) || length(column_names_string) != 1) {
    stop("column_names_string must be a single character string")
  }

  if (!is.character(directory) || length(directory) != 1) {
    stop("directory must be a single character string")
  }

  if (!is.character(filename) || length(filename) != 1) {
    stop("filename must be a single character string")
  }

  # Clean up the column names string (remove extra spaces)
  column_names_string <- trimws(column_names_string)

  if (nchar(column_names_string) == 0) {
    stop("column_names_string cannot be empty")
  }

  # Split the comma-separated string into individual column names
  column_names <- trimws(strsplit(column_names_string, ",")[[1]])

  # Remove empty column names
  column_names <- column_names[column_names != ""]

  if (length(column_names) == 0) {
    stop("No valid column names found in the input string")
  }

  # Create directory if it doesn't exist
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }

  # Create the full file path
  file_path <- file.path(directory, filename)

  # Create an empty data frame with the specified column names
  empty_df <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  names(empty_df) <- column_names

  # Write to CSV file
  tryCatch({
    write.csv(empty_df, file_path, row.names = FALSE)
    message("CSV file created successfully: ", file_path)
    message("Columns: ", paste(column_names, collapse = ", "))
    invisible(file_path)
  }, error = function(e) {
    stop("Error creating CSV file: ", e$message)
  })
}

#' Create Excel File with Column Names
#'
#' Creates an Excel file with specified column names from a comma-separated string.
#' The Excel file will have headers in row 1 starting from column A, but no data rows.
#' Requires the 'openxlsx' package to be installed.
#'
#' @param column_names_string Character string. Comma-separated column names (e.g., "Name,Age,City").
#' @param directory Character string. Directory where the Excel file should be created.
#' @param filename Character string. Name of the Excel file to create (default: "template.xlsx").
#' @param sheet_name Character string. Name of the worksheet (default: "Sheet1").
#' @return Character string. Path to the created Excel file (invisible).
#' @export
#' @examples
#' \dontrun{
#' create_excel_with_columns("Name,Age,City", "C:/MyProject", "people_template.xlsx")
#' create_excel_with_columns("Timestamp,PM2.5,Temperature", "C:/Data")
#' }
create_excel_with_columns <- function(column_names_string, directory, filename = "template.xlsx", sheet_name = "Sheet1") {
  # Check if openxlsx package is available
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required but not installed. Please install it using: install.packages('openxlsx')")
  }

  # Validate inputs
  if (!is.character(column_names_string) || length(column_names_string) != 1) {
    stop("column_names_string must be a single character string")
  }

  if (!is.character(directory) || length(directory) != 1) {
    stop("directory must be a single character string")
  }

  if (!is.character(filename) || length(filename) != 1) {
    stop("filename must be a single character string")
  }

  if (!is.character(sheet_name) || length(sheet_name) != 1) {
    stop("sheet_name must be a single character string")
  }

  # Ensure filename has .xlsx extension
  if (!grepl("\\.xlsx$", filename, ignore.case = TRUE)) {
    filename <- paste0(tools::file_path_sans_ext(filename), ".xlsx")
  }

  # Clean up the column names string (remove extra spaces)
  column_names_string <- trimws(column_names_string)

  if (nchar(column_names_string) == 0) {
    stop("column_names_string cannot be empty")
  }

  # Split the comma-separated string into individual column names
  column_names <- trimws(strsplit(column_names_string, ",")[[1]])

  # Remove empty column names
  column_names <- column_names[column_names != ""]

  if (length(column_names) == 0) {
    stop("No valid column names found in the input string")
  }

  # Create directory if it doesn't exist
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }

  # Create the full file path
  file_path <- file.path(directory, filename)

  # Create Excel workbook and worksheet
  tryCatch({
    # Create workbook
    wb <- openxlsx::createWorkbook()

    # Add worksheet
    openxlsx::addWorksheet(wb, sheet_name)

    # Write column names to row 2, starting from column A
    openxlsx::writeData(wb, sheet_name, t(column_names), startCol = 1, startRow = 2, colNames = FALSE)

    # Save the workbook
    openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

    message("Excel file created successfully: ", file_path)
    message("Sheet: ", sheet_name)
    message("Columns: ", paste(column_names, collapse = ", "))
    invisible(file_path)

  }, error = function(e) {
    stop("Error creating Excel file: ", e$message)
  })
}

#' Add New Sheet to Existing Excel File
#'
#' Adds a new worksheet to an existing Excel file with specified column names.
#' The columns will be placed starting from row 2, column A.
#'
#' @param excel_file_path Character string. Full path to the existing Excel file.
#' @param sheet_name Character string. Name of the new worksheet to add.
#' @param column_names_string Character string. Comma-separated column names (e.g., "Name,Age,City").
#' @return Character string. Path to the modified Excel file (invisible).
#' @export
#' @examples
#' \dontrun{
#' add_excel_new_sheet("C:/Data/myfile.xlsx", "NewSheet", "Col1,Col2,Col3")
#' }
add_excel_new_sheet <- function(excel_file_path, sheet_name, column_names_string) {
  # Check if openxlsx package is available
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required but not installed. Please install it using: install.packages('openxlsx')")
  }

  # Validate inputs
  if (!is.character(excel_file_path) || length(excel_file_path) != 1) {
    stop("excel_file_path must be a single character string")
  }

  if (!is.character(sheet_name) || length(sheet_name) != 1) {
    stop("sheet_name must be a single character string")
  }

  if (!is.character(column_names_string) || length(column_names_string) != 1) {
    stop("column_names_string must be a single character string")
  }

  # Check if Excel file exists
  if (!file.exists(excel_file_path)) {
    stop("Excel file does not exist: ", excel_file_path)
  }

  # Clean up the column names string (remove extra spaces)
  column_names_string <- trimws(column_names_string)

  if (nchar(column_names_string) == 0) {
    stop("column_names_string cannot be empty")
  }

  # Split the comma-separated string into individual column names
  column_names <- trimws(strsplit(column_names_string, ",")[[1]])

  # Remove empty column names
  column_names <- column_names[column_names != ""]

  if (length(column_names) == 0) {
    stop("No valid column names found in the input string")
  }

  # Load existing workbook and add new sheet
  tryCatch({
    # Load existing workbook
    wb <- openxlsx::loadWorkbook(excel_file_path)

    # Add new worksheet
    openxlsx::addWorksheet(wb, sheet_name)

    # Write column names to row 2, starting from column A
    openxlsx::writeData(wb, sheet_name, t(column_names), startCol = 1, startRow = 2, colNames = FALSE)

    # Save the workbook
    openxlsx::saveWorkbook(wb, excel_file_path, overwrite = TRUE)

    message("New sheet added successfully to: ", excel_file_path)
    message("Sheet: ", sheet_name)
    message("Columns: ", paste(column_names, collapse = ", "))
    invisible(excel_file_path)

  }, error = function(e) {
    stop("Error adding sheet to Excel file: ", e$message)
  })
}

# Update - 12/Oct/2025
# - Add setup_excel_file()
# - Add add_excel_new_sheet()
# - Set up protocols for adding CAMNMonitorTracking.xlsx and respective sheets to the program.
# - Add check_excel_file() to validate the structure of CAMNMonitorTracking.xlsx
# - Removed WEEKLY_QUALTRICS_TEMPLATE_CL_BLOCKID, WEEKLY_QUALTRICS_TEMPLATE_PA_BLOCKID from .Renviron
# - Removed:
# + WEEKLY_QUALTRICS_TEMPLATE_PA_FLOWID_BRANCH="FL_10"
# + WEEKLY_QUALTRICS_TEMPLATE_PA_FLOWID_BLOCK="FL_11"
# + WEEKLY_QUALTRICS_TEMPLATE_CL_FLOWID_BRANCH="FL_4"
# + WEEKLY_QUALTRICS_TEMPLATE_CL_FLOWID_BLOCK="FL_6"
