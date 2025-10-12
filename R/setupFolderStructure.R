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
    list(directory = file.path(directory, "CSV/Exports"), filename = "QualtricsUpdateLog.csv", columns = "OriginDate,Action,SaveData"),
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
#' @return Logical. TRUE if the exact folder structure and files exist, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' check_folder_and_file_structure("C:/MyProject")
#' }
check_folder_and_file_structure <- function(directory) {
  # Validate input
  if (!is.character(directory) || length(directory) != 1) {
    stop("directory must be a single character string")
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
      return(FALSE)
    }
  }
  
  # Define required CSV files with their expected column structures
  required_csv_files <- list(
    "CSV/Exports/ClarityLog.csv" = c("OriginDate", "Complete"),
    "CSV/Exports/PurpleAirLog.csv" = c("OriginDate", "Complete"),
    "CSV/Exports/QualtricsMonthlyLog.csv" = c("OriginDate", "Action", "SaveData"),
    "CSV/Exports/QualtricsUpdateLog.csv" = c("OriginDate", "Action", "SaveData"),
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
        return(FALSE)
      }
    }, error = function(e) {
      # If we can't read the file, consider it invalid
      return(FALSE)
    })
  }
  
  return(TRUE)
}

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
    
    # Write column names to row 1, starting from column A
    openxlsx::writeData(wb, sheet_name, t(column_names), startCol = 1, startRow = 1, colNames = FALSE)
    
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
