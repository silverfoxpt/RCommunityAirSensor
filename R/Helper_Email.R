#' Send Email via Gmail SMTP
#'
#' Sends an email using Gmail's SMTP server with optional file attachments.
#' Requires Gmail credentials stored in environment variables.
#'
#' @param Sender Character string of sender's email address
#' @param Recipients Character vector of recipient email addresses
#' @param Subject Character string of email subject
#' @param Body Character string of email body content
#' @param AttachmentPaths Character vector of file paths to attach
#' @param AttachmentNames Character vector of attachment display names
#'
#' @return NULL (function called for side effects)
#'
#' @details
#' Requires GMAIL_USERNAME and GMAIL_APP_ACCOUNT environment variables.
#' Uses SSL connection on port 465.
#'
#' @examples
#' \dontrun{
#' send_email_gmail(
#'   Sender = "sender@gmail.com",
#'   Recipients = c("recipient@example.com"),
#'   Subject = "Test Email",
#'   Body = "This is a test message",
#'   AttachmentPaths = c("/path/to/file.csv"),
#'   AttachmentNames = c("data.csv")
#' )
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
send_email_gmail <- function(Sender, Recipients, Subject, Body, AttachmentPaths, AttachmentNames) {
  mailR::send.mail(
    from = Sender,
    to = Recipients,
    subject = Subject,
    body = Body,
    smtp = list(
      host.name = "smtp.gmail.com",
      port = 465,
      user.name = Sys.getenv("GMAIL_USERNAME"),
      passwd = Sys.getenv("GMAIL_APP_ACCOUNT"),
      ssl = TRUE
    ),
    authenticate = TRUE,
    send = TRUE,
    attach.files = AttachmentPaths,
    file.names = AttachmentNames,
    debug = F
  )
}

#' Send Email via SMTP2GO API
#'
#' Sends a plain text email using the SMTP2GO API service.
#' Provides reliable email delivery without SMTP server configuration.
#'
#' @param api_key Character string containing SMTP2GO API key
#' @param sender Character string of sender's email address
#' @param recipient Character string of recipient email address
#' @param subject Character string of email subject
#' @param text_body Character string of plain text email body
#'
#' @return List containing API response with delivery status
#'
#' @examples
#' \dontrun{
#' response <- send_email_smtp2go(
#'   api_key = "your_api_key",
#'   sender = "sender@example.com",
#'   recipient = "recipient@example.com",
#'   subject = "Test Email",
#'   text_body = "This is a test message"
#' )
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
send_email_smtp2go <- function(api_key, sender, recipient, subject, text_body) {
  url <- "https://api.smtp2go.com/v3/email/send"

  req <- httr2::request(url) %>%
    httr2::req_method("POST") %>%
    httr2::req_headers(
      "Content-Type" = "application/json",
      "X-Smtp2go-Api-Key" = api_key,
      "accept" = "application/json"
    ) %>%
    httr2::req_body_json(list(
      sender = sender,
      to = list(recipient),
      subject = subject,
      text_body = text_body
    ))

  resp <- httr2::req_perform(req)
  return(httr2::resp_body_json(resp))
}

#' Send Email with Attachment via SMTP2GO API
#'
#' Sends an email with a single file attachment using the SMTP2GO API.
#' Automatically encodes the file in Base64 and determines MIME type.
#'
#' @param api_key Character string containing SMTP2GO API key
#' @param sender Character string of sender's email address
#' @param recipient Character string of recipient email address
#' @param subject Character string of email subject
#' @param text_body Character string of plain text email body
#' @param attachment_path Character string path to file to attach
#'
#' @return List containing API response with delivery status
#'
#' @details
#' File is automatically Base64 encoded and MIME type is detected.
#' Only supports single attachment per email.
#'
#' @examples
#' \dontrun{
#' response <- send_email_smtp2go_attachment(
#'   api_key = "your_api_key",
#'   sender = "sender@example.com",
#'   recipient = "recipient@example.com",
#'   subject = "Report",
#'   text_body = "Please find the report attached",
#'   attachment_path = "/path/to/report.pdf"
#' )
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
send_email_smtp2go_attachment <- function(api_key, sender, recipient, subject, text_body, attachment_path) {
  url <- "https://api.smtp2go.com/v3/email/send"

  # Read file and encode in Base64
  attachment_name <- basename(attachment_path)
  file_content <- base64enc::base64encode(attachment_path)
  mimetype <- mime::guess_type(attachment_path)

  req <- httr2::request(url) %>%
    httr2::req_method("POST") %>%
    httr2::req_headers(
      "Content-Type" = "application/json",
      "X-Smtp2go-Api-Key" = api_key,
      "accept" = "application/json"
    ) %>%
    httr2::req_body_json(list(
      sender = sender,
      to = recipient,
      subject = subject,
      text_body = text_body,
      attachments = list(
        list(
          filename = attachment_name,
          fileblob = file_content,
          mimetype = mimetype
        )
      )
    ))

  resp <- httr2::req_perform(req)
  return(httr2::resp_body_json(resp))
}

# Update 22/10/2025:
# - Add httr2::, mime:: base64enc:: prefixes to function calls
# - Add roxygen2 comments

