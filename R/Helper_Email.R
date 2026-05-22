#' Build and send an SMTP2GO request.
#'
#' Internal helper that centralizes request construction so both exported email
#' helpers share the same request path and can be tested with injected stubs.
#'
#' @param payload Named list containing the SMTP2GO request body.
#' @param api_key Character string containing SMTP2GO API key.
#' @param request_fun Function used to create the request object.
#' @param req_method_fun Function used to set the HTTP method.
#' @param req_headers_fun Function used to add request headers.
#' @param req_body_json_fun Function used to attach the JSON body.
#' @param req_perform_fun Function used to perform the request.
#' @param resp_body_json_fun Function used to parse the response body.
#'
#' @return Parsed response body returned by SMTP2GO.
#'
#' @noRd
.send_email_smtp2go_request <- function(payload, api_key,
                                        request_fun = httr2::request,
                                        req_method_fun = httr2::req_method,
                                        req_headers_fun = httr2::req_headers,
                                        req_body_json_fun = httr2::req_body_json,
                                        req_perform_fun = httr2::req_perform,
                                        resp_body_json_fun = httr2::resp_body_json) {
  url <- "https://api.smtp2go.com/v3/email/send"

  req <- request_fun(url) %>%
    req_method_fun("POST") %>%
    req_headers_fun(
      "Content-Type" = "application/json",
      "X-Smtp2go-Api-Key" = api_key,
      "accept" = "application/json"
    ) %>%
    req_body_json_fun(payload)

  resp <- req_perform_fun(req)
  resp_body_json_fun(resp)
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
  .send_email_smtp2go_request(list(
      sender = sender,
      to = list(recipient),
      subject = subject,
      text_body = text_body
    ), api_key = api_key)
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
  # Read the attachment locally so the request payload stays deterministic.
  attachment_name <- basename(attachment_path)
  file_content <- base64enc::base64encode(attachment_path)
  mimetype <- mime::guess_type(attachment_path)

  .send_email_smtp2go_request(list(
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
    ), api_key = api_key)
}

# Update 22/10/2025:
# - Add httr2::, mime:: base64enc:: prefixes to function calls
# - Add roxygen2 comments

