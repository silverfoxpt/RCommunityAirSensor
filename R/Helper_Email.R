send_email_gmail <- function(Sender, Recipients, Subject, Body, AttachmentPaths, AttachmentNames) {
  send.mail(
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

send_email_smtp2go <- function(api_key, sender, recipient, subject, text_body) {
  library(httr2)
  library(jsonlite)

  url <- "https://api.smtp2go.com/v3/email/send"

  req <- request(url) %>%
    req_method("POST") %>%
    req_headers(
      "Content-Type" = "application/json",
      "X-Smtp2go-Api-Key" = api_key,
      "accept" = "application/json"
    ) %>%
    req_body_json(list(
      sender = sender,
      to = list(recipient),
      subject = subject,
      text_body = text_body
    ))

  resp <- req_perform(req)
  return(resp_body_json(resp))
}

send_email_smtp2go_attachment <- function(api_key, sender, recipient, subject, text_body, attachment_path) {
  library(httr2)
  library(jsonlite)
  library(base64enc)

  url <- "https://api.smtp2go.com/v3/email/send"

  # Read file and encode in Base64
  attachment_name <- basename(attachment_path)
  file_content <- base64encode(attachment_path)
  mimetype <- mime::guess_type(attachment_path)

  req <- request(url) %>%
    req_method("POST") %>%
    req_headers(
      "Content-Type" = "application/json",
      "X-Smtp2go-Api-Key" = api_key,
      "accept" = "application/json"
    ) %>%
    req_body_json(list(
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

  resp <- req_perform(req)
  return(resp_body_json(resp))
}

