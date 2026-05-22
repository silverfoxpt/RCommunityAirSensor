test_that(".send_email_smtp2go_request builds the expected request and returns the response body", {
  captured <- new.env(parent = emptyenv())

  result <- .send_email_smtp2go_request(
    payload = list(
      sender = "sender@example.com",
      to = list("recipient@example.com"),
      subject = "Test Email",
      text_body = "This is a test message"
    ),
    api_key = "test-api-key",
    request_fun = function(url) {
      captured$url <- url
      list()
    },
    req_method_fun = function(req, method) {
      captured$method <- method
      req
    },
    req_headers_fun = function(req, ...) {
      captured$headers <- list(...)
      req
    },
    req_body_json_fun = function(req, body) {
      captured$body <- body
      req
    },
    req_perform_fun = function(req) {
      captured$performed <- TRUE
      list(response = list(message = "sent", id = 123))
    },
    resp_body_json_fun = function(resp) {
      resp$response
    }
  )

  expect_equal(captured$url, "https://api.smtp2go.com/v3/email/send")
  expect_equal(captured$method, "POST")
  expect_equal(
    captured$headers,
    list(
      "Content-Type" = "application/json",
      "X-Smtp2go-Api-Key" = "test-api-key",
      "accept" = "application/json"
    )
  )
  expect_equal(
    captured$body,
    list(
      sender = "sender@example.com",
      to = list("recipient@example.com"),
      subject = "Test Email",
      text_body = "This is a test message"
    )
  )
  expect_true(isTRUE(captured$performed))
  expect_equal(result, list(message = "sent", id = 123))
})

test_that(".send_email_smtp2go_request propagates request failures", {
  expect_error(
    .send_email_smtp2go_request(
      payload = list(sender = "sender@example.com"),
      api_key = "test-api-key",
      request_fun = function(url) list(),
      req_method_fun = function(req, method) req,
      req_headers_fun = function(req, ...) req,
      req_body_json_fun = function(req, body) req,
      req_perform_fun = function(req) {
        stop("simulated request failure")
      },
      resp_body_json_fun = function(resp) resp
    ),
    "simulated request failure"
  )
})

with_mocked_send_email_helper <- function(mock, code) {
  # Temporarily swap the internal helper so the exported wrappers stay isolated.
  namespace <- asNamespace("testPackage")
  original <- get(".send_email_smtp2go_request", envir = namespace)

  unlockBinding(".send_email_smtp2go_request", namespace)
  assign(".send_email_smtp2go_request", mock, envir = namespace)
  lockBinding(".send_email_smtp2go_request", namespace)

  on.exit({
    unlockBinding(".send_email_smtp2go_request", namespace)
    assign(".send_email_smtp2go_request", original, envir = namespace)
    lockBinding(".send_email_smtp2go_request", namespace)
  }, add = TRUE)

  force(code)
}

test_that("send_email_smtp2go forwards plain email payload to the helper", {
  captured <- new.env(parent = emptyenv())

  with_mocked_send_email_helper(
    function(payload, api_key, ...) {
      captured$payload <- payload
      captured$api_key <- api_key
      list(message = "sent")
    },
    {
      result <- send_email_smtp2go(
        api_key = "test-api-key",
        sender = "sender@example.com",
        recipient = "recipient@example.com",
        subject = "Test Email",
        text_body = "This is a test message"
      )

      expect_equal(
        captured$payload,
        list(
          sender = "sender@example.com",
          to = list("recipient@example.com"),
          subject = "Test Email",
          text_body = "This is a test message"
        )
      )
      expect_equal(captured$api_key, "test-api-key")
      expect_equal(result, list(message = "sent"))
    }
  )
})

test_that("send_email_smtp2go_attachment forwards attachment payload to the helper", {
  captured <- new.env(parent = emptyenv())
  # Create a real attachment path so the helper can base64-encode it.
  attachment_path <- tempfile(fileext = ".txt")
  writeLines("Attachment body", attachment_path)

  with_mocked_send_email_helper(
    function(payload, api_key, ...) {
      captured$payload <- payload
      captured$api_key <- api_key
      list(message = "sent with attachment")
    },
    {
      result <- send_email_smtp2go_attachment(
        api_key = "test-api-key",
        sender = "sender@example.com",
        recipient = "recipient@example.com",
        subject = "Report",
        text_body = "Please find the report attached",
        attachment_path = attachment_path
      )

      expect_equal(
        captured$payload,
        list(
          sender = "sender@example.com",
          to = "recipient@example.com",
          subject = "Report",
          text_body = "Please find the report attached",
          attachments = list(
            list(
              filename = basename(attachment_path),
              fileblob = base64enc::base64encode(attachment_path),
              mimetype = mime::guess_type(attachment_path)
            )
          )
        )
      )
      expect_equal(captured$api_key, "test-api-key")
      expect_equal(result, list(message = "sent with attachment"))
    }
  )
})