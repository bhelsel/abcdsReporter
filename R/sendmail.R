#' @title Send an Email with Text and a gt Table
#'
#' @description
#' A wrapper around the \code{\link[bhelselR]{sendmail}} function that retrieves
#' SMTP host and port information from the user's R environment and attempts to
#' send an email containing formatted text and a \code{\link[gt]{gt}} table.
#'
#' @param sender A character vector of length two specifying the name and email
#' address of the sender (e.g., \code{c("Brian Helsel", "bhelsel@kumc.edu")}).
#' @param recipients A named list of recipients, where each list element name is
#' the recipient’s first name and the value is their email address
#' (e.g., \code{list("Brian" = "bhelsel@kumc.edu", "Jane" = "jdoe@kumc.edu")}).
#' @param subject A brief string providing the subject line for the email.
#' @param text The name of the HTML file (without extension) located in the
#' \code{html} folder of the \code{abcds} package that serves as the message body
#' (e.g., \code{"CoenrollTable"}).
#' @param table A \code{\link[gt]{gt}} table that has been converted to raw HTML
#' using \code{\link[gt]{as_raw_html}} for inclusion in the email body.
#'
#' @return
#' An alert message printed to the console from \code{\link[bhelselR]{sendmail}}
#' indicating whether the email was sent successfully.
#'
#' @details
#' The function simplifies email generation for ABC-DS reporting workflows. It
#' automatic

send_email <- function(sender, recipients, subject, text, table) {
  email_host <- Sys.getenv("EMAIL_HOST")
  email_port <- Sys.getenv("EMAIL_PORT")
  text <- system.file(sprintf("emails/%s.html", text), package = "abcds")
  has_names <- !is.null(names(recipients))
  recipient_names <- has_names %?% collapse_and(names(recipients)) %:% "Team"
  recipient_emails <- unname(unlist(recipients))
  tryCatch(
    {
      bhelselR::sendmail(
        host = email_host,
        port = email_port,
        sender = sender,
        recipients = recipient_emails,
        subject = subject,
        body = glue::glue(paste0(readLines(text), collapse = "")),
        html = TRUE
      )
      symbol_bell <- "\U1F514"
      cli::cli_text(
        "{symbol_bell} Email was successfully sent to {recipient_names}"
      )
    },
    error = function(e) {
      symbol_cross <- "\u274C"
      cli::cli_text(
        "{symbol_cross} Email did not send: {conditionMessage(e)}"
      )
    }
  )

  return(invisible(NULL))
}
