#' Delete records from a REDCap project
#'
#' @inheritParams fetch_records
#'
#' @param records Character vector of record IDs to delete
#'
#' @return
#' An integer, the number of records successfully deleted
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' # delete all records associated with IDs "P004" and "P007"
#' delete_records(conn, records = c("P004", "P007"))
#' }
#'
#' @importFrom stats setNames
#' @importFrom httr POST
#' @export delete_records
delete_records <- function(conn, records) {

  records_index <- paste0("records", "[", seq_along(records) - 1L, "]")

  body <- c(
    list(
      token = conn$token,
      content = "record",
      action = "delete"
    ),
    stats::setNames(records, records_index)
  )

  response <- httr::POST(
    conn$url,
    body = body,
    config = conn$config,
    encode = "form"
  )

  if (response$status_code != 200L) {
    stop(as.character(response), call. = FALSE)
  } else {
    return(as.integer(as.character(response)))
  }
}
