#' Export project logging (audit trail) for a REDCap project
#'
#' Execute an "Export Logging" API request to export the logging (audit trail)
#' of all changes made to the project, including data exports, data changes,
#' project metadata changes, modification of user rights, etc.
#'
#' @inheritParams fetch_records
#'
#' @param type Type of logging to return. Defaults to NULL to return all types.
#'   Specific logging types include:
#'   - "export": Data export
#'   - "manage": Manage/Design
#'   - "user": User or role created-updated-deleted
#'   - "record": Record created-updated-deleted
#'   - "record_add": Record created (only)
#'   - "record_edit": Record updated (only)
#'   - "record_delete": Record deleted (only)
#'   - "lock_record": Record locking & e-signatures
#'   - "page_view": Page Views
#'
#' @param user REDCap username to fetch logs for. Defaults to NULL to fetch logs
#'   relating to all users. Note that, in the current API version (10.8.5), it's
#'   not possible to pass a character vector with multiple usernames (i.e. one
#'   user per request, or NULL for all).
#' @param record Record ID to fetch logs for. Defaults to NULL to fetch logs
#'   relating to all record IDs. Note that, in the current API version (10.8.5),
#'   it's not possible to pass a character vector with multiple record IDs (i.e.
#'   one record per request, or NULL for all).
#' @param datetime_start Fetch logs from *after* a given date-time. Use format
#'   "YYYY-MM-DD HH:MM". Defaults to NULL to omit a lower time limit.
#' @param datetime_end Fetch logs from *before* a given date-time. Use format
#'   "YYYY-MM-DD HH:MM". Defaults to NULL to omit an upper time limit.
#' @param datetime_to_posix Logical indicating whether to convert the
#'   `timestamp` column to class POSIXct using [`lubridate::as_datetime`].
#'   Defaults to TRUE.
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame with 4 columns:
#' - `timestamp`
#' - `username`
#' - `actions`
#' - `details`
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' project_logging(conn)
#' }
#'
#' @importFrom lubridate parse_date_time
#' @export project_logging
project_logging <- function(conn,
                            type = NULL,
                            user = NULL,
                            record = NULL,
                            datetime_start = NULL,
                            datetime_end = NULL,
                            datetime_to_posix = TRUE) {

  # validate args
  if (!is.null(type)) {
    type <- match.arg(
      type,
      c(
        "export",
        "manage",
        "user",
        "record",
        "record_add",
        "record_edit",
        "record_delete",
        "lock_record",
        "page_view"
      )
    )
  }

  if (length(user) > 1) {
    stop("Argument `user` must be of length 1")
  }

  if (length(record) > 1) {
    stop("Argument `record` must be of length 1")
  }

  body <- list(
    token = conn$token,
    content = "log",
    logtype = type,
    user = user,
    record = record,
    beginTime = datetime_start,
    endTime = datetime_end,
    format = "csv",
    returnFormat = "json"
  )

  out <- post_wrapper(
    conn,
    body = body,
    on_error = "fail"
  )

  if (datetime_to_posix & is.data.frame(out)) {
    out$timestamp <- lubridate::parse_date_time(out$timestamp, orders = "Ymd HM")
  }

  out
}

