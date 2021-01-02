#' Fetch repeating instrument names and labels for a REDCap project
#'
#' Execute an "Export Repeating Instruments and Events" API request to fetch a
#' [`tibble`][tibble::tbl_df]-style data frame containing repeating instrument
#' names and labels.
#'
#' @inheritParams fetch_records
#' @inheritParams meta_events
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame with 2 columns:
#' - `instrument_name`
#' - `instrument_label`
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' meta_repeating(conn)
#' }
#'
#' @export meta_repeating
meta_repeating <- function(conn, on_error = "fail") {

  post_wrapper(
    conn,
    body = NULL,
    content = "repeatingFormsEvents",
    on_error = on_error
  )
}

