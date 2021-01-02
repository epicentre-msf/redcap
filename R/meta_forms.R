#' Fetch instrument names and labels for a REDCap project
#'
#' Execute an "Export Instrument (Data Entry Forms)" API request to fetch a
#' [`tibble`][tibble::tbl_df]-style data frame containing instrument names and
#' labels.
#'
#' @inheritParams fetch_records
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
#' meta_forms(conn)
#' }
#'
#' @export meta_forms
meta_forms <- function(conn) {

  post_wrapper(
    conn,
    body = NULL,
    content = "instrument",
    on_error = "fail"
  )
}

