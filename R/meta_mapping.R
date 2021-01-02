#' Fetch instrument-event mappings for a REDCap project
#'
#' Execute an "Export Instrument-Event Mappings" API request to fetch a
#' [`tibble`][tibble::tbl_df]-style data frame containing the mapping between
#' instruments and events. Note that this request type is not available for
#' 'classic projects', from which event details cannot be exported.
#'
#' @inheritParams fetch_records
#' @inheritParams meta_events
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame with 3 columns:
#' - `arm_num`
#' - `unique_event_name`
#' - `form`
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' meta_mapping(conn)
#' }
#'
#' @export meta_mapping
meta_mapping <- function(conn, on_error = "fail") {

  post_wrapper(
    conn,
    body = NULL,
    content = "formEventMapping",
    on_error = on_error
  )
}

