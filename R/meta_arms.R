#' Export REDCap project arms
#'
#' Execute an "Export Arms" API request to fetch the "arms" (number and name)
#' associated with a REDCap project.
#'
#' @inheritParams fetch_records
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame with 2 columns:
#' - `arm_num`
#' - `name`
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' meta_arms(conn)
#' }
#'
#' @export meta_arms
meta_arms <- function(conn) {

  body <- list(
    token = conn$token,
    content = "arm",
    format = "csv",
    returnFormat = "json"
  )

  post_wrapper(
    conn,
    body = body,
    on_error = "fail"
  )
}
