#' Fetch the REDCap database version used for a particular project
#'
#' Execute an "Export REDCap version" API request
#'
#' @inheritParams fetch_records
#'
#' @return
#' A character string
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' redcap_version(conn)
#' }
#'
#' @export redcap_version
redcap_version <- function(conn) {

  names(
    post_wrapper(
      conn,
      body = NULL,
      content = "version",
      on_error = "fail"
    )
  )
}
