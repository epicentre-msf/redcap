#' Export REDCap Project information
#'
#' Execute an "Export Project Info" API request to fetch project-related details
#' (e.g. title, creation time, production status, language, etc.) corresponding
#' to a REDCap project.
#'
#' @inheritParams fetch_records
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame containing the columns returned
#' by an "Export Project Info" API request
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' project_info(conn)
#' }
#'
#' @export project_info
project_info <- function(conn) {

  body <- list(
    token = conn$token,
    content = "project",
    format = "csv",
    returnFormat = "json"
  )

  post_wrapper(
    conn,
    body = body,
    on_error = "fail"
  )
}
