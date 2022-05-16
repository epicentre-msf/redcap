#' Export REDCap user information
#'
#' Execute an "Export Users" API request to fetch user-related details (e.g.
#' username, email, access permissions, etc.) corresponding to a REDCap project.
#'
#' @inheritParams fetch_records
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame containing the columns returned
#' by an "Export Users" API request
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' project_users(conn)
#' }
#'
#' @export project_users
project_users <- function(conn) {

  body <- list(
    token = conn$token,
    content = "user",
    format = "csv",
    returnFormat = "json"
  )

  post_wrapper(
    conn,
    body = body,
    on_error = "fail"
  )
}
