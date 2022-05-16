#' Export REDCap user information
#'
#' Execute an "Export Data Access Groups (DAGs)" API request to fetch the DAGs
#' (labels and code names) associated with a REDCap project.
#'
#' @inheritParams fetch_records
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame with 2 columns:
#' - `data_access_group_name`
#' - `unique_group_name`
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' project_dags(conn)
#' }
#'
#' @export project_dags
project_dags <- function(conn) {

  body <- list(
    token = conn$token,
    content = "dag",
    format = "csv",
    returnFormat = "json"
  )

  post_wrapper(
    conn,
    body = body,
    on_error = "fail"
  )
}
