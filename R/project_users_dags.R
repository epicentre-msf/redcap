#' Export REDCap mapping between users and Data Access Groups (DAGs)
#'
#' Execute an "Export User-DAG Assignments" API request to fetch the mapping
#' between users and Data Access Groups (DAGs) associated with a REDCap project.
#'
#' @inheritParams fetch_records
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame with 2 columns:
#' - `username`
#' - `redcap_data_access_group`
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' project_users_dags(conn)
#' }
#'
#' @export project_users_dags
project_users_dags <- function(conn) {

  body <- list(
    token = conn$token,
    content = "userDagMapping",
    format = "csv",
    returnFormat = "json"
  )

  post_wrapper(
    conn,
    body = body,
    on_error = "fail"
  )
}
