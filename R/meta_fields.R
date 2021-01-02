#' Fetch exported field names for a REDCap project
#'
#' @description
#' Execute an "Export List of Export Field Names" API request to fetch a
#' [`tibble`][tibble::tbl_df]-style data frame containing field (i.e. variable)
#' names as listed in the project codebook (column `original_field_name`) and
#' the corresponding exported variable name(s) (column `export_field_name`).
#'
#' Original and exported field names will be identical except in the case of
#' checkbox-type variables. A given checkbox variable (e.g. "patient_status")
#' will have a single entry in the codebook (i.e. `field_name` =
#' "patient_status"), but will be exported as multiple variables — one for each
#' possible choice value.
#'
#' | `original_field_name` | `choice_value` | `export_field_name` |
#' | --------------------- | -------------- | ------------------- |
#' | patient_status | 1 | patient_status___1 |
#' | patient_status | 2 | patient_status___2 |
#' | patient_status | 3 | patient_status___3 |
#' | patient_status | 88 | patient_status___88 |
#'
#' @inheritParams fetch_records
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame with 3 columns:
#' - `original_field_name`
#' - `choice_value`
#' - `export_field_name`
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' meta_fields(conn)
#' }
#'
#' @export meta_fields
meta_fields <- function(conn) {

  post_wrapper(
    conn,
    body = NULL,
    content = "exportFieldNames",
    on_error = "fail"
  )
}

