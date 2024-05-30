#' Export REDCap Project XML file
#'
#' Execute an "Export Entire Project as REDCap XML File" API request to fetch
#' all metadata (and optionally also data records) corresponding to a REDCap
#' project.
#'
#' @inheritParams fetch_records
#' @param meta_only Logical indicating whether to fetch only the project
#'   metadata (if `TRUE`) or both the metadata and data records (if `FALSE`).
#'   Defaults to `FALSE` to fetch both metadata and data.
#' @param records Optional character vector of specific record IDs to fetch
#'   record data for. Only used when `meta_only = FALSE`.
#' @param fields Optional character vector of specific fields to fetch record
#'   data for. Only used when `meta_only = FALSE`.
#' @param events Optional character vector of specific events to fetch record
#'   data for. Only used when `meta_only = FALSE`.
#' @param filter_logic Optional character string containing a REDCap-style
#'   expression used to filter records returned by the API (e.g. "\[age\] > 30")
#' @param export_dag Logical indicating whether to export the
#'   redcap_data_access_group field. Defaults to `FALSE`.
#' @param export_survey Logical indicating whether to export survey identifier
#'   or timestamp fields, if surveys are used in the project. Defaults to
#'   `FALSE`.
#' @param export_files Logical indicating whether to export uploaded files. Note
#'   this may lead to large exports. Defaults to `FALSE`.
#'
#' @return
#' An object of class [`xml_document`][xml2::xml_document-class]
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' project_xml(conn)
#' }
#'
#' @importFrom httr POST content
#' @importFrom xml2 read_xml
#' @export project_xml
project_xml <- function(conn,
                        meta_only = FALSE,
                        records = NULL,
                        fields = NULL,
                        events = NULL,
                        filter_logic = NULL,
                        export_dag = FALSE,
                        export_survey = FALSE,
                        export_files = FALSE) {


  # metadata
  m_dict <- meta_dictionary(conn)
  m_events <- meta_events(conn, on_error = "null")

  # validate fields
  # if (is.null(fields) & !is.null(forms)) {
  #   fields <- m_dict$field_name[m_dict$form_name %in% forms]
  #   # add ID field if necessary
  #   name_id_field <- m_dict$field_name[1]
  #   if (!name_id_field %in% fields) fields <- c(name_id_field, fields)
  # }

  test_valid(fields, "fields", m_dict$field_name)

  # validate events
  test_valid(events, "events", m_events$unique_event_name)


  body <- list(
    token = conn$token,
    content = "project_xml",
    returnMetadataOnly = tolower(meta_only),
    exportFiles = tolower(export_files),
    exportSurveyFields = tolower(export_survey),
    exportDataAccessGroups = tolower(export_dag),
    filterLogic = filter_logic,
    format = "xml",
    returnFormat = "json"
  )

  if (!is.null(records)) body <- c(body, post_array(records))
  if (!is.null(fields)) body <- c(body, post_array(fields))
  if (!is.null(events)) body <- c(body, post_array(events))

  response <- httr::POST(
    conn$url,
    body = body,
    config = conn$config,
    encode = "form"
  )

  if (response$status_code != 200L) {
    stop(httr::content(response)[[1]], call. = FALSE)
  } else {

    out <- xml2::read_xml(response)
  }

  out
}

