#' Convert a REDCap project XML file to a tidy data frame of project records
#'
#' Extract records from a REDCap project XML file (e.g. returned by
#' [`project_xml`]) and assemble into a tidy long-form data frame, with one row
#' for each combination of record x field x event x instance.
#'
#' @param x A REDCap project XML obect of class
#'   [`xml_document`][xml2::xml_document-class], e.g. returned by
#'   [`project_xml`]
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame with 6 columns:
#' - `record_id`
#' - `form`
#' - `redcap_event`
#' - `redcap_repeat_instance`
#' - `field`
#' - `value`
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' parse_xml(project_xml(conn))
#' }
#'
#' @importFrom purrr map_dfr
#' @importFrom xml2 xml_find_all xml_ns
#' @export parse_xml
parse_xml <- function(x) {

  purrr::map_dfr(
    xml2::xml_find_all(x, "//d1:SubjectData"),
    participant_to_df,
    ns = xml2::xml_ns(x)
  )
}



#' @noRd
#' @importFrom xml2 xml_attr xml_find_all
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate `%>%`
#' @importFrom rlang .env .data
participant_to_df <- function(x, ns) {

  record_id <- xml2::xml_attr(x, "SubjectKey", ns = ns)
  x_events <- xml2::xml_find_all(x, ".//d1:StudyEventData", ns = ns)

  purrr::map_dfr(x_events, event_to_df, ns = ns) %>%
    dplyr::mutate(record_id = .env$record_id, .before = 1)
}


#' @noRd
#' @importFrom xml2 xml_attr xml_find_all
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate `%>%`
#' @importFrom rlang .env .data
event_to_df <- function(x, ns) {

  event <- xml2::xml_attr(x, "StudyEventOID", ns = ns)
  event <- gsub("^Event\\.", "", event)

  x_forms <- xml2::xml_find_all(x, ".//d1:FormData", ns = ns)

  purrr::map_dfr(x_forms, form_to_df, ns = ns) %>%
    dplyr::mutate(form = gsub("^Form\\.", "", .data$form)) %>%
    dplyr::mutate(redcap_event = .env$event, .after = "form")
}


#' @noRd
#' @importFrom xml2 xml_find_all xml_attr
#' @importFrom dplyr tibble
form_to_df <- function(x, ns) {

  item_data <- xml2::xml_find_all(x, ".//d1:ItemData", ns = ns)

  dplyr::tibble(
    form = xml2::xml_attr(x, "FormOID", ns = ns),
    redcap_repeat_instance = xml2::xml_attr(x, "FormRepeatKey", ns = ns),
    field = xml2::xml_attr(item_data, "ItemOID", ns = ns),
    value = xml2::xml_attr(item_data, "Value", ns = ns)
  )
}

