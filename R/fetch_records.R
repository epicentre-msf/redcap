#' Fetch records for a REDCap project
#'
#' @description
#' Execute an "Export Records" API request to fetch a
#' [`tibble`][tibble::tbl_df]-style data frame containing records for one or
#' more REDCap instruments.
#'
#' @param conn A REDCap API connection object (created with [`rconn`])
#' @param forms Character vector of forms (i.e. instruments) to fetch data for.
#'   Set to `NULL` (the default) to fetch all forms in the project.
#' @param events Character vector of events to fetch. Must correspond to the
#'   selected `forms`. Set to `NULL` (the default) to fetch all events
#'   corresponding to the selected form(s).
#' @param records Character vector of record IDs to fetch. Set to `NULL` (the
#'   default) to fetch all record IDs corresponding to the selected form(s).
#' @param records_omit Character vector of record IDs to ignore. Set to `NULL`
#'   (the default) to _not_ ignore any records. If a given record ID appears in
#'   both argument `records` and `records_omit`, argument `records_omit` takes
#'   precedence and that record will not be returned.
#' @param fields Character vector of fields (i.e. variables) to fetch. Set to
#'   `NULL` (the default) to fetch all fields corresponding to the selected
#'   form(s).
#' @param id_field Logical indicating whether to always include the 'record ID'
#'   field (defined in REDCap to be the first variable in the project codebook)
#'   in the API request, even if it's not specified in argument `fields`.
#'   Defaults to `TRUE`.
#'
#'   The record ID field is defined within the first form of a REDCap project,
#'   and so API requests for other forms will not include the record ID field by
#'   default (unless it's explicitly requested with argument `fields`). The
#'   `id_field` argument is a shortcut to avoid having to always explicitly
#'   request the record ID field.
#' @param rm_empty Logical indicating whether to remove rows for which all
#'   fields from the relevant form(s) are missing. See section __Removing empty
#'   rows__. Defaults to `TRUE`.
#' @param value_labs Logical indicating whether to return value labels (`TRUE`)
#'   or raw values (`FALSE`) for categorical REDCap variables (radio, dropdown,
#'   yesno, checkbox). Defaults to `TRUE` to return labels.
#' @param header_labs Logical indicating whether to export column names as
#'   labels (`TRUE`) or raw variable names (`FALSE`). Defaults to `FALSE` to
#'   return raw variable names.
#' @param checkbox_labs Logical indicating whether to export checkbox labels
#'   (`TRUE`) or statuses (i.e. "Unchecked" or "Checked") (`FALSE`). Defaults to
#'   `FALSE` to export statuses. Note this argument is only relevant when
#'   `value_labs` is `TRUE` — if `value_labs` is `FALSE` checkbox variables will
#'   always be exported as raw values (usually "0"/"1").
#' @param use_factors Logical indicating whether categorical REDCap variables
#'   (radio, dropdown, yesno, checkbox) should be returned as factors. Factor
#'   levels can either be raw values (e.g. "0"/"1") or labels (e.g. "No"/"Yes")
#'   depending on arguments `value_labs` and `checkbox_labs`. Defaults to
#'   `FALSE`.
#' @param na Character vector of strings to interpret as missing values. Passed
#'   to [readr::read_csv]. Defaults to `c("", "NA")`.
#' @param dag Logical indicating whether to export the
#'   `redcap_data_access_group` field (if used in the project). Defaults to
#'   `TRUE`.
#' @param double_resolve Logical indicating whether to resolve double-entries
#'   (i.e. records entered in duplicate using REDCap's Double Data Entry
#'   module), by filtering to the lowest entry number associated with each
#'   unique record.
#'
#'   If a project uses double-entry, the record IDs returned by an "Export
#'   Records" API request will be a concatenation of the normal record ID and
#'   the entry number (1 or 2), normally separated by "--" (e.g. "P0285--1"). To
#'   resolve double entries we move the entry number portion of the ID to its
#'   own column (`entry`), identify all entries belonging to the same unique
#'   record, and retain only the row with the lowest entry number for each
#'   unique record.
#'
#'   Unique records are identified using the record ID column (after separating
#'   the entry number portion), and any of the following columns when present
#'   (accounting for argument `header_labs`): redcap_event_name (Redcap Event),
#'   redcap_repeat_instrument (Repeat Instrument), redcap_repeat_instance
#'   (Repeat Instance).
#' @param double_sep If `double_resolve` is `TRUE`, the string separator used to
#'   split the record ID field into the record ID and entry number. Defaults to
#'   "--".
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame containing the requested
#' records
#'
#' @section Removing empty rows:
#' Depending on the database design, an "Export Records" API request can
#' sometimes return empty rows, representing forms for which no data has been
#' collected. For example, if forms __F1__ and __F2__ are part of the same
#' event, and participant "P001" has form data for __F2__ but not __F1__, an API
#' request for __F1__ will include a row for participant "P001" where all
#' __F1__-specific fields are empty.
#'
#' If argument `rm_empty` is `TRUE` (the default), `fetch_records()` will filter
#' out such rows. The check for empty rows is based only on fields that are
#' specific to the form(s) specified in argument `forms` — i.e. it excludes the
#' record ID field, and generic fields like `redcap_event_name`,
#' `redcap_data_access_group`, etc. The check for empty rows also accounts for
#' checkbox fields, which, if argument `checkbox_labs` is `FALSE`, will be set
#' to "Unchecked" in an empty form (rather than missing per se).
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' fetch_records(conn, forms = "my_form")
#' }
#'
#' @export fetch_records
fetch_records <- function(conn,
                          forms = NULL,
                          events = NULL,
                          records = NULL,
                          records_omit = NULL,
                          fields = NULL,
                          id_field = TRUE,
                          rm_empty = TRUE,
                          value_labs = TRUE,
                          header_labs = FALSE,
                          checkbox_labs = FALSE,
                          use_factors = FALSE,
                          na = c("", "NA"),
                          dag = TRUE,
                          double_resolve = FALSE,
                          double_sep = "--") {

  ## fetch metadata (dictionary, instruments, repeat instr, event mapping) -----
  m_dict <- meta_dictionary(conn)
  m_instr <- meta_forms(conn)
  m_events <- meta_events(conn, on_error = "null")
  m_repeat <- suppressWarnings(meta_repeating(conn, on_error = "null"))
  m_mapping <- meta_mapping(conn, on_error = "null")

  ## fetch records -------------------------------------------------------------
  # the use of the lower-level fn fetch_records_ is to enable vectorization over
  # forms in fetch_database() without having to repeatedly fetch the same
  # metadata tables for each separate form
  fetch_records_(
    conn = conn,
    forms = forms,
    events = events,
    records = records,
    records_omit = records_omit,
    fields = fields,
    id_field = id_field,
    rm_empty = rm_empty,
    value_labs = value_labs,
    header_labs = header_labs,
    checkbox_labs = checkbox_labs,
    use_factors = use_factors,
    na = na,
    dag = dag,
    double_resolve = double_resolve,
    double_sep = double_sep,
    m_dict = m_dict,
    m_instr = m_instr,
    m_events = m_events,
    m_repeat = m_repeat,
    m_mapping = m_mapping
  )
}



#' @noRd
#' @importFrom dplyr left_join
fetch_records_ <- function(conn,
                           forms,
                           events,
                           records,
                           records_omit,
                           fields,
                           id_field,
                           rm_empty,
                           value_labs,
                           header_labs,
                           checkbox_labs,
                           use_factors,
                           na,
                           dag,
                           double_resolve,
                           double_sep,
                           m_dict,
                           m_instr,
                           m_events,
                           m_repeat,
                           m_mapping) {

  ## argument validation -------------------------------------------------------

  # forms
  test_valid(forms, m_instr$instrument_name)
  if (is.null(forms)) forms <- unique(m_instr$instrument_name)

  # events (note: events metadata not available for classic projects)
  if (!is.null(m_mapping)) {
    events_for_forms <- m_mapping$unique_event_name[m_mapping$form %in% forms]
    test_valid(events, events_for_forms)
    if (is.null(events)) events <- events_for_forms
  }

  # fields
  test_valid(fields, m_dict$field_name)

  # add ID field
  name_id_field <- m_dict$field_name[1]
  if (id_field & !name_id_field %in% fields) fields <- c(name_id_field, fields)

  ## prepare request -----------------------------------------------------------
  body <- list(
    token = conn$token,
    content = "record",
    format = "csv",
    type = "flat",
    csvDelimiter = "",
    forms = paste(forms, collapse = ","),
    events = paste(events, collapse = ","),
    rawOrLabel = ifelse(value_labs, "label", "raw"),
    rawOrLabelHeaders = ifelse(header_labs, "label", "raw"),
    exportCheckboxLabel = tolower(checkbox_labs),
    exportDataAccessGroups = tolower(dag),
    returnFormat = "csv"
  )

  # add records and fields, if given
  if (!is.null(records)) body[["records"]] <- paste(records, collapse = ",")
  if (!is.null(fields)) body[["fields"]] <- paste(fields, collapse = ",")

  ## fetch ---------------------------------------------------------------------
  out <- post_wrapper(
    conn,
    body = body,
    content = NULL,
    na = na,
    on_error = "fail"
  )

  ## filter to selected redcap_repeat_instance ---------------------------------

  # prepare df identifying expected repeat instruments for given events
  if (!is.null(m_repeat)) {

    col_repeat <- ifelse(header_labs, "Repeat Instrument", "redcap_repeat_instrument")
    col_event <- ifelse(header_labs, "Event Name", "redcap_event_name")

    m_repeat_join <- unique(m_repeat[m_repeat$form_name %in% forms,])
    m_repeat_join$keep_repeat_instr <- TRUE

    if (nrow(m_repeat_join) > 0 & all(c(col_repeat, col_event) %in% names(out))) {
      if (value_labs) {
        m_repeat_join[[col_repeat]] <- recode_vec(
          m_repeat_join$form_name,
          m_instr$instrument_name,
          m_instr$instrument_label
        )
        m_repeat_join[[col_event]] <- recode_vec(
          m_repeat_join$event_name,
          m_events$unique_event_name,
          m_events$event_name
        )
      } else {
        m_repeat_join[[col_repeat]] <- m_repeat_join$form_name
        m_repeat_join[[col_event]] <- m_repeat_join$event_name
      }

      m_repeat_join <- m_repeat_join[,c(col_repeat, col_event, "keep_repeat_instr")]

      # join expected event/instrument combinations to form
      out <- dplyr::left_join(out, m_repeat_join, by = c(col_repeat, col_event))

      # filter form to expected even/instrument combinations
      rows_keep <- !is.na(out$keep_repeat_instr) | !out[[col_event]] %in% m_repeat_join[[col_event]]
      out <- out[rows_keep, !names(out) %in% "keep_repeat_instr", drop = FALSE]
    }
  }

  ## filter out records_omit ---------------------------------------------------
  if (!is.null(records_omit)) {
    rows_omit <- out[[name_id_field]] %in% records_omit
    out <- out[!rows_omit, , drop = FALSE]
  }

  ## filter out rows with all fields missing -----------------------------------
  if (rm_empty) {
    rows_missing <- all_fields_missing(
      x = out,
      dict = m_dict,
      forms = forms,
      value_labs = value_labs,
      header_labs = header_labs,
      checkbox_labs = checkbox_labs
    )

    out <- out[!rows_missing, , drop = FALSE]
  }

  ## resolve double-entry ------------------------------------------------------
  if (double_resolve & !name_id_field %in% names(out)) {
    warning(
      "Can't resolve double entries because return doesn't contain record ID field ",
      sQuote(name_id_field, q = FALSE), call. = FALSE
    )
  } else if (double_resolve) {
    out <- resolve_double_entry(
      x = out,
      header_labs = header_labs,
      name_id_field = name_id_field,
      double_sep = double_sep
    )
  }

  ## reclass columns and return ------------------------------------------------
  reclass(out, m_dict, use_factors, value_labs, header_labs)
}



#' @noRd
all_fields_missing <- function(x,
                               dict,
                               forms,
                               value_labs,
                               header_labs,
                               checkbox_labs) {

  col_field <- ifelse(header_labs, "field_label", "field_name")

  dict <- dict[-1, , drop = FALSE]
  dict_form <- dict[dict$form_name %in% forms, , drop = FALSE]

  # if value_labs = TRUE
  #  - checkbox_labs = TRUE, empty checkbox fields will be <NA>
  #  - checkbox_labs = FALSE, empty checkbox fields will be "Unchecked"
  # if value_labs = FALSE
  #  - empty checkbox fields will be "0"
  if (value_labs & checkbox_labs) {

    x_sub <- x[, names(x) %in% dict_form[[col_field]], drop = FALSE]
    rows_missing <- apply(x_sub, 1, function(x) all(is.na(x)))

  } else {

    missing_val <- ifelse(!value_labs, "0", "Unchecked")
    dict_form_check <- dict_form[dict_form$field_type %in% "checkbox", , drop = FALSE]
    dict_form_other <- dict_form[!dict_form$field_type %in% "checkbox", , drop = FALSE]

    x_sub_check <- x[,names(x) %in% dict_form_check[[col_field]], drop = FALSE]
    x_sub_other <- x[,names(x) %in% dict_form_other[[col_field]], drop = FALSE]

    rows_missing_checkbox <- apply(x_sub_check, 1, function(x) all(x %in% missing_val))
    rows_missing_other <- apply(x_sub_other, 1, function(x) all(is.na(x)))

    rows_missing <- rows_missing_checkbox & rows_missing_other
  }

  rows_missing
}



#' @importFrom rlang enquo `!!` `!!!` .data
#' @importFrom tidyr separate
#' @importFrom dplyr `%>%` group_by arrange slice ungroup
resolve_double_entry <- function(x,
                                 header_labs,
                                 name_id_field,
                                 double_sep) {

  # warn if ID column doesn't contain double_sep
  # may indicate double entries have been merged and resolved, or incorrect double_sep
  if (!any(grepl(double_sep, x[[name_id_field]]))) {
    warning(
      "Pattern 'double_sep' not found in any elements of record ID column ",
      sQuote(name_id_field, q = FALSE), ", so no double entries have been filtered",
      call. = FALSE
    )
  }

  # split record ID field into record ID + entry
  x_split <- tidyr::separate(
    x,
    col = !!enquo(name_id_field),
    into = c(name_id_field, "entry"),
    sep = double_sep,
    fill = "right"
  )

  # vec of grouping columns used to identify unique records
  group_cols <- if (header_labs) {
    c("Event Name", "Repeat Instrument", "Repeat Instance")
  } else {
    c("redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")
  }

  group_cols <- c(name_id_field, intersect(group_cols, names(x)))
  group_cols_sym <- lapply(group_cols, str2lang)

  # for each unique record, sort by duplicate entry anf
  x_split %>%
    dplyr::group_by(!!!group_cols_sym) %>%
    dplyr::arrange(.data$entry) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
}

