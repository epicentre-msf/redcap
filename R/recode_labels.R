#' Convert between values and labels for factor-type variables (e.g. yes/no,
#' radio, dropdown, checkbox)
#'
#' @inheritParams fetch_records
#'
#' @param x Data frame representing a REDCap form (e.g. from a previous export
#'   using [`fetch_records`])
#' @param dict REDCap metadata dictionary. Defaults to fetching the current
#'   version with [`meta_dictionary`]
#' @param convert_to Convert values to labels ("labels") or labels to values
#'   ("values")
#' @param types Types of REDCap variables to convert, based on column
#'   "field_type" in the metadata dictionary. Defaults to `c("radio", "yesno",
#'   "dropdown", "checkbox")`.
#' @param header_labs Logical indicating whether column names of `x` are labels
#'   (TRUE) or raw variable names (FALSE). Default assumes header has raw
#'   variable names (i.e. FALSE).
#'
#' @export recode_labels
recode_labels <- function(x,
                          conn,
                          dict = redcap::meta_dictionary(conn, add_complete = TRUE),
                          convert_to = c("labels", "values"),
                          types = c("radio", "yesno", "dropdown", "checkbox"),
                          header_labs = FALSE) {

  convert_to <- match.arg(convert_to, c("labels", "values"))

  # which dictionary column corresponds to colnames of x?
  col_field <- ifelse(header_labs, "field_label", "field_name")

  # subset dictionary to variables in x
  dict_foc <- dict[dict[[col_field]] %in% names(x), , drop = FALSE]

  # convert redcap_event_name
  cols_event <- ifelse(header_labs, "Event Name", "redcap_event_name")
  cols_event <- intersect(cols_event, names(x))

  if (length(cols_event) > 0 && cols_event %in% names(x)) {

    df_events <- redcap::meta_events(conn)

    events_from <- ifelse(convert_to == "labels", "unique_event_name", "event_name")
    events_to <- ifelse(convert_to == "labels", "event_name", "unique_event_name")

    x <- cols_recode(
      x,
      cols_event,
      dict_from = df_events[[events_from]],
      dict_to = df_events[[events_to]]
    )
  }

  # convert redcap_repeat_instrument
  cols_instrument <- ifelse(header_labs, "Repeat Instrument", "redcap_repeat_instrument")
  cols_instrument <- intersect(cols_instrument, names(x))

  if (length(cols_instrument) > 0 && cols_instrument %in% names(x)) {

    df_forms <- redcap::meta_forms(conn)

    instrument_from <- ifelse(convert_to == "labels", "instrument_name", "instrument_label")
    instrument_to <- ifelse(convert_to == "labels", "instrument_label", "instrument_name")

    x <- cols_recode(
      x,
      cols_instrument,
      dict_from = df_forms[[instrument_from]],
      dict_to = df_forms[[instrument_to]]
    )
  }

  # convert other list-type variables
  df_factors <- prep_meta_factors(dict, types)

  factors_from <- ifelse(convert_to == "labels", "value", "label")
  factors_to <- ifelse(convert_to == "labels", "label", "value")

  cols_factor <- intersect(names(x), df_factors[[col_field]])

  x <- cols_recode(
    x,
    cols_factor,
    df_factors[[col_field]],
    df_factors[[factors_from]],
    df_factors[[factors_to]]
  )

  # return
  x
}



#' @noRd
#' @importFrom dplyr recode
#' @importFrom rlang `!!!`
cols_recode <- function(x, cols, dict_vars = NULL, dict_from, dict_to) {
  for (j in cols) {
    if (is.null(dict_vars)) {
      from_j <- dict_from
      to_j <- dict_to
    } else {
      from_j <- dict_from[dict_vars %in% j]
      to_j <- dict_to[dict_vars %in% j]
    }

    x[[j]] <- dplyr::recode(x[[j]], !!!setNames(to_j, from_j))
  }

  x
}

