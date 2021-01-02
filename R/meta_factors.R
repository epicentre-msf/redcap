#' Fetch field option values and labels for factor-type variables in a REDCap
#' project
#'
#' @description
#' Converts field options and labels from the metadata dictionary (see
#' [`meta_dictionary`]), which has 1 row per variable and field options in
#' compact string-form, to long form with 1 row per option. E.g.:
#'
#' __Dictionary version__ (1 row per variable):
#'
#' | `field_name` | `form_name` | `field_type` | `choices` |
#' | ------------ | ----------- | ------------ | --------- |
#' | consented | enrollment | radio | 0, No \| 1, Yes |
#'
#' __Long format__ (1 row per option):
#'
#' | `field_name` | `form_name` | `field_type` | `value` | `label` |
#' | ------------ | ----------- | ------------ | ------- | ------- |
#' | consented | enrollment | radio | 0 | No |
#' | consented | enrollment | radio | 1 | Yes |
#'
#' @inheritParams fetch_records
#' @inheritParams meta_dictionary
#'
#' @param forms Character vector of forms (i.e. instruments) to include. Set to
#'   `NULL` (the default) to return field options for all forms in the project.
#' @param types Character vector of variable types to return field options for.
#'   \cr Defaults to `c("radio", "yesno", "dropdown", "checkbox")`.
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame with 6 columns:
#'
#' - field_name
#' - form_name
#' - field_type
#' - field_label
#' - value
#' - label
#'
#' @seealso [`meta_dictionary`]
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' meta_factors(conn)
#' }
#'
#' @export meta_factors
meta_factors <- function(conn,
                         forms = NULL,
                         expand_checkbox = TRUE,
                         types = c("radio", "yesno", "dropdown", "checkbox")) {

  dict <- meta_dictionary(conn, forms = forms, expand_checkbox = expand_checkbox)
  prep_meta_factors(dict, types = types)
}


#' @importFrom tidyr unnest
#' @importFrom dplyr as_tibble
prep_meta_factors <- function(dict, types) {

  # filter dict to relevant variable types
  radio <- dict[dict$field_type %in% types,]

  # populated choice for yesno variables
  radio$choices[radio$field_type %in% "yesno"] <- "0, No | 1, Yes"

  # cols to return
  cols_return <- c(
    "field_name",
    "form_name",
    "field_type",
    "field_label",
    "value",
    "label"
  )

  if (nrow(radio) > 0) {

    # split choices into vector (1 element/row per choice) and unnest
    radio$choices_split <- lapply(radio$choices, split_choices)
    radio_unnest <- tidyr::unnest(radio, "choices_split")

    # separate value/label
    radio_unnest$value <- vapply(
      radio_unnest$choices_split,
      split_value_label,
      "",
      i = 1,
      USE.NAMES = FALSE
    )

    radio_unnest$label <- vapply(
      radio_unnest$choices_split,
      split_value_label,
      "",
      i = 2,
      USE.NAMES = FALSE
    )

    # return
    out <- radio_unnest[, cols_return, drop = FALSE]

  } else {

    out <- dplyr::as_tibble(
      vapply(cols_return, function(x) character(0), character(0))
    )
  }

  out
}

