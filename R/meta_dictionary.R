#' Fetch variable dictionary for a REDCap project
#'
#' Execute an "Export Metadata (Data Dictionary)" API request to fetch a
#' [`tibble`][tibble::tbl_df]-style data frame containing the project codebook
#' (field names, types, labels, choices, validation, etc.).
#'
#' @inheritParams fetch_records
#'
#' @param forms Character vector of forms (i.e. instruments) to include in the
#'   return. Set to `NULL` (the default) to return dictionary entries for all
#'   forms in the project.
#' @param expand_checkbox Logical indicating whether to expand checkbox
#'   variables. Defaults to `TRUE`.
#'
#'   Unlike an "Export Records" API request (see [`fetch_records`]), which
#'   returns 1 column for each checkbox _option_, an "Export Metadata (Data
#'   Dictionary)" request returns a single row for each _field_ — including
#'   checkbox fields. Thus, the `field_name` and `field_label` entries for
#'   checkbox variables in the data dictionary will never exactly match the
#'   respective column names or values returned by [`fetch_records`].
#'
#'   When `expand_checkbox` is `TRUE`, rows for checkbox fields are expanded to
#'   1 row per checkbox _option_, so that dictionary entries for `field_name`,
#'   `field_label`, and `choices` will always match the relevant entries
#'   returned by [`fetch_records`].
#' @param add_complete Logical indicating whether to add "\{form\}_complete"
#'   fields to the dictionary, one for each form included in the return. These
#'   will be of field_type "radio" with possible choices "0, Incomplete | 1,
#'   Unverified | 2, Complete". Defaults to `FALSE`.
#' @param cols_omit Character vector of dictionary columns to omit from the
#'   return for brevity. Set to `NULL` to return all columns.
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame containing the project
#' dictionary. Note that some of the returned column names are shortened
#' versions of the original column names returned by the API:
#'
#' | __Original__ | __Returned__ |
#' | ------------ | ------------ |
#' |`select_choices_or_calculations` | `choices` |
#' |`text_validation_type_or_show_slider_number`&nbsp; | `validation` |
#' |`text_validation_min` | `validation_min` |
#' |`text_validation_max` | `validation_max` |
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' meta_dictionary(conn)
#' }
#'
#' @importFrom dplyr rename
#' @importFrom dplyr bind_rows
#' @export meta_dictionary
meta_dictionary <- function(conn,
                            forms = NULL,
                            expand_checkbox = TRUE,
                            add_complete = FALSE,
                            cols_omit =  c(
                              "section_header",
                              "custom_alignment",
                              "question_number",
                              "matrix_group_name",
                              "matrix_ranking"
                            )) {

  out <- post_wrapper(
    conn,
    body = NULL,
    content = "metadata",
    on_error = "fail"
  )

  if (!is.null(forms)) {
    m_instr <- meta_forms(conn)
    test_valid(forms, "forms", m_instr$instrument_name)
    out <- out[out$form_name %in% forms, , drop = FALSE]
  }

  if (add_complete) {
    out_split <- split(out, out$form_name)

    out <- dplyr::bind_rows(
      mapply(
        add_entry_complete,
        form = names(out_split),
        dict = out_split,
        SIMPLIFY = FALSE
      )
    )
  }

  if (!is.null(out)) {

    ## shorten names
    out <- dplyr::rename(
      out,
      "choices" = "select_choices_or_calculations",
      "validation" = "text_validation_type_or_show_slider_number",
      "validation_min" = "text_validation_min",
      "validation_max" = "text_validation_max"
    )

    ## expand checkbox
    if (expand_checkbox) {
      out <- expand_checkbox(out)
    }

    ## omit columns
    out <- out[,!names(out) %in% cols_omit, drop = FALSE]
  }

  out
}


#' @noRd
#' @importFrom dplyr `%>%` arrange mutate filter select bind_rows
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @importFrom purrr map map_chr
expand_checkbox <- function(x) {

  x$rowid <- seq_len(nrow(x))

  x_check <- x %>%
    dplyr::filter(.data$field_type %in% "checkbox") %>%
    dplyr::mutate(choices = purrr::map(.data$choices, split_choices)) %>%
    tidyr::unnest("choices") %>%
    dplyr::mutate(
      field_name_orig = .data$field_name,
      field_label_orig = .data$field_label,
      checkbox_value = purrr::map_chr(.data$choices, ~ strsplit(.x, ", *")[[1]][1]),
      checkbox_label = purrr::map_chr(.data$choices, ~ strsplit(.x, ", *")[[1]][2]),
      field_name = paste(.data$field_name, tolower(.data$checkbox_value), sep = "___"),
      field_label = paste0(.data$field_label, " (choice=", .data$checkbox_label, ")"),
      choices = "0, Unchecked | 1, Checked"
    )

  x %>%
    dplyr::filter(!.data$field_type %in% "checkbox") %>%
    dplyr::bind_rows(x_check) %>%
    dplyr::arrange(.data$rowid) %>%
    dplyr::select(-c("rowid", "checkbox_value", "checkbox_label"))
}



#' @noRd
#' @importFrom dplyr tibble
#' @importFrom dplyr bind_rows
add_entry_complete <- function(form, dict) {

  entry_add <- dplyr::tibble(
    field_name = paste0(form, "_complete"),
    form_name = form,
    field_type = "radio",
    section_header = "Form Status",
    field_label = "Complete?",
    select_choices_or_calculations = "0, Incomplete | 1, Unverified | 2, Complete"
  )

  dplyr::bind_rows(dict, entry_add)
}
