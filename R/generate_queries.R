#' Generate data validation queries for a REDCap project based on branching
#' logic specified in the project codebook
#'
#' @description
#' Generates two types of data validation queries using the project codebook
#' (see [`meta_dictionary`]) and [`translate_logic`]:
#'
#' 1. __Field missing__: Branching logic evaluates to `TRUE` (if specified), but
#' field is missing. By default only applies to required fields (`required_field
#' == "y"`) (can modify with argument `non_required`).
#'
#' 2. __Field not missing__: Branching logic evaluates to `FALSE` but field is
#' not missing. Applies to any field with branching logic.
#'
#' @inheritParams fetch_records
#' @inheritParams translate_logic
#'
#' @param forms Character vector of forms (i.e. instruments) to include. Set to
#'   `NULL` (the default) to generate queries for all forms in the project.
#' @param dict Metadata dictionary. By default is fetched automatically with
#'   [`meta_dictionary`], but it's included as an argument here to allow the
#'   user to modify the dictionary before passing to `generate_queries` (e.g. to
#'   correct bugs in branching logic). If passing a modified version, make sure
#'   it is initially fetched with argument `expand_checkbox = FALSE`.
#' @param lang Query language, either English ("en") or French ("fr"). Defaults
#'   to "en".
#' @param query_types Which type of queries to generate (see __Description__
#'   above). Options are "missing", "not missing", or "both". Defaults to
#'   "both".
#' @param non_required Logical indicating whether to include non-required fields
#'   in queries of type "Field missing". Defaults to `FALSE`.
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame specifying queries, with the
#' following 7 columns:
#'
#' \describe{
#'   \item{query_id}{Unique query identifier based on form name and integer sequence}
#'   \item{field_name}{Field name (from REDCap dictionary, see [`meta_dictionary`])}
#'   \item{form_name}{Form name (from REDCap dictionary)}
#'   \item{required}{Is it a required field in REDCap dictionary ("y" or `<NA>`) ?}
#'   \item{description}{Description of query (e.g. "Missing: \[Signed consent forms?\]")}
#'   \item{suggestion}{Suggestion for query resolution. A human-readable
#'   translation of query expression (e.g. If \[Is the participant 18 years or
#'   older?\] is "Yes", item \[Signed consent forms?\] should not be missing)}
#'   \item{branching_logic}{Branching logic for given field (from REDCap dictionary)}
#'   \item{query}{R-style query expression (can be evaluated with [`queryr::query`])}
#' }
#'
#' @importFrom dplyr `%>%` filter mutate select left_join if_else n rename
#'   arrange all_of bind_rows group_by ungroup
#' @importFrom rlang .data .env
#' @export generate_queries
generate_queries <- function(conn,
                             forms = NULL,
                             dict = meta_dictionary(
                               conn,
                               forms = forms,
                               expand_checkbox = FALSE
                             ),
                             lang = "en",
                             query_types = "both",
                             non_required = FALSE,
                             drop_redundant = FALSE,
                             on_error = "warn") {

  ## validate argument lang
  lang <- match.arg(lang, c("en", "fr"))

  ## validate argument forms
  m_instr <- meta_forms(conn)

  if (!is.null(forms)) {
    test_valid(forms, m_instr$instrument_name)
  } else {
    forms <- m_instr$instrument_name
  }

  ## validate argument query_types
  query_types <- match.arg(query_types, c("missing", "not missing", "both"))

  ## fetch metadata dictionary
  dict$field_label <- string_squish(dict$field_label)
  dict_check <- expand_checkbox(dict)

  ## fetch metadata exported fields
  exported_fields <- meta_fields(conn)

  ## derive long-form data frame of factor options from metadata dictionary
  types <- c("radio", "yesno", "dropdown", "checkbox")
  fact_check <- prep_meta_factors(dict_check, types = types)

  ## prep logic-portion of query expressions (i.e. branching_logic)
  fields_logic <- c(
    "field_name",
    "form_name",
    "field_type",
    "field_label",
    "branching_logic",
    "required_field"
  )

  rows_logic <- dict$form_name %in% forms & !dict$field_type %in% "descriptive"

  q_logic <- dict[rows_logic, fields_logic]

  q_logic$logic_base <- wrap_parens(
    translate_logic(
      unparens(q_logic$branching_logic),
      use_value_labs = TRUE,
      use_header_labs = FALSE,
      use_is_na = TRUE,
      use_in = TRUE,
      drop_redundant = FALSE,
      meta_factors = fact_check,
      meta_dictionary = NULL,
      on_error = on_error
    )
  )

  q_logic$logic_base_text <- translate_human(
    translate_logic(
      q_logic$branching_logic,
      use_value_labs = TRUE,
      use_header_labs = TRUE,
      use_is_na = FALSE,
      use_in = FALSE,
      drop_redundant = drop_redundant,
      meta_factors = fact_check,
      meta_dictionary = dict_check,
      on_error = "ignore"
    ),
    lang = lang
  )

  ## prep field-portion of query expressions (i.e. 'Is missing' or 'Not missing')
  # For non-checkbox variables use:
  # - is.na(var)
  # - !is.na(var)
  # For checkbox variables use:
  # - var___1 %in% "Unchecked" & var___2 %in% "Unchecked" ...
  # - var___1 %in% "Checked" | var___2 %in% "Checked" ...
  q_field <- exported_fields %>%
    dplyr::left_join(dict, by = c("original_field_name" = "field_name")) %>%
    dplyr::select(
      field_name = .data$original_field_name,
      .data$export_field_name,
      .data$form_name,
      .data$field_type
    ) %>%
    dplyr::mutate(
      var_missing = dplyr::if_else(
        .data$field_type %in% "checkbox",
        paste0(.data$export_field_name, " %in% 'Unchecked'"),
        paste0("is.na(", .data$export_field_name, ")")
      ),
      var_not_missing = dplyr::if_else(
        .data$field_type %in% "checkbox",
        paste0(.data$export_field_name, " %in% 'Checked'"),
        paste0("!is.na(", .data$export_field_name, ")")
      ),
    ) %>%
    dplyr::group_by(
      .data$field_name, .data$field_type
    ) %>%
    dplyr::summarize(
      var_missing = paste(.data$var_missing, collapse = " & "),
      var_not_missing = paste(.data$var_not_missing, collapse = " | "),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      var_missing = dplyr::if_else(
        .data$field_type %in% "checkbox",
        paste0("(", .data$var_missing, ")"),
        .data$var_missing
      ),
      var_not_missing = dplyr::if_else(
        .data$field_type %in% "checkbox",
        paste0("(", .data$var_not_missing, ")"),
        .data$var_not_missing
      )
    )

  ## join q_logic and q_field
  q_full <- q_logic %>%
    dplyr::left_join(q_field, by = c("field_name", "field_type")) %>%
    dplyr::mutate(rownumber = seq_len(dplyr::n()))

  ## queries for var missing when should not be
  lab_missing_pre <- ifelse(
    lang == "fr",
    "Manquant: ",
    "Missing: "
  )
  lab_not_missing_pre <- ifelse(
    lang == "fr",
    "Renseign\U00E9: ",
    "Not missing: "
  )
  lab_missing_suf <- ifelse(
    lang == "fr",
    " devrait \U00EAtre renseign\U00E9",
    " should not be missing"
  )
  lab_not_missing_suf <- ifelse(
    lang == "fr",
    " ne devrait pas \U00EAtre renseign\U00E9",
    " should be missing"
  )
  lab_if <- ifelse(
    lang == "fr",
    "Si ",
    "If "
  )
  lab_unless <- ifelse(
    lang == "fr",
    "Sauf si ",
    "Unless "
  )
  lab_item <- ifelse(
    lang == "fr",
    "L'\U00E9l\U00E9ment ",
    "Item "
  )
  lab_item_middle <- ifelse(
    lang == "fr",
    ", l'\U00E9l\U00E9ment ",
    ", item "
  )
  lab_not_missing_mid <- ifelse(
    lang == "fr",
    " ne devrait \U00EAtre renseign\U00E9 que si ",
    " should only be filled if "
  )

  if (query_types %in% c("missing", "both")) {

    if (non_required) {
      req_fields <- c("y", NA_character_)
    } else {
      req_fields <- "y"
    }

    q_missing <- q_full %>%
      dplyr::filter(.data$required_field %in% req_fields) %>%
      dplyr::mutate(
        query_type = "Missing",
        query = dplyr::case_when(
          !is.na(.data$branching_logic) & is.na(.data$logic_base) ~ NA_character_,
          is.na(.data$logic_base) ~ .data$var_missing,
          TRUE ~ paste(.data$logic_base, .data$var_missing, sep = " & ")
        ),
        description = paste0(
          .env$lab_missing_pre, enclose(.data$field_label, l = "[", r = "]")
        ),
        suggestion = dplyr::if_else(
          is.na(.data$branching_logic),
          paste0(
            .env$lab_item, enclose(.data$field_label, l = "[", r = "]"), .env$lab_missing_suf
          ),
          paste0(
            .env$lab_if, .data$logic_base_text, .env$lab_item_middle,
            enclose(.data$field_label, l = "[", r = "]"), .env$lab_missing_suf
          )
        )
      )
  } else {
    q_missing <- NULL
  }

  ## queries for var not missing when should be
  if (query_types %in% c("not missing", "both")) {
    q_not_missing <- q_full %>%
      dplyr::filter(!is.na(.data$logic_base)) %>%
      dplyr::mutate(
        query_type = "Not missing",
        query = paste0("!", wrap_parens(.data$logic_base), " & ", .data$var_not_missing),
        description = paste0(.env$lab_not_missing_pre, enclose(.data$field_label, l = "[", r = "]")),
        suggestion = paste0(
          .env$lab_item,
          enclose(.data$field_label, l = "[", r = "]"),
          .env$lab_not_missing_mid,
          .data$logic_base_text
        )
      )
  } else {
    q_not_missing <- NULL
  }

  ## combine and return
  dplyr::bind_rows(q_missing, q_not_missing) %>%
    dplyr::rename("required" = "required_field") %>%
    dplyr::arrange(.data$rownumber, .data$query_type) %>%
    dplyr::group_by(.data$form_name) %>%
    dplyr::mutate(query_id = formatC(seq_len(dplyr::n()), width = 3, flag = "0")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(query_id = paste(.data$form_name, .data$query_id, sep = "__")) %>%
    dplyr::select(
      dplyr::all_of(
        c(
          "query_id",
          "field_name",
          "form_name",
          "required",
          "description",
          "suggestion",
          "branching_logic",
          "query"
        )
      )
    )
}



#' @noRd
translate_human <- function(x, lang = "en") {

  lang <- match.arg(lang, c("en", "fr"))

  x <- gsub("\"\\[", "[", x)
  x <- gsub("\\]\"", "]", x)

  if (lang == "fr") {
    x <- gsub(" == ", " est ", x)
    x <- gsub(" != ", " n'est pas ", x)
    x <- gsub(" \\& ", " et ", x)
    x <- gsub(" \\| ", " ou ", x)
    x <- gsub("\"\"", "manquant", x)
  } else {
    x <- gsub(" == ", " is ", x)
    x <- gsub(" != ", " is not ", x)
    x <- gsub(" \\& ", " and ", x)
    x <- gsub(" \\| ", " or ", x)
    x <- gsub("\"\"", "missing", x)
  }

  x
}
