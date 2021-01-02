
#' Enclose a string in a special character (e.g. brackets, backticks), if not
#' already enclosed
#'
#' @param x A string to enclose
#' @param l Character symbol to add on left side
#' @param r Character symbol to add on right
#'
#' @noRd
enclose <- function(x, l, r) {
  reg_l <- paste0("^\\", l)
  reg_r <- paste0("\\", r, "$")
  already_enclosed <- (grepl(reg_l, x) & grepl(reg_r, x)) | is.na(x)
  x[!already_enclosed] <- paste0(l, x[!already_enclosed], r)
  x
}


#' Remove enclosing backticks
#'
#' @noRd
untick <- function(x) {
   gsub("^\\`|\\`$", "", x)
}


#' Remove enclosing parentheses (if only 1 set)
#'
#' @noRd
unparens <- function(x) {
  single_set_parens <- grepl("^\\(", x) & string_count(x, "\\(") == 1 &
    grepl("\\)$", x) & string_count(x, "\\)") == 1
  x[single_set_parens] <- gsub("^\\(|\\)$", "", x[single_set_parens])
  x
}


#' Quick version of stringr::str_squish to avoid dependency
#'
#' @noRd
string_squish <- function(x) {
  x <- gsub("[[:space:]]+", " ", x)
  x <- gsub("^[[:space:]]|[[:space:]]$", "", x)
  x
}


#' Quick version of stringr::str_count to avoid dependency
#'
#' @noRd
string_count <- function(string, pattern) {
  lengths(regmatches(string, gregexpr(pattern, string)))
}


#' @noRd
test_valid <- function(arg, options) {
  arg_name <- deparse(substitute(arg))
  non_matching <- setdiff(arg, options)

  if (length(non_matching) > 0) {
    stop(
      "The following ", arg_name, " were not found: ",
      paste(non_matching, collapse = "; "),
      call. = FALSE
    )
  }
}


#' @noRd
#' @importFrom httr POST content
#' @importFrom readr cols col_character
post_wrapper <- function(conn, body = NULL, content = NULL, on_error = "fail") {

  if (!is_rconn(conn)) {
    stop("Argument conn must be a REDCap connection created with rconn()")
  }

  on_error <- match.arg(on_error, c("fail", "null"))

  if (is.null(body)) {
    body <- list(
      token = conn$token,
      content = content,
      format = "csv",
      returnFormat = "json"
    )
  }

  response <- httr::POST(
    conn$url,
    body = body,
    config = conn$config,
    encode = "form"
  )

  if (response$status_code != 200L) {
    if (on_error == "fail") {
      stop(httr::content(response)[[1]], call. = FALSE)
    } else {
      out <- NULL
    }
  } else {
    out <- httr::content(
      response,
      col_types = readr::cols(.default = readr::col_character())
    )
  }

  out
}


#' @noRd
#' @importFrom lubridate as_date as_datetime
reclass <- function(x, dict, use_factors, value_labs, header_labs) {

  if (FALSE) {
    x <- out
    dict <- m_dict
  }

  # which dictionary column corresponds to colnames of x?
  col_field <- ifelse(header_labs, "field_label", "field_name")

  # subset dictionary to variables in x
  dict_foc <- dict[dict[[col_field]] %in% names(x), , drop = FALSE]

  # numeric variables
  is_num <- grepl("number|integer", dict_foc$validation) | dict_foc$field_type == "calc"
  cols_num <- dict_foc[[col_field]][is_num]
  cols_num <- setdiff(cols_num, dict[[col_field]][1]) # remove ID col
  x <- cols_reclass(x, cols_num, as.numeric)

  # date variables
  cols_date <- dict_foc[[col_field]][grepl("date_", dict_foc$validation)]
  x <- cols_reclass(x, cols_date, lubridate::as_date)

  # datetime variables
  cols_datetime <- dict_foc[[col_field]][grepl("datetime_", dict_foc$validation)]
  x <- cols_reclass(x, cols_datetime, lubridate::as_datetime)

  # repeat instance column to integer
  cols_instance <- ifelse(header_labs, "Repeat Instance", "redcap_repeat_instance")
  cols_instance <- intersect(cols_instance, names(x))
  x <- cols_reclass(x, cols_instance, as.integer)

  # factors
  if (use_factors) {
    types <- c("radio", "yesno", "dropdown", "checkbox")
    df_factors <- prep_meta_factors(dict, types)
    col_level <- ifelse(value_labs, "label", "value")
    cols_factor <- intersect(names(x), df_factors[[col_field]])
    x <- cols_factorize(x, cols_factor, df_factors, col_field, col_level)
  }

  # return
  x
}


#' @noRd
cols_reclass <- function(x, cols, fun, ...) {
  fun <- match.fun(fun)
  for (i in seq_along(cols)) {
    x[[cols[i]]] <- fun(x[[cols[i]]], ...)
  }
  x
}


#' @noRd
cols_factorize <- function(x, cols, df_factors, col_field, col_level) {
  for (i in seq_along(cols)) {
    col_focal <- cols[i]
    levels_focal <- df_factors[[col_level]][df_factors[[col_field]] %in% col_focal]
    x[[col_focal]] <- factor(x[[col_focal]], levels = levels_focal)
  }
  x
}


#' @noRd
split_choices <- function(x) {
  strsplit(x, "[[:space:]]*\\|[[:space:]]*")[[1]]
}


#' @noRd
split_value_label <- function(x, i) {
  strsplit(x, "\\,[[:space:]]*")[[1]][i]
}

