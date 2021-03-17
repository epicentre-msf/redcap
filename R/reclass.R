#' Reclass columns of a data frame to match classes specified in a metadata
#' dictionary
#'
#' @inheritParams fetch_records
#'
#' @param x A data frame representing a REDCap form
#' @param dict A metadata dictionary
#'
#' @importFrom lubridate as_date as_datetime
#' @export reclass
reclass <- function(x,
                    dict,
                    use_factors = FALSE,
                    value_labs = TRUE,
                    header_labs = FALSE) {

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

  # time variables
  cols_time <- dict_foc[[col_field]][grepl("^time$", dict_foc$validation)]
  x <- cols_reclass(x, cols_time, parse_redcap_time)

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
