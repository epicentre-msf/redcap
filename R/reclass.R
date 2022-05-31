#' Reclass columns of a data frame to match classes specified in a metadata
#' dictionary
#'
#' @inheritParams fetch_records
#'
#' @param x A data frame representing a REDCap form
#' @param dict A metadata dictionary
#' @param fn_dates Function to parse REDCap date variables. Defaults to
#'   `parse_date`, an internal wrapper to [`lubridate::parse_date_time`]. If
#'   date variables have been converted to numeric (e.g. by writing to Excel),
#'   set to e.g. [`lubridate::as_date`] to convert back to dates.
#' @param fn_datetimes Function to parse REDCap datetime variables. Defaults to
#'   [`lubridate::as_datetime`].
#'
#' @importFrom lubridate as_datetime
#' @export reclass
reclass <- function(x,
                    dict,
                    use_factors = FALSE,
                    value_labs = TRUE,
                    header_labs = FALSE,
                    times_chron = TRUE,
                    fn_dates = parse_date,
                    fn_datetimes = lubridate::as_datetime) {

  fn_dates <- match.fun(fn_dates)
  fn_datetimes <- match.fun(fn_datetimes)

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
  x <- cols_reclass(x, cols_date, fn_dates)

  # datetime variables
  cols_datetime <- dict_foc[[col_field]][grepl("datetime_", dict_foc$validation)]
  x <- cols_reclass(x, cols_datetime, fn_datetimes)

  # time variables
  cols_time <- dict_foc[[col_field]][grepl("^time$", dict_foc$validation)]
  x <- cols_reclass(x, cols_time, parse_redcap_time)
  if (!times_chron) x <- cols_reclass(x, cols_time, prep_redcap_time)

  # repeat instrument column to character
  cols_instrument <- ifelse(header_labs, "Repeat Instrument", "redcap_repeat_instrument")
  cols_instrument <- intersect(cols_instrument, names(x))
  x <- cols_reclass(x, cols_instrument, as.character)

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
#' @importFrom lubridate as_date parse_date_time
parse_date <- function(x, orders = c("Ymd", "dmY")) {
  lubridate::as_date(
    lubridate::parse_date_time(x, orders = orders)
  )
}

