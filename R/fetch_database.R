#' Fetch records from multiple REDCap forms, returning separate list elements
#' for each form
#'
#' Wrapper to [`fetch_records`] that's vectorized over forms (i.e. instruments).
#' Returns a list whose elements are [`tibble`][tibble::tbl_df]-style data
#' frames corresponding to each requested form.
#'
#' @inheritParams fetch_records
#'
#' @param names_fn Function for creating custom list element names given a
#'   vector of form names. Defaults to an identity function in which case
#'   element names will correspond exactly to form names.
#' @param fns Optional list of one or more functions to apply to each list
#'   element (i.e. each form). Could be used e.g. to filter out record IDs from
#'   test entries, create derived variables, etc. Each function should take a
#'   data frame returned by [`fetch_records`] as its first argument.
#'
#' @return
#' A list of [`tibble`][tibble::tbl_df]-style data frames corresponding to each
#' of the requested forms.
#'
#' @inheritSection fetch_records Removing empty rows
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' fetch_database(
#'   conn,
#'   forms = c("my_form1", "my_form2", "my_form3")
#' )
#'
#' # use a custom fn to format the 'participant_id' column of each form
#' # the function must take a data frame as its first argument
#' format_ids <- function(x) {
#'   x$participant_id <- toupper(x$participant_id)
#'   x$participant_id <- gsub("[^[:alnum:]]+", "_", x$participant_id)
#'   x
#' }
#'
#' fetch_database(
#'   conn,
#'   forms = c("my_form1", "my_form2", "my_form3"),
#'   fns = list(format_ids)
#' )
#' }
#'
#' @export fetch_database
fetch_database <- function(conn,
                           forms = NULL,
                           names_fn = function(x) x,
                           records = NULL,
                           records_omit = NULL,
                           id_field = TRUE,
                           rm_empty = TRUE,
                           value_labs = TRUE,
                           header_labs = FALSE,
                           checkbox_labs = FALSE,
                           use_factors = FALSE,
                           times_chron = TRUE,
                           date_range_begin = NULL,
                           date_range_end = NULL,
                           fn_dates = parse_date,
                           fn_dates_args = list(orders = c("Ymd", "dmY")),
                           fn_datetimes = lubridate::parse_date_time,
                           fn_datetimes_args = list(orders = c("Ymd HMS", "Ymd HM")),
                           na = c("", "NA"),
                           dag = TRUE,
                           double_resolve = FALSE,
                           double_sep = "--",
                           fns = NULL) {

  ## fetch metadata (dictionary, instruments, repeat instr, event mapping) -----
  m_dict <- meta_dictionary(conn)
  m_instr <- meta_forms(conn)
  m_events <- meta_events(conn, on_error = "null")
  m_repeat <- suppressWarnings(meta_repeating(conn, on_error = "null"))
  m_mapping <- meta_mapping(conn, on_error = "null")

  ## validate arguments --------------------------------------------------------
  names_fn <- match.fun(names_fn)
  test_valid(forms, m_instr$instrument_name)
  if (is.null(forms)) forms <- unique(m_instr$instrument_name)
  if (!is.null(fns)) fns_match <- lapply(fns, match.fun)

  ## fetch records -------------------------------------------------------------
  # uses lower-level fn fetch_records_() to avoid having to repeatedly fetch the
  # same metadata tables for each separate form
  out <- lapply(
    X = forms,
    FUN = fetch_records_,
    conn = conn,
    events = NULL,
    records = records,
    records_omit = records_omit,
    fields = NULL,
    id_field = id_field,
    rm_empty = rm_empty,
    value_labs = value_labs,
    header_labs = header_labs,
    checkbox_labs = checkbox_labs,
    use_factors = use_factors,
    times_chron = times_chron,
    date_range_begin = date_range_begin,
    date_range_end = date_range_end,
    fn_dates = fn_dates,
    fn_dates_args = fn_dates_args,
    fn_datetimes = fn_datetimes,
    fn_datetimes_args = fn_datetimes_args,
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

  names(out) <- names_fn(forms)

  ## apply custom function(s) --------------------------------------------------
  if (!is.null(fns)) {
    for (i in seq_along(fns_match)) {
      out <- lapply(out, fns_match[[i]])
    }
  }

  ## return --------------------------------------------------------------------
  out
}

