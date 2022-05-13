#' Convert a REDCap project log file to a tidy data frame
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' REDCap project log files have a complicated format where multiple types of
#' information are contained in a single column. For example the column `action`
#' contains the relevant record ID, the type of action that was taken (e.g.
#' Updated / Created / Deleted), and sometimes further details about the source
#' of the action (e.g. API / Import / Automatic field calculation). The
#' `details` column contains a string of variable/value combinations describing
#' any changes (e.g. "var1 = '0', var2 = '1', var3(1) = checked"), and may also
#' contain the relevant repeat instance number (e.g. "\[instance = 3\]").
#'
#' The parse_logging() function tidies up the log file by splitting the record
#' ID, action, action type, and repeat instance into separate columns.
#' Optionally, the string of variable/value changes in the `details` column may
#' be further transformed to long format to yield a single row for each
#' combination of variable and value.
#'
#' Note that this function only deals with log entries of type Created / Deleted
#' / Updated Record. All other log entries (e.g. Data Export, Manage/Design,
#' Edited Role, User Assigned to Role) will be filtered out.
#'
#' @param x REDCap project log file (data frame), e.g. returned by
#'   [`project_logging`]
#' @param format_long Logical indicating whether to transform the log file to
#'   long format, with one row per variable-value combination. Defaults to
#'   `FALSE`.
#' @param dict A REDCap metadata dictionary (data frame), e.g. returned by
#'   [`meta_dictionary`]. Only needed when argument `format_long` is `TRUE`.
#'
#' @return
#' A [`tibble`][tibble::tbl_df]-style data frame with 8 columns:
#'
#' \describe{
#'   \item{`rowid`}{Row number based on original log file. There may be gaps for
#'   rows that have been excluded from the output because they reflected an
#'   action type other than create / delete / update.}
#'   \item{`timestamp`}{unchanged from original log file}
#'   \item{`username`}{unchanged from original log file}
#'   \item{`action`}{One of "Created Record", "Deleted Record", or "Updated
#'   Record", extracted from original `details` column}
#'   \item{`action_type`}{Parenthetical details, if any, extracted from original
#'   `action` column (e.g. "(API)", "(import)", "(Auto calculation)")}
#'   \item{`record_id`}{Record ID, extracted from original `action` column}
#'   \item{`redcap_repeat_instance`}{Instance number (integer), extracted from
#'   original `details` column. Note that 1st instances are not explicitly
#'   specified in the log file and will appear as NA}
#'   \item{`details`}{String of variable value pairs (e.g. "var1 = '0', var2 =
#'   '1', var3(1) = checked"), reflecting the data that was modified}
#' }
#'
#' If argument `format_long` is `TRUE` the `details` column will be replaced
#' with three other columns:
#'
#' \describe{
#'   \item{`form_name`}{Form name, joined from metadata dictionary based on
#'   variable `field_name`. Will be `<NA>` in cases where field name has been
#'   changed or removed and therefore doesn't appear in the dictionary, or for
#'   fields not associated with a specific form like
#'   `redcap_data_access_group`.}
#'   \item{`field_name`}{Field name, extracted from original `details` column}
#'   \item{`value`}{Value, extracted from original `details` column}
#' }
#'
#' @examples
#' \dontrun{
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' parse_logging(project_logging(conn))
#' }
#'
#' @importFrom dplyr `%>%` select filter mutate relocate if_else left_join n
#' @importFrom stringr str_extract
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @importFrom rlang .data
#' @export parse_logging
parse_logging <- function(x, format_long = FALSE, dict = NULL) {


  if (format_long & is.null(dict)) {
    stop("If format_long = TRUE, argument dict must be provided", call. = FALSE)
  }

  ## check that all action types are covered (should be nrow = 0)
  # x %>%
  #   filter(!grepl("^(Created|Deleted|Updated) Record", action)) %>%
  #   filter(!grepl("^(Created|Deleted|Updated) (Role|User)", action)) %>%
  #   filter(!grepl("^Data Export", action)) %>%
  #   filter(!grepl("^Manage/Design", action)) %>%
  #   filter(!grepl("^(Edited|Renamed) Role", action)) %>%
  #   filter(!grepl("^User (Assigned to|Removed from) Role", action)) %>%
  #   count(action)


  ## filter log file to entries related to records (create/update/delete)
  log_file_records <- x %>%
    mutate(rowid = seq_len(n()), .before = 1) %>%
    filter(grepl("^(Created|Deleted|Updated) Record", .data$action))

  # log_file_other <- x %>%
  #   filter(!grepl("^(Created|Deleted|Updated) Record", action))


  ## parse action (create/delete/update), action type (API/import/NA), record ID, and repeat instance
  log_parse <- log_file_records %>%
    filter(!grepl("^\\[instance = \\d+\\]$", .data$details)) %>% # rm if details contains only e.g. "[instance = d+]"
    filter(!is.na(.data$details)) %>%                            # rm if empty details
    mutate(
      action_raw = .data$action,
      action = stringr::str_extract(.data$action_raw, "(Created|Updated|Deleted) Record"),
      action_type = stringr::str_extract(.data$action_raw, "\\(.*\\)"),
      record_id = gsub("(Created|Updated|Deleted) Record (\\(.*\\) )*", "", .data$action_raw),
      # note approach below is much faster than single regex statement with PERL look-behind
      redcap_repeat_instance = stringr::str_extract(.data$details, "\\[instance = \\d+\\]"),
      redcap_repeat_instance = as.integer(stringr::str_extract(.data$redcap_repeat_instance, "[[:digit:]]+")),
      details = gsub("\\[instance = \\d+\\]\\, ", "", .data$details),
      dag = grepl("Assign record to Data Access Group", .data$details),
      details = if_else(.data$dag, stringr::str_extract(.data$details, "redcap_data_access_group \\= \\'.*\\'"), .data$details)
    ) %>%
    select(-.data$action_raw, -.data$dag) %>%
    relocate(c(.data$action, .data$action_type, .data$record_id, .data$redcap_repeat_instance), .after = .data$username)


  if (format_long) {

    ## prep field_name to allowing joining form_name to log
    dict_prep <- dict %>%
      mutate(field_name = gsub("___(\\d+)$", "(\\1)", .data$field_name)) %>%
      select(.data$field_name, .data$form_name)

    ## convert to long-format
    log_parse <- log_parse %>%
      mutate(field_and_value = purrr::map(.data$details, parse_value_var)) %>%
      tidyr::unnest("field_and_value") %>%
      mutate(
        field_name = stringr::str_extract(.data$field_and_value, "^.*(?= \\= )"),
        value = stringr::str_extract(.data$field_and_value, "(?<= \\= ).*$"),
        value = gsub("^\\'|\\'$", "", .data$value),
        value = dplyr::if_else(.data$value == "", NA_character_, .data$value)
      ) %>%
      select(-.data$details, -.data$field_and_value) %>%
      left_join(dict_prep, by = "field_name") %>%
      relocate(.data$form_name, .after = "record_id")
  }


  ## return
  log_parse
}



#' function to extract var/value combinations from strings in log file
#' strings have form: "vars1 = '...', var2 = '...', var3 = '...'"
#' e.g. "v2_01_send = 'OK', v2_01_comments = 'La participante n'a pas', v2_01_r_rempli = '1110'"
#' @noRd
#' @importFrom stringr str_locate_all
#' @importFrom stringr str_sub
parse_value_var <- function (x) {
  # vars normally should follow [a-z][a-z0-9_]+, but "__GROUPID__ = ''" also appears in some logs
  x_vars <- stringr::str_locate_all(x, "[a-zA-Z0-9_]+(\\(\\d+\\))? \\= (\\'|checked|unchecked)")[[1]]

  if (nrow(x_vars) <= 1) {
    out <- x
  } else {
    vars_start <- x_vars[,1]
    # var/val token ends 3 chars before next var
    vars_end   <- c(x_vars[,1][-1] - 3, nchar(x))
    out <- stringr::str_sub(x, vars_start, vars_end)
  }

  out
}

