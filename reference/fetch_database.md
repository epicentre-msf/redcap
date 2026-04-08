# Fetch records from multiple REDCap forms, returning separate list elements for each form

Wrapper to
[`fetch_records`](https://epicentre-msf.github.io/redcap/reference/fetch_records.md)
that's vectorized over forms (i.e. instruments). Returns a list whose
elements are
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frames corresponding to each requested form.

## Usage

``` r
fetch_database(
  conn,
  forms = NULL,
  names_fn = function(x) x,
  records = NULL,
  records_omit = NULL,
  id_field = TRUE,
  rm_empty = TRUE,
  rm_empty_omit_calc = FALSE,
  value_labs = TRUE,
  value_labs_fetch_raw = FALSE,
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
  batch_size = 100L,
  batch_delay = 0.5,
  form_delay = 0.5,
  double_resolve = FALSE,
  double_remove = FALSE,
  double_sep = "--",
  fns = NULL
)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

- forms:

  Character vector of forms (i.e. instruments) to fetch data for. Set to
  `NULL` (the default) to fetch all forms in the project.

- names_fn:

  Function for creating custom list element names given a vector of form
  names. Defaults to an identity function in which case element names
  will correspond exactly to form names.

- records:

  Character vector of record IDs to fetch. Set to `NULL` (the default)
  to fetch all record IDs corresponding to the selected form(s).

- records_omit:

  Character vector of record IDs to ignore. Set to `NULL` (the default)
  to *not* ignore any records. If a given record ID appears in both
  argument `records` and `records_omit`, argument `records_omit` takes
  precedence and that record will not be returned.

- id_field:

  Logical indicating whether to always include the 'record ID' field
  (defined in REDCap to be the first variable in the project codebook)
  in the API request, even if it's not specified in argument `fields`.
  Defaults to `TRUE`.

  The record ID field is defined within the first form of a REDCap
  project, and so API requests for other forms will not include the
  record ID field by default (unless it's explicitly requested with
  argument `fields`). The `id_field` argument is a shortcut to avoid
  having to always explicitly request the record ID field.

- rm_empty:

  Logical indicating whether to remove rows for which all fields from
  the relevant form(s) are missing. See section **Removing empty rows**.
  Defaults to `TRUE`.

- rm_empty_omit_calc:

  Logical indicating whether to exclude calculated fields from
  assessment of empty rows. Defaults to FALSE. In some cases calculated
  fields can be autopopulated for certain records even when the relevant
  form is truly empty, which would otherwise lead to "empty" forms being
  returned even when `rm_empty` is `TRUE`. Defaults to `FALSE`.

- value_labs:

  Logical indicating whether to return value labels (`TRUE`) or raw
  values (`FALSE`) for categorical REDCap variables (radio, dropdown,
  yesno, checkbox). Defaults to `TRUE` to return labels.

- value_labs_fetch_raw:

  Logical indicating whether to request raw values for categorical
  REDCap variables (radio, dropdown, yesno, checkbox), which are then
  transformed to labels in a separate step when `value_labs = TRUE`.
  Primarily used for troubleshooting issues with the REDCap API
  returning fewer records than expected when given certain combinations
  of request parameters.

- header_labs:

  Logical indicating whether to export column names as labels (`TRUE`)
  or raw variable names (`FALSE`). Defaults to `FALSE` to return raw
  variable names.

- checkbox_labs:

  Logical indicating whether to export checkbox labels (`TRUE`) or
  statuses (i.e. "Unchecked" or "Checked") (`FALSE`). Defaults to
  `FALSE` to export statuses. Note this argument is only relevant when
  `value_labs` is `TRUE` — if `value_labs` is `FALSE` checkbox variables
  will always be exported as raw values (usually "0"/"1").

- use_factors:

  Logical indicating whether categorical REDCap variables (radio,
  dropdown, yesno, checkbox) should be returned as factors. Factor
  levels can either be raw values (e.g. "0"/"1") or labels (e.g.
  "No"/"Yes") depending on arguments `value_labs` and `checkbox_labs`.
  Defaults to `FALSE`.

- times_chron:

  Logical indicating whether to reclass time variables using
  [chron::times](https://rdrr.io/pkg/chron/man/dates.html) (`TRUE`) or
  leave as character HH:MM format (`FALSE`). Defaults to `TRUE`. Note
  this only applies to variables of REDCap type "Time (HH:MM)", and not
  "Time (MM:SS)".

- date_range_begin:

  Fetch only records created or modified *after* a given date-time. Use
  format "YYYY-MM-DD HH:MM:SS" (e.g., "2017-01-01 00:00:00" for January
  1, 2017 at midnight server time). Defaults to NULL to omit a lower
  time limit.

- date_range_end:

  Fetch only records created or modified *before* a given date-time. Use
  format "YYYY-MM-DD HH:MM:SS" (e.g., "2017-01-01 00:00:00" for January
  1, 2017 at midnight server time). Defaults to NULL to omit a lower
  time limit.

- fn_dates:

  Function to parse REDCap date variables. Defaults to `parse_date`, an
  internal wrapper to
  [`lubridate::parse_date_time`](https://lubridate.tidyverse.org/reference/parse_date_time.html).
  If date variables have been converted to numeric (e.g. by writing to
  Excel), set to e.g.
  [`lubridate::as_date`](https://lubridate.tidyverse.org/reference/as_date.html)
  to convert back to dates.

- fn_dates_args:

  List of arguments to pass to `fn_dates`. Can set to empty list
  [`list()`](https://rdrr.io/r/base/list.html) if using a function that
  doesn't take any arguments.

- fn_datetimes:

  Function to parse REDCap datetime variables. Defaults to
  [`lubridate::parse_date_time`](https://lubridate.tidyverse.org/reference/parse_date_time.html).

- fn_datetimes_args:

  List of arguments to pass to `fn_datetimes`. Can set to empty list
  [`list()`](https://rdrr.io/r/base/list.html) if using a function that
  doesn't take any arguments.

- na:

  Character vector of strings to interpret as missing values. Passed to
  [readr::read_csv](https://readr.tidyverse.org/reference/read_delim.html).
  Defaults to `c("", "NA")`.

- dag:

  Logical indicating whether to export the `redcap_data_access_group`
  field (if used in the project). Defaults to `TRUE`.

- batch_size:

  Number of records to fetch per batch. Defaults to `100L`. Set to `Inf`
  or `NA` to fetch all records at once.

- batch_delay:

  Delay in seconds between fetching successive batches, to give the
  REDCap server time to respond to other requests. Defaults to `0.5`.

- form_delay:

  Delay in seconds between fetching successive forms, to give the REDCap
  server time to respond to other requests. Defaults to `0.5`.

- double_resolve:

  Logical indicating whether to resolve double-entries (i.e. records
  entered in duplicate using REDCap's Double Data Entry module), by
  filtering to the lowest entry number associated with each unique
  record.

  If a project uses double-entry, the record IDs returned by an "Export
  Records" API request will be a concatenation of the normal record ID
  and the entry number (1 or 2), normally separated by "–" (e.g.
  "P0285–1"). To resolve double entries we move the entry number portion
  of the ID to its own column (`entry`), identify all entries belonging
  to the same unique record, and retain only the row with the lowest
  entry number for each unique record.

  Unique records are identified using the record ID column (after
  separating the entry number portion), and any of the following columns
  when present (accounting for argument `header_labs`):
  redcap_event_name (Redcap Event), redcap_repeat_instrument (Repeat
  Instrument), redcap_repeat_instance (Repeat Instance).

- double_remove:

  Logical indicating whether to *remove* double-entries (i.e. records
  entered in duplicate using REDCap's Double Data Entry module), by
  filtering out records where the record ID field contains pattern
  `double_sep` (see next argument), so that only merged records remain.

- double_sep:

  If `double_resolve` is `TRUE`, the string separator used to split the
  record ID field into the record ID and entry number. Defaults to "–".

- fns:

  Optional list of one or more functions to apply to each list element
  (i.e. each form). Could be used e.g. to filter out record IDs from
  test entries, create derived variables, etc. Each function should take
  a data frame returned by
  [`fetch_records`](https://epicentre-msf.github.io/redcap/reference/fetch_records.md)
  as its first argument.

## Value

A list of
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frames corresponding to each of the requested forms.

## Removing empty rows

Depending on the database design, an "Export Records" API request can
sometimes return empty rows, representing forms for which no data has
been collected. For example, if forms **F1** and **F2** are part of the
same event, and participant "P001" has form data for **F2** but not
**F1**, an API request for **F1** will include a row for participant
"P001" where all **F1**-specific fields are empty.

If argument `rm_empty` is `TRUE` (the default),
[`fetch_records()`](https://epicentre-msf.github.io/redcap/reference/fetch_records.md)
will filter out such rows. The check for empty rows is based only on
fields that are specific to the form(s) specified in argument `forms` —
i.e. it excludes the record ID field, and generic fields like
`redcap_event_name`, `redcap_data_access_group`, etc. The check for
empty rows also accounts for checkbox fields, which, if argument
`checkbox_labs` is `FALSE`, will be set to "Unchecked" in an empty form
(rather than missing per se).

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

fetch_database(
  conn,
  forms = c("my_form1", "my_form2", "my_form3")
)

# use a custom fn to format the 'participant_id' column of each form
# the function must take a data frame as its first argument
format_ids <- function(x) {
  x$participant_id <- toupper(x$participant_id)
  x$participant_id <- gsub("[^[:alnum:]]+", "_", x$participant_id)
  x
}

fetch_database(
  conn,
  forms = c("my_form1", "my_form2", "my_form3"),
  fns = list(format_ids)
)
} # }
```
