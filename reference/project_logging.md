# Export project logging (audit trail) for a REDCap project

Execute an "Export Logging" API request to export the logging (audit
trail) of all changes made to the project, including data exports, data
changes, project metadata changes, modification of user rights, etc.

## Usage

``` r
project_logging(
  conn,
  type = NULL,
  user = NULL,
  record = NULL,
  time_start = NULL,
  time_end = NULL,
  timestamp_to_posix = TRUE
)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

- type:

  Type of logging to return. Defaults to NULL to return all types.
  Specific logging types include:

  - "export": Data export

  - "manage": Manage/Design

  - "user": User or role created-updated-deleted

  - "record": Record created-updated-deleted

  - "record_add": Record created (only)

  - "record_edit": Record updated (only)

  - "record_delete": Record deleted (only)

  - "lock_record": Record locking & e-signatures

  - "page_view": Page Views

- user:

  REDCap username to fetch logs for. Defaults to NULL to fetch logs
  relating to all users. Note that, in the current API version (10.8.5),
  it's not possible to pass a character vector with multiple usernames
  (i.e. one user per request, or NULL for all).

- record:

  Record ID to fetch logs for. Defaults to NULL to fetch logs relating
  to all record IDs. Note that, in the current API version (10.8.5),
  it's not possible to pass a character vector with multiple record IDs
  (i.e. one record per request, or NULL for all).

- time_start:

  Fetch logs from *after* a given date-time. Use format "YYYY-MM-DD
  HH:MM". Defaults to NULL to omit a lower time limit.

- time_end:

  Fetch logs from *before* a given date-time. Use format "YYYY-MM-DD
  HH:MM". Defaults to NULL to omit an upper time limit.

- timestamp_to_posix:

  Logical indicating whether to convert the `timestamp` column to class
  POSIXct using
  [`lubridate::as_datetime`](https://lubridate.tidyverse.org/reference/as_date.html).
  Defaults to TRUE.

## Value

A
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame with 4 columns:

- `timestamp`

- `username`

- `actions`

- `details`

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

project_logging(conn)
} # }
```
