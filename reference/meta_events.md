# Fetch event names and labels for a REDCap project

Execute an "Export Events" API request to fetch a
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame containing event names and labels. Note that this request
type is not available for 'classic projects', from which event details
cannot be exported.

## Usage

``` r
meta_events(conn, on_error = "fail")
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

- on_error:

  How to handle errors returned by the API (e.g. events cannot be
  exported for classic projects). Set to "fail" to halt execution and
  return the API error message, or "null" to ignore the error and return
  `NULL`. Defaults to "fail".

## Value

A
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame with 7 columns:

- `event_name`

- `arm_num`

- `day_offset`

- `offset_min`

- `offset_max`

- `unique_event_name`

- `custom_event_label`

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

meta_events(conn)
} # }
```
