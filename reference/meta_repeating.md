# Fetch repeating instrument names and labels for a REDCap project

Execute an "Export Repeating Instruments and Events" API request to
fetch a
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame containing repeating instrument names and labels.

## Usage

``` r
meta_repeating(conn, on_error = "fail")
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
data frame with 2 columns:

- `instrument_name`

- `instrument_label`

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

meta_repeating(conn)
} # }
```
