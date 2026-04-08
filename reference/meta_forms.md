# Fetch instrument names and labels for a REDCap project

Execute an "Export Instrument (Data Entry Forms)" API request to fetch a
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame containing instrument names and labels.

## Usage

``` r
meta_forms(conn)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

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

meta_forms(conn)
} # }
```
