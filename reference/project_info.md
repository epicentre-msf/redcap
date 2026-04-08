# Export REDCap Project information

Execute an "Export Project Info" API request to fetch project-related
details (e.g. title, creation time, production status, language, etc.)
corresponding to a REDCap project.

## Usage

``` r
project_info(conn)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

## Value

A
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame containing the columns returned by an "Export Project Info"
API request

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

project_info(conn)
} # }
```
