# Export REDCap user information

Execute an "Export Users" API request to fetch user-related details
(e.g. username, email, access permissions, etc.) corresponding to a
REDCap project.

## Usage

``` r
project_users(conn)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

## Value

A
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame containing the columns returned by an "Export Users" API
request

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

project_users(conn)
} # }
```
