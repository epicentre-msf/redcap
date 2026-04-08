# Fetch the REDCap database version used for a particular project

Execute an "Export REDCap version" API request

## Usage

``` r
redcap_version(conn)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

## Value

A character string

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

redcap_version(conn)
} # }
```
