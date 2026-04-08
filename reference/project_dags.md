# Export REDCap user information

Execute an "Export Data Access Groups (DAGs)" API request to fetch the
DAGs (labels and code names) associated with a REDCap project.

## Usage

``` r
project_dags(conn)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

## Value

A
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame with 2 columns:

- `data_access_group_name`

- `unique_group_name`

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

project_dags(conn)
} # }
```
