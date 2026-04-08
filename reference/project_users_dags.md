# Export REDCap mapping between users and Data Access Groups (DAGs)

Execute an "Export User-DAG Assignments" API request to fetch the
mapping between users and Data Access Groups (DAGs) associated with a
REDCap project.

## Usage

``` r
project_users_dags(conn)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

## Value

A
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame with 2 columns:

- `username`

- `redcap_data_access_group`

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

project_users_dags(conn)
} # }
```
