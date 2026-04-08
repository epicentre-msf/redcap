# Export REDCap project arms

Execute an "Export Arms" API request to fetch the "arms" (number and
name) associated with a REDCap project.

## Usage

``` r
meta_arms(conn)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

## Value

A
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame with 2 columns:

- `arm_num`

- `name`

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

meta_arms(conn)
} # }
```
