# Delete records from a REDCap project

Delete records from a REDCap project

## Usage

``` r
delete_records(conn, records)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

- records:

  Character vector of record IDs to delete

## Value

An integer, the number of records successfully deleted

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

# delete all records associated with IDs "P004" and "P007"
delete_records(conn, records = c("P004", "P007"))
} # }
```
