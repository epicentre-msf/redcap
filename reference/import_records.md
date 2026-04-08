# Import records into a REDCap project

Import records into a REDCap project

## Usage

``` r
import_records(
  conn,
  data,
  type = c("flat", "eav"),
  overwrite = "normal",
  return = "count"
)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

- data:

  A data.frame containing record data to import into REDCap

- type:

  One of:

  - "flat": data in wide-form with one record per row (default)

  - "eav": data in long-form with one row per participant/instance/field
    (data should have columns "record", "field_name", and "value", and
    if longitudinal then also "redcap_event_name" and
    "redcap_repeat_instance")

- overwrite:

  Overwrite behaviour. Either "normal" to prevent missing values from
  overwriting data, or "overwrite" to allow data to be overwritten with
  missing values. Defaults to "normal".

- return:

  What to return. Use "count" to return a count of imported records,
  "ids" to return a vector of the IDs that were imported, or "nothing"
  to return nothing. Defaults to "count".

## Value

Depends on argument `return`. Either a count of imported records, a
vector of record IDs, or nothing.
