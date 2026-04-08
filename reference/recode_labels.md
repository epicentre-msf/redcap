# Convert between values and labels for factor-type variables (e.g. yes/no, radio, dropdown, checkbox)

Convert between values and labels for factor-type variables (e.g.
yes/no, radio, dropdown, checkbox)

## Usage

``` r
recode_labels(
  x,
  conn,
  dict = redcap::meta_dictionary(conn, add_complete = TRUE),
  convert_to = c("labels", "values"),
  types = c("radio", "yesno", "dropdown", "checkbox"),
  header_labs = FALSE
)
```

## Arguments

- x:

  Data frame representing a REDCap form (e.g. from a previous export
  using
  [`fetch_records`](https://epicentre-msf.github.io/redcap/reference/fetch_records.md))

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

- dict:

  REDCap metadata dictionary. Defaults to fetching the current version
  with
  [`meta_dictionary`](https://epicentre-msf.github.io/redcap/reference/meta_dictionary.md)

- convert_to:

  Convert values to labels ("labels") or labels to values ("values")

- types:

  Types of REDCap variables to convert, based on column "field_type" in
  the metadata dictionary. Defaults to
  `c("radio", "yesno", "dropdown", "checkbox")`.

- header_labs:

  Logical indicating whether column names of `x` are labels (TRUE) or
  raw variable names (FALSE). Default assumes header has raw variable
  names (i.e. FALSE).
