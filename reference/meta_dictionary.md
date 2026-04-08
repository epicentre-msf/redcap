# Fetch variable dictionary for a REDCap project

Execute an "Export Metadata (Data Dictionary)" API request to fetch a
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame containing the project codebook (field names, types, labels,
choices, validation, etc.).

## Usage

``` r
meta_dictionary(
  conn,
  forms = NULL,
  expand_checkbox = TRUE,
  add_complete = FALSE,
  cols_omit = c("section_header", "custom_alignment", "question_number",
    "matrix_group_name", "matrix_ranking")
)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

- forms:

  Character vector of forms (i.e. instruments) to include in the return.
  Set to `NULL` (the default) to return dictionary entries for all forms
  in the project.

- expand_checkbox:

  Logical indicating whether to expand checkbox variables. Defaults to
  `TRUE`.

  Unlike an "Export Records" API request (see
  [`fetch_records`](https://epicentre-msf.github.io/redcap/reference/fetch_records.md)),
  which returns 1 column for each checkbox *option*, an "Export Metadata
  (Data Dictionary)" request returns a single row for each *field* —
  including checkbox fields. Thus, the `field_name` and `field_label`
  entries for checkbox variables in the data dictionary will never
  exactly match the respective column names or values returned by
  [`fetch_records`](https://epicentre-msf.github.io/redcap/reference/fetch_records.md).

  When `expand_checkbox` is `TRUE`, rows for checkbox fields are
  expanded to 1 row per checkbox *option*, so that dictionary entries
  for `field_name`, `field_label`, and `choices` will always match the
  relevant entries returned by
  [`fetch_records`](https://epicentre-msf.github.io/redcap/reference/fetch_records.md).

- add_complete:

  Logical indicating whether to add "{form}\_complete" fields to the
  dictionary, one for each form included in the return. These will be of
  field_type "radio" with possible choices "0, Incomplete \| 1,
  Unverified \| 2, Complete". Defaults to `FALSE`.

- cols_omit:

  Character vector of dictionary columns to omit from the return for
  brevity. Set to `NULL` to return all columns.

## Value

A
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame containing the project dictionary. Note that some of the
returned column names are shortened versions of the original column
names returned by the API:

|                                               |                  |
|-----------------------------------------------|------------------|
| **Original**                                  | **Returned**     |
| `select_choices_or_calculations`              | `choices`        |
| `text_validation_type_or_show_slider_number`  | `validation`     |
| `text_validation_min`                         | `validation_min` |
| `text_validation_max`                         | `validation_max` |

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

meta_dictionary(conn)
} # }
```
