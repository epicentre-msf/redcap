# Fetch field option values and labels for factor-type variables in a REDCap project

Converts field options and labels from the metadata dictionary (see
[`meta_dictionary`](https://epicentre-msf.github.io/redcap/reference/meta_dictionary.md)),
which has 1 row per variable and field options in compact string-form,
to long form with 1 row per option. E.g.:

**Dictionary version** (1 row per variable):

|              |             |              |                 |
|--------------|-------------|--------------|-----------------|
| `field_name` | `form_name` | `field_type` | `choices`       |
| consented    | enrollment  | radio        | 0, No \| 1, Yes |

**Long format** (1 row per option):

|              |             |              |         |         |
|--------------|-------------|--------------|---------|---------|
| `field_name` | `form_name` | `field_type` | `value` | `label` |
| consented    | enrollment  | radio        | 0       | No      |
| consented    | enrollment  | radio        | 1       | Yes     |

## Usage

``` r
meta_factors(
  conn,
  forms = NULL,
  expand_checkbox = TRUE,
  add_complete = FALSE,
  types = c("radio", "yesno", "dropdown", "checkbox")
)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

- forms:

  Character vector of forms (i.e. instruments) to include. Set to `NULL`
  (the default) to return field options for all forms in the project.

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

- types:

  Character vector of variable types to return field options for.  
  Defaults to `c("radio", "yesno", "dropdown", "checkbox")`.

## Value

A
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame with 6 columns:

- field_name

- form_name

- field_type

- field_label

- value

- label

## See also

[`meta_dictionary`](https://epicentre-msf.github.io/redcap/reference/meta_dictionary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

meta_factors(conn)
} # }
```
