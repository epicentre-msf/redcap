# Generate data validation queries for a REDCap project based on branching logic specified in the project codebook

Generates two types of data validation queries using the project
codebook (see
[`meta_dictionary`](https://epicentre-msf.github.io/redcap/reference/meta_dictionary.md))
and
[`translate_logic`](https://epicentre-msf.github.io/redcap/reference/translate_logic.md):

1.  **Field missing**: Branching logic evaluates to `TRUE` (if
    specified), but field is missing. By default only applies to
    required fields (`required_field == "y"`) (can modify with argument
    `non_required`).

2.  **Field not missing**: Branching logic evaluates to `FALSE` but
    field is not missing. Applies to any field with branching logic.

## Usage

``` r
generate_queries(
  conn,
  forms = NULL,
  dict = meta_dictionary(conn, forms = forms, expand_checkbox = FALSE),
  lang = "en",
  query_types = "both",
  non_required = FALSE,
  drop_redundant = FALSE,
  field_nchar_max = 80L,
  on_error = "warn"
)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

- forms:

  Character vector of forms (i.e. instruments) to include. Set to `NULL`
  (the default) to generate queries for all forms in the project.

- dict:

  Metadata dictionary. By default is fetched automatically with
  [`meta_dictionary`](https://epicentre-msf.github.io/redcap/reference/meta_dictionary.md),
  but it's included as an argument here to allow the user to modify the
  dictionary before passing to `generate_queries` (e.g. to correct bugs
  in branching logic). If passing a modified version, make sure it is
  initially fetched with argument `expand_checkbox = FALSE`.

- lang:

  Query language, either English ("en") or French ("fr"). Defaults to
  "en".

- query_types:

  Which type of queries to generate (see **Description** above). Options
  are "missing", "not missing", or "both". Defaults to "both".

- non_required:

  Logical indicating whether to include non-required fields in queries
  of type "Field missing". Defaults to `FALSE`.

- drop_redundant:

  Logical indicating whether to simplify expressions by removing
  redundant components from expressions that test both for equality and
  inequality with the same variable. E.g.:  
  `var == "X" & var != ""` becomes `var == "X"`

- field_nchar_max:

  Integer indicating the maximum number of characters to allow in field
  name labels before they are truncated and appended with "...".
  Defaults to 80L.

- on_error:

  What to do if one or more elements of statements can't be translated
  into valid R expressions? Options are "ignore" (return `NA` for
  relevant elements), "warn" (return `NA` for relevant elements and give
  warning), or "fail" (throw error). Defaults to "warn".

## Value

A
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame specifying queries, with the following 7 columns:

- query_id:

  Unique query identifier based on form name and integer sequence

- field_name:

  Field name (from REDCap dictionary, see
  [`meta_dictionary`](https://epicentre-msf.github.io/redcap/reference/meta_dictionary.md))

- form_name:

  Form name (from REDCap dictionary)

- required:

  Is it a required field in REDCap dictionary ("y" or `<NA>`) ?

- description:

  Description of query (e.g. "Missing: \[Signed consent forms?\]")

- suggestion:

  Suggestion for query resolution. A human-readable translation of query
  expression (e.g. If \[Is the participant 18 years or older?\] is
  "Yes", item \[Signed consent forms?\] should not be missing)

- branching_logic:

  Branching logic for given field (from REDCap dictionary)

- query:

  R-style query expression (can be evaluated with
  [`queryr::query`](https://rdrr.io/pkg/queryr/man/query.html))
