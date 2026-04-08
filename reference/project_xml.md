# Export REDCap Project XML file

Execute an "Export Entire Project as REDCap XML File" API request to
fetch all metadata (and optionally also data records) corresponding to a
REDCap project.

## Usage

``` r
project_xml(
  conn,
  meta_only = FALSE,
  records = NULL,
  fields = NULL,
  events = NULL,
  filter_logic = NULL,
  export_dag = FALSE,
  export_survey = FALSE,
  export_files = FALSE
)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

- meta_only:

  Logical indicating whether to fetch only the project metadata (if
  `TRUE`) or both the metadata and data records (if `FALSE`). Defaults
  to `FALSE` to fetch both metadata and data.

- records:

  Optional character vector of specific record IDs to fetch record data
  for. Only used when `meta_only = FALSE`.

- fields:

  Optional character vector of specific fields to fetch record data for.
  Only used when `meta_only = FALSE`.

- events:

  Optional character vector of specific events to fetch record data for.
  Only used when `meta_only = FALSE`.

- filter_logic:

  Optional character string containing a REDCap-style expression used to
  filter records returned by the API (e.g. "\[age\] \> 30")

- export_dag:

  Logical indicating whether to export the redcap_data_access_group
  field. Defaults to `FALSE`.

- export_survey:

  Logical indicating whether to export survey identifier or timestamp
  fields, if surveys are used in the project. Defaults to `FALSE`.

- export_files:

  Logical indicating whether to export uploaded files. Note this may
  lead to large exports. Defaults to `FALSE`.

## Value

An object of class `xml_document`

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

project_xml(conn)
} # }
```
