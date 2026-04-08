# Convert a REDCap project XML file to a tidy data frame of project records

Extract records from a REDCap project XML file (e.g. returned by
[`project_xml`](https://epicentre-msf.github.io/redcap/reference/project_xml.md))
and assemble into a tidy long-form data frame, with one row for each
combination of record x field x event x instance.

## Usage

``` r
parse_xml(x)
```

## Arguments

- x:

  A REDCap project XML obect of class `xml_document`, e.g. returned by
  [`project_xml`](https://epicentre-msf.github.io/redcap/reference/project_xml.md)

## Value

A
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame with 6 columns:

- `record_id`

- `form`

- `redcap_event`

- `redcap_repeat_instance`

- `field`

- `value`

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

parse_xml(project_xml(conn))
} # }
```
