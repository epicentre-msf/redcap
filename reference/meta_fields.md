# Fetch exported field names for a REDCap project

Execute an "Export List of Export Field Names" API request to fetch a
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame containing field (i.e. variable) names as listed in the
project codebook (column `original_field_name`) and the corresponding
exported variable name(s) (column `export_field_name`).

Original and exported field names will be identical except in the case
of checkbox-type variables. A given checkbox variable (e.g.
"patient_status") will have a single entry in the codebook (i.e.
`field_name` = "patient_status"), but will be exported as multiple
variables — one for each possible choice value.

|                       |                |                        |
|-----------------------|----------------|------------------------|
| `original_field_name` | `choice_value` | `export_field_name`    |
| patient_status        | 1              | patient_status\_\_\_1  |
| patient_status        | 2              | patient_status\_\_\_2  |
| patient_status        | 3              | patient_status\_\_\_3  |
| patient_status        | 88             | patient_status\_\_\_88 |

## Usage

``` r
meta_fields(conn)
```

## Arguments

- conn:

  A REDCap API connection object (created with
  [`rconn`](https://epicentre-msf.github.io/redcap/reference/rconn.md))

## Value

A
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame with 3 columns:

- `original_field_name`

- `choice_value`

- `export_field_name`

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

meta_fields(conn)
} # }
```
