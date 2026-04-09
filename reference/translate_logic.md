# Translate REDCap branching logic into R expressions

Translate REDCap branching logic into expressions evaluable in R. E.g.

|                                     |                                               |
|-------------------------------------|-----------------------------------------------|
| **Translation step**                | **Example expression**                        |
| 0\. Initial REDCap logic            | `[over_18]='1' and [signed_consent]<>''`      |
| 1\. Translate to R                  | `over_18 == '1' & signed_consent != ''`       |
| 2\. (Optional) Swap values/labels   | `over_18 == 'Yes' & signed_consent != ''`     |
| 3\. (Optional) Use is.na            | `over_18 == 'Yes' & !is.na(signed_consent)`   |
| 4\. (Optional) Use %in%             | `over_18 %in% 'Yes' & !is.na(signed_consent)` |

## Usage

``` r
translate_logic(
  x,
  use_value_labs = TRUE,
  use_header_labs = FALSE,
  use_is_na = TRUE,
  use_in = TRUE,
  drop_redundant = FALSE,
  field_nchar_max = 80L,
  meta_factors = NULL,
  meta_dictionary = NULL,
  on_error = "warn"
)
```

## Arguments

- x:

  A character vector of REDCap branching logic statements

- use_value_labs:

  Logical indicating whether to replace factor option values with labels
  (based on the mapping defined in `meta_factors`). E.g.:  
  `y == '1'` becomes `y == 'Yes'`

- use_header_labs:

  Logical indicating whether to use labels instead of variable names as
  column names (based on the mapping defined in `meta_dictionary`).
  E.g.:  
  `age >= 18` becomes `"Participant's age" >= 18`

- use_is_na:

  Logical indicating whether to replace REDCap-style tests for
  missingness with [`is.na`](https://rdrr.io/r/base/NA.html). E.g.:  
  `y == ""` becomes `is.na(y)`  
  `y != ""` becomes `!is.na(y)`

- use_in:

  Logical indicating whether to replace instances of
  [`==`](https://rdrr.io/r/base/Comparison.html) and
  [`!=`](https://rdrr.io/r/base/Comparison.html) associated with
  factor-type variables (as defined in `meta_factors`) with
  [`base::%in%`](https://rdrr.io/r/base/match.html). E.g.:  
  `y == 'Yes'` becomes `y %in% 'Yes'`  
  `y != 'Yes'` becomes `!y %in% 'Yes'`

- drop_redundant:

  Logical indicating whether to simplify expressions by removing
  redundant components from expressions that test both for equality and
  inequality with the same variable. E.g.:  
  `var == "X" & var != ""` becomes `var == "X"`

- field_nchar_max:

  Integer indicating the maximum number of characters to allow in field
  name labels before they are truncated and appended with "...".
  Defaults to 80L.

- meta_factors:

  A data frame containing variable names (column `field_name`) and
  corresponding values (column `value`) and labels (column `label`) for
  factor-type variables. Fetch with
  [`meta_factors`](https://epicentre-msf.github.io/redcap/reference/meta_factors.md).
  Only needed if either `use_value_labs` or `use_in` are `TRUE`.

- meta_dictionary:

  A data frame containing variable names (column `field_name`) and
  labels (column `field_label`). Fetch with
  [`meta_dictionary`](https://epicentre-msf.github.io/redcap/reference/meta_dictionary.md).
  Only needed if `use_header_labs` is `TRUE`.

- on_error:

  What to do if one or more elements of statements can't be translated
  into valid R expressions? Options are "ignore" (return `NA` for
  relevant elements), "warn" (return `NA` for relevant elements and give
  warning), or "fail" (throw error). Defaults to "warn".

## Value

A character vector of R-style expressions

## Examples

``` r
# normally would fetch factor metadata with redcap::meta_factors(), but here
# we'll create a simple example by hand
df_factors <- data.frame(
  field_name = c("head_household", "head_household"),
  value = c("0", "1"),
  label = c("No", "Yes"),
  stringsAsFactors = FALSE
)

redcap_logic <- "head_household=1 and age<>\"\""
translate_logic(redcap_logic, meta_factors = df_factors)
#> [1] "head_household %in% \"Yes\" & !is.na(age)"
```
