# Reclass columns of a data frame to match classes specified in a metadata dictionary

Reclass columns of a data frame to match classes specified in a metadata
dictionary

## Usage

``` r
reclass(
  x,
  dict,
  use_factors = FALSE,
  value_labs = TRUE,
  header_labs = FALSE,
  times_chron = TRUE,
  fn_dates = parse_date,
  fn_dates_args = list(orders = c("Ymd", "dmY")),
  fn_datetimes = lubridate::parse_date_time,
  fn_datetimes_args = list(orders = c("Ymd HMS", "Ymd HM"))
)
```

## Arguments

- x:

  A data frame representing a REDCap form

- dict:

  A metadata dictionary

- use_factors:

  Logical indicating whether categorical REDCap variables (radio,
  dropdown, yesno, checkbox) should be returned as factors. Factor
  levels can either be raw values (e.g. "0"/"1") or labels (e.g.
  "No"/"Yes") depending on arguments `value_labs` and `checkbox_labs`.
  Defaults to `FALSE`.

- value_labs:

  Logical indicating whether to return value labels (`TRUE`) or raw
  values (`FALSE`) for categorical REDCap variables (radio, dropdown,
  yesno, checkbox). Defaults to `TRUE` to return labels.

- header_labs:

  Logical indicating whether to export column names as labels (`TRUE`)
  or raw variable names (`FALSE`). Defaults to `FALSE` to return raw
  variable names.

- times_chron:

  Logical indicating whether to reclass time variables using
  [chron::times](https://rdrr.io/pkg/chron/man/dates.html) (`TRUE`) or
  leave as character HH:MM format (`FALSE`). Defaults to `TRUE`. Note
  this only applies to variables of REDCap type "Time (HH:MM)", and not
  "Time (MM:SS)".

- fn_dates:

  Function to parse REDCap date variables. Defaults to `parse_date`, an
  internal wrapper to
  [`lubridate::parse_date_time`](https://lubridate.tidyverse.org/reference/parse_date_time.html).
  If date variables have been converted to numeric (e.g. by writing to
  Excel), set to e.g.
  [`lubridate::as_date`](https://lubridate.tidyverse.org/reference/as_date.html)
  to convert back to dates.

- fn_dates_args:

  List of arguments to pass to `fn_dates`. Can set to empty list
  [`list()`](https://rdrr.io/r/base/list.html) if using a function that
  doesn't take any arguments.

- fn_datetimes:

  Function to parse REDCap datetime variables. Defaults to
  [`lubridate::parse_date_time`](https://lubridate.tidyverse.org/reference/parse_date_time.html).

- fn_datetimes_args:

  List of arguments to pass to `fn_datetimes`. Can set to empty list
  [`list()`](https://rdrr.io/r/base/list.html) if using a function that
  doesn't take any arguments.
