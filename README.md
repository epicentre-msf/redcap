
<!-- README.md is generated from README.Rmd. Please edit that file -->

# redcap: R utilities for interacting with REDCap

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/epicentre-msf/redcap/workflows/R-CMD-check/badge.svg)](https://github.com/epicentre-msf/redcap/actions)
[![Codecov test
coverage](https://codecov.io/gh/epicentre-msf/redcap/branch/main/graph/badge.svg)](https://codecov.io/gh/epicentre-msf/redcap?branch=main)
<!-- badges: end -->

An R package for interacting with
[REDCap](http://www.project-redcap.org/), inspired by the
[redcapAPI](https://github.com/nutterb/redcapAPI) package by Benjamin
Nutter.

#### Features

  - Outputs
    [tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
    data frames
  - New features for `fetch_records()`
      - by default, ensure record ID field always returned
      - by default, omit rows where all form-specific fields are empty
      - optional arguments for resolving Double Data Entry to single
        entry per record
      - simplified column classes
          - REDCap categorical variables have class “character” (can
            optionally use “factor”)
          - REDCap date variables have class “Date” (“POSIXct” used only
            for datetime variables)
  - New function `fetch_database()` vectorizes `fetch_records()` over
    forms. Accepts additional functions as arguments, e.g. for deriving
    new variables or customizing form names
  - New function `generate_queries()` creates data validation queries
    based on branching logic specified in the project codebook. These
    can be run using the `query_vec()` function in
    [queryr](https://github.com/epicentre-msf/queryr).

### Installation

Install from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("epicentre-msf/redcap")
```

### Example usage

``` r
library(redcap)

### create an API connection
conn <- rconn(
  url = "https://www.research.epicentre.msf.org/api/",
  token = Sys.getenv("REDCAP_PKG") # this project just used for testing
)

### fetch metadata tables
df_dictionary <- meta_dictionary(conn)
df_fields     <- meta_fields(conn)
df_forms      <- meta_forms(conn)
df_events     <- meta_events(conn)
df_mapping    <- meta_mapping(conn)
df_repeating  <- meta_repeating(conn)

### fetch records (generally from a single form)
df_records <- fetch_records(conn, forms = "eligibility")

### fetch records from all forms (as a list of data frames, one per form)
db_records <- fetch_database(conn)

### generate queries based on branching logic in project codebook
df_queries <- generate_queries(conn)

# run queries using the queryr package
queries_out <- queryr::query_vec(
  x = db_records,                 # REDCap database as list of data frames
  cond = df_queries$query,        # query expressions
  name = df_queries$query_id,     # name/identifier for each query
  element = df_queries$form_name, # primary form for each query expression
  cols_base = record_id,          # columns to always include in return
  join_type = "left",             # join type if query refers to multiple forms
  join_by = "record_id"           # join key if query refers to multiple forms
)
```

### More examples

1.  Customize form-specific names returned by `fetch_database()`

<!-- end list -->

``` r
### default element names returned by fetch_database() are REDCap form names
db_records <- fetch_database(conn)
names(db_records)
#> [1] "enrolment"   "eligibility" "followup"

### to customize we need a function that takes a vector of form names and
# returns a vector of custom names
recode_forms <- function(x) {
  dplyr::recode(
    x,
    enrolment = "ENR", eligibility = "ELG", followup = "FUP"
  )
}

db_records <- fetch_database(conn, names_fn = recode_forms)
names(db_records)
#> [1] "ENR" "ELG" "FUP"
```

2.  Pass additional functions to `fetch_database()` to act on each form

<!-- end list -->

``` r
library(dplyr, warn.conflicts = FALSE)
library(rlang)

### for each form, find the column giving the date and time of form completion
# (ends with "_form_dt"), and use it to derive a new date column form_date
add_form_date  <- function(df) {
  # find name of datetime col for given form
  col_dt <- grep("_form_dt$", names(df), value = TRUE)
  # derive col form_date
  dplyr::mutate(
    df,
    form_date = lubridate::as_date(!!ensym(col_dt)),
    .after = "record_id"
  )
}

db_records <- fetch_database(conn, fns = list(add_form_date))
db_records$enrolment[,1:5] # print first few cols to show new form_date column
#> # A tibble: 3 x 5
#>   record_id form_date  redcap_event_name redcap_repeat_instrument redcap_repeat_instance
#>   <chr>     <date>     <chr>             <chr>                                     <int>
#> 1 0001      2020-12-01 Enrollment        <NA>                                         NA
#> 2 0002      2020-11-25 Enrollment        <NA>                                         NA
#> 3 0003      2020-12-11 Enrollment        <NA>                                         NA
```
