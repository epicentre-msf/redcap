# Convert a REDCap project log file to a tidy data frame

**\[experimental\]**

REDCap project log files have a complicated format where multiple types
of information are contained in a single column. For example the column
`action` contains the relevant record ID, the type of action that was
taken (e.g. Updated / Created / Deleted), and sometimes further details
about the source of the action (e.g. API / Import / Automatic field
calculation). The `details` column contains a string of variable/value
combinations describing any changes (e.g. "var1 = '0', var2 = '1',
var3(1) = checked"), and may also contain the relevant repeat instance
number (e.g. "\[instance = 3\]").

The parse_logging() function tidies up the log file by splitting the
record ID, action, action type, and repeat instance into separate
columns. Optionally, the string of variable/value changes in the
`details` column may be further transformed to long format to yield a
single row for each combination of variable and value.

Note that this function only deals with log entries of type Created /
Deleted / Updated Record. All other log entries (e.g. Data Export,
Manage/Design, Edited Role, User Assigned to Role) will be filtered out.

## Usage

``` r
parse_logging(x, format_long = FALSE, dict = NULL)
```

## Arguments

- x:

  REDCap project log file (data frame), e.g. returned by
  [`project_logging`](https://epicentre-msf.github.io/redcap/reference/project_logging.md)

- format_long:

  Logical indicating whether to transform the log file to long format,
  with one row per variable-value combination. Defaults to `FALSE`.

- dict:

  A REDCap metadata dictionary (data frame), e.g. returned by
  [`meta_dictionary`](https://epicentre-msf.github.io/redcap/reference/meta_dictionary.md).
  Only needed when argument `format_long` is `TRUE`.

## Value

A
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)-style
data frame with 8 columns:

- `rowid`:

  Row number based on original log file. There may be gaps for rows that
  have been excluded from the output because they reflected an action
  type other than create / delete / update.

- `timestamp`:

  unchanged from original log file

- `username`:

  unchanged from original log file

- `action`:

  One of "Created Record", "Deleted Record", or "Updated Record",
  extracted from original `details` column

- `action_type`:

  Parenthetical details, if any, extracted from original `action` column
  (e.g. "(API)", "(import)", "(Auto calculation)")

- `record_id`:

  Record ID, extracted from original `action` column

- `redcap_repeat_instance`:

  Instance number (integer), extracted from original `details` column.
  Note that 1st instances are not explicitly specified in the log file
  and will appear as NA

- `details`:

  String of variable value pairs (e.g. "var1 = '0', var2 = '1', var3(1)
  = checked"), reflecting the data that was modified

If argument `format_long` is `TRUE` the `details` column will be
replaced with three other columns:

- `form_name`:

  Form name, joined from metadata dictionary based on variable
  `field_name`. Will be `<NA>` in cases where field name has been
  changed or removed and therefore doesn't appear in the dictionary, or
  for fields not associated with a specific form like
  `redcap_data_access_group`.

- `field_name`:

  Field name, extracted from original `details` column

- `value`:

  Value, extracted from original `details` column

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)

parse_logging(project_logging(conn))
} # }
```
