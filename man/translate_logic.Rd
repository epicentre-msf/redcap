% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/translate_logic.R
\name{translate_logic}
\alias{translate_logic}
\title{Translate REDCap branching logic into R expressions}
\usage{
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
}
\arguments{
\item{x}{A character vector of REDCap branching logic statements}

\item{use_value_labs}{Logical indicating whether to replace factor option
values with labels (based on the mapping defined in \code{meta_factors}).
E.g.:\cr
\code{y == '1'} becomes \code{y == 'Yes'}}

\item{use_header_labs}{Logical indicating whether to use labels instead of
variable names as column names (based on the mapping defined in
\code{meta_dictionary}).
E.g.:\cr
\code{age >= 18} becomes \code{"Participant's age" >= 18}}

\item{use_is_na}{Logical indicating whether to replace REDCap-style tests for
missingness with \code{\link{is.na}}. E.g.:\cr
\code{y == ""} becomes \code{is.na(y)}\cr
\code{y != ""} becomes \code{!is.na(y)}}

\item{use_in}{Logical indicating whether to replace instances of \code{\link{==}} and
\code{\link{!=}} associated with factor-type variables (as defined in \code{meta_factors})
with \code{\link{\%in\%}}. E.g.:\cr
\code{y == 'Yes'} becomes \code{y \%in\% 'Yes'}\cr
\code{y != 'Yes'} becomes \code{!y \%in\% 'Yes'}}

\item{drop_redundant}{Logical indicating whether to simplify expressions by
removing redundant components from expressions that test both for equality
and inequality with the same variable. E.g.:\cr
\code{var == "X" & var != ""} becomes \code{var == "X"}}

\item{field_nchar_max}{Integer indicating the maximum number of characters to
allow in field name labels before they are truncated and appended with
"...". Defaults to 80L.}

\item{meta_factors}{A data frame containing variable names (column
\code{field_name}) and corresponding values (column \code{value}) and labels (column
\code{label}) for factor-type variables. Fetch with \code{\link{meta_factors}}. Only
needed if either \code{use_value_labs} or \code{use_in} are \code{TRUE}.}

\item{meta_dictionary}{A data frame containing variable names (column
\code{field_name}) and labels (column \code{field_label}). Fetch with
\code{\link{meta_dictionary}}. Only needed if \code{use_header_labs} is \code{TRUE}.}

\item{on_error}{What to do if one or more elements of statements can't be
translated into valid R expressions? Options are "ignore" (return \code{NA} for
relevant elements), "warn" (return \code{NA} for relevant elements and give
warning), or "fail" (throw error). Defaults to "warn".}
}
\value{
A character vector of R-style expressions
}
\description{
Translate REDCap branching logic into expressions evaluable in R. E.g.\tabular{ll}{
   \strong{Translation step} \tab \strong{Example expression} \cr
   0. Initial REDCap logic \tab \verb{[over_18]='1' and [signed_consent]<>''} \cr
   1. Translate to R \tab \code{over_18 == '1' & signed_consent != ''} \cr
   2. (Optional) Swap values/labels   \tab \code{over_18 == 'Yes' & signed_consent != ''} \cr
   3. (Optional) Use is.na \tab \code{over_18 == 'Yes' & !is.na(signed_consent)} \cr
   4. (Optional) Use \%in\% \tab \code{over_18 \%in\% 'Yes' & !is.na(signed_consent)} \cr
}
}
\examples{
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

}
