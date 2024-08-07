% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_queries.R
\name{generate_queries}
\alias{generate_queries}
\title{Generate data validation queries for a REDCap project based on branching
logic specified in the project codebook}
\usage{
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
}
\arguments{
\item{conn}{A REDCap API connection object (created with \code{\link{rconn}})}

\item{forms}{Character vector of forms (i.e. instruments) to include. Set to
\code{NULL} (the default) to generate queries for all forms in the project.}

\item{dict}{Metadata dictionary. By default is fetched automatically with
\code{\link{meta_dictionary}}, but it's included as an argument here to allow the
user to modify the dictionary before passing to \code{generate_queries} (e.g. to
correct bugs in branching logic). If passing a modified version, make sure
it is initially fetched with argument \code{expand_checkbox = FALSE}.}

\item{lang}{Query language, either English ("en") or French ("fr"). Defaults
to "en".}

\item{query_types}{Which type of queries to generate (see \strong{Description}
above). Options are "missing", "not missing", or "both". Defaults to
"both".}

\item{non_required}{Logical indicating whether to include non-required fields
in queries of type "Field missing". Defaults to \code{FALSE}.}

\item{drop_redundant}{Logical indicating whether to simplify expressions by
removing redundant components from expressions that test both for equality
and inequality with the same variable. E.g.:\cr
\code{var == "X" & var != ""} becomes \code{var == "X"}}

\item{field_nchar_max}{Integer indicating the maximum number of characters to
allow in field name labels before they are truncated and appended with
"...". Defaults to 80L.}

\item{on_error}{What to do if one or more elements of statements can't be
translated into valid R expressions? Options are "ignore" (return \code{NA} for
relevant elements), "warn" (return \code{NA} for relevant elements and give
warning), or "fail" (throw error). Defaults to "warn".}
}
\value{
A \code{\link[tibble:tbl_df-class]{tibble}}-style data frame specifying queries, with the
following 7 columns:

\describe{
\item{query_id}{Unique query identifier based on form name and integer sequence}
\item{field_name}{Field name (from REDCap dictionary, see \code{\link{meta_dictionary}})}
\item{form_name}{Form name (from REDCap dictionary)}
\item{required}{Is it a required field in REDCap dictionary ("y" or \verb{<NA>}) ?}
\item{description}{Description of query (e.g. "Missing: [Signed consent forms?]")}
\item{suggestion}{Suggestion for query resolution. A human-readable
translation of query expression (e.g. If [Is the participant 18 years or
older?] is "Yes", item [Signed consent forms?] should not be missing)}
\item{branching_logic}{Branching logic for given field (from REDCap dictionary)}
\item{query}{R-style query expression (can be evaluated with \code{\link[queryr:query]{queryr::query}})}
}
}
\description{
Generates two types of data validation queries using the project codebook
(see \code{\link{meta_dictionary}}) and \code{\link{translate_logic}}:
\enumerate{
\item \strong{Field missing}: Branching logic evaluates to \code{TRUE} (if specified), but
field is missing. By default only applies to required fields (\code{required_field == "y"}) (can modify with argument \code{non_required}).
\item \strong{Field not missing}: Branching logic evaluates to \code{FALSE} but field is
not missing. Applies to any field with branching logic.
}
}
