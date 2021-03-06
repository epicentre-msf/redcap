% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reclass.R
\name{reclass}
\alias{reclass}
\title{Reclass columns of a data frame to match classes specified in a metadata
dictionary}
\usage{
reclass(
  x,
  dict,
  use_factors = FALSE,
  value_labs = TRUE,
  header_labs = FALSE,
  times_chron = TRUE
)
}
\arguments{
\item{x}{A data frame representing a REDCap form}

\item{dict}{A metadata dictionary}

\item{use_factors}{Logical indicating whether categorical REDCap variables
(radio, dropdown, yesno, checkbox) should be returned as factors. Factor
levels can either be raw values (e.g. "0"/"1") or labels (e.g. "No"/"Yes")
depending on arguments \code{value_labs} and \code{checkbox_labs}. Defaults to
\code{FALSE}.}

\item{value_labs}{Logical indicating whether to return value labels (\code{TRUE})
or raw values (\code{FALSE}) for categorical REDCap variables (radio, dropdown,
yesno, checkbox). Defaults to \code{TRUE} to return labels.}

\item{header_labs}{Logical indicating whether to export column names as
labels (\code{TRUE}) or raw variable names (\code{FALSE}). Defaults to \code{FALSE} to
return raw variable names.}

\item{times_chron}{Logical indicating whether to reclass time variables using
\link[chron:dates]{chron::times} (\code{TRUE}) or leave as character hh:mm format (\code{FALSE}).
Defaults to \code{TRUE}.}
}
\description{
Reclass columns of a data frame to match classes specified in a metadata
dictionary
}
