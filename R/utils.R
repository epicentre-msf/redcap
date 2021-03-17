

#' Convert REDCap time variables to chron 'times' class
#'
#' @noRd
#' @importFrom chron times
parse_redcap_time <- function(x) {
  x <- gsub("(^\\d{2}:\\d{2}$)", "\\1:00", x)
  chron::times(x, format = c(times = "h:m:s"))
}


#' Wrapper to dplyr::recode
#'
#' @noRd
#'
#' @importFrom stats setNames
#' @importFrom dplyr recode
#' @importFrom rlang `!!!`
recode_vec <- function(x, from, to) {
  vec_recode <- stats::setNames(to, from)
  dplyr::recode(x, !!!vec_recode)
}


#' Enclose a string in a special character (e.g. brackets, backticks), if not
#' already enclosed
#'
#' @param x A string to enclose
#' @param l Character symbol to add on left side
#' @param r Character symbol to add on right
#'
#' @noRd
enclose <- function(x, l, r) {
  reg_l <- paste0("^\\", l)
  reg_r <- paste0("\\", r, "$")
  already_enclosed <- (grepl(reg_l, x) & grepl(reg_r, x)) | is.na(x)
  x[!already_enclosed] <- paste0(l, x[!already_enclosed], r)
  x
}


#' Remove enclosing backticks
#'
#' @noRd
untick <- function(x) {
   gsub("^\\`|\\`$", "", x)
}


#' Remove enclosing parentheses (if only 1 set)
#'
#' @noRd
unparens <- function(x) {
  single_set_parens <- grepl("^\\(", x) & string_count(x, "\\(") == 1 &
    grepl("\\)$", x) & string_count(x, "\\)") == 1
  x[single_set_parens] <- gsub("^\\(|\\)$", "", x[single_set_parens])
  x
}


#' Quick version of stringr::str_squish to avoid dependency
#'
#' @noRd
string_squish <- function(x) {
  x <- gsub("[[:space:]]+", " ", x)
  x <- gsub("^[[:space:]]|[[:space:]]$", "", x)
  x
}


#' Quick version of stringr::str_count to avoid dependency
#'
#' @noRd
string_count <- function(string, pattern) {
  lengths(regmatches(string, gregexpr(pattern, string)))
}


#' @noRd
test_valid <- function(arg, options) {
  arg_name <- deparse(substitute(arg))
  non_matching <- setdiff(arg, options)

  if (length(non_matching) > 0) {
    stop(
      "The following ", arg_name, " were not found: ",
      paste(non_matching, collapse = "; "),
      call. = FALSE
    )
  }
}


#' @noRd
#' @importFrom httr POST content
#' @importFrom readr cols col_character
post_wrapper <- function(conn,
                         body = NULL,
                         content = NULL,
                         na = c("", "NA"),
                         on_error = "fail") {

  if (!is_rconn(conn)) {
    stop("Argument conn must be a REDCap connection created with rconn()")
  }

  on_error <- match.arg(on_error, c("fail", "null"))

  if (is.null(body)) {
    body <- list(
      token = conn$token,
      content = content,
      format = "csv",
      returnFormat = "json"
    )
  }

  response <- httr::POST(
    conn$url,
    body = body,
    config = conn$config,
    encode = "form"
  )

  if (response$status_code != 200L) {
    if (on_error == "fail") {
      stop(httr::content(response)[[1]], call. = FALSE)
    } else {
      out <- NULL
    }
  } else {
    out <- httr::content(
      response,
      col_types = readr::cols(.default = readr::col_character()),
      na = na
    )
  }

  out
}


#' @noRd
split_choices <- function(x) {
  strsplit(x, "[[:space:]]*\\|[[:space:]]*")[[1]]
}


#' @noRd
#' @importFrom stringr str_split
split_value_label <- function(x, i) {
  # strsplit(x, "\\,[[:space:]]*")[[1]][i]
  stringr::str_split(x, "\\,[[:space:]]*", n = 2)[[1]][i]
}

