#' Connection to a REDCap Database
#'
#' Creates an object of class "rconn" containing the URL and token used to
#' access a REDCap project API.
#'
#' @param url URL for a REDCap database API
#' @param token REDCap project API token (good practice to set using an
#'   environmental variable, e.g. with [`Sys.getenv`]).
#' @param config Optional configuration settings passed to [`httr::POST`].
#'   Defaults to [`httr::config()`].
#'
#' @return
#' An object of class "rconn", to be passed as the first argument to most other
#' `redcap` functions.
#'
#' @examples
#' conn <- rconn(
#'   url = "https://redcap.msf.fr/api/",
#'   token = Sys.getenv("MY_REDCAP_TOKEN")
#' )
#'
#' @importFrom httr config
#' @export rconn
rconn <- function(url,
                  token,
                  config = httr::config()) {

  if (missing(url) | is.null(url) | is.na(url)) {
    stop("Argument url must be provided")
  }
  if (missing(token) | is.null(token) | is.na(token)) {
    stop("Argument token must be provided")
  }

  structure(
    list(url = url, token = token, config = config),
    class = "rconn"
  )
}


#' @noRd
is_rconn <- function(x) {
  class(x) %in% "rconn"
}

