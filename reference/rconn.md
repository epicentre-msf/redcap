# Connection to a REDCap Database

Creates an object of class "rconn" containing the URL and token used to
access a REDCap project API.

## Usage

``` r
rconn(url, token, config = httr::config())
```

## Arguments

- url:

  URL for a REDCap database API

- token:

  REDCap project API token (good practice to set using an environmental
  variable, e.g. with
  [`Sys.getenv`](https://rdrr.io/r/base/Sys.getenv.html)).

- config:

  Optional configuration settings passed to
  [`httr::POST`](https://httr.r-lib.org/reference/POST.html). Defaults
  to [`httr::config()`](https://httr.r-lib.org/reference/config.html).

## Value

An object of class "rconn", to be passed as the first argument to most
other `redcap` functions.

## Examples

``` r
conn <- rconn(
  url = "https://redcap.msf.fr/api/",
  token = Sys.getenv("MY_REDCAP_TOKEN")
)
```
