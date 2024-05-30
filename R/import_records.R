#' Import records into a REDCap project
#'
#' @inheritParams fetch_records
#'
#' @param data A data.frame containing record data to import into REDCap
#' @param type One of:
#'  - "flat": data in wide-form with one record per row (default)
#'  - "eav": data in long-form with one row per participant/instance/field (data
#'  should have columns "record", "field_name", and "value", and if longitudinal
#'  then also "redcap_event_name" and "redcap_repeat_instance")
#' @param overwrite Overwrite behaviour. Either "normal" to prevent missing
#'   values from overwriting data, or "overwrite" to allow data to be
#'   overwritten with missing values. Defaults to "normal".
#' @param return What to return. Use "count" to return a count of imported
#'   records, "ids" to return a vector of the IDs that were imported, or
#'   "nothing" to return nothing. Defaults to "count".
#'
#' @return
#' Depends on argument `return`. Either a count of imported records, a vector of
#' record IDs, or nothing.
#'
#' @importFrom httr POST
#' @importFrom dplyr anti_join all_of
#' @importFrom tidyr pivot_longer
#' @export import_records
import_records <- function(conn,
                           data,
                           type = c("flat", "eav"),
                           overwrite = "normal",
                           return = "count") {


  ## fetch metadata (dictionary, factors) --------------------------------------
  m_dict <- meta_dictionary(conn)
  m_fact <- meta_factors(conn)
  m_forms <- meta_forms(conn)


  ## argument validation -------------------------------------------------------
  type <- match.arg(type, c("flat", "eav"))
  overwrite <- match.arg(overwrite, c("normal", "overwrite"))
  return <- match.arg(return, c("count", "ids", "nothin"))

  if (type == "flat") {

    # fields
    fields_meta <- c(
      "redcap_event_name",
      "redcap_repeat_instrument",
      "redcap_repeat_instance",
      "redcap_data_access_group",
      paste0(m_forms$instrument_name, "_complete")
    )

    test_valid(names(data), "column names", c(m_dict$field_name, fields_meta))

    # ID field
    name_id_field <- m_dict$field_name[1]

    if (!name_id_field %in% names(data)) {
      stop("data must contain record ID field: ", name_id_field, call. = FALSE)
    }

    ## validate factors ----------------------------------------------------------
    if (any(m_fact$field_name %in% names(data))) {

      cols_long <- c(name_id_field, m_fact$field_name)

      data_long <- tidyr::pivot_longer(
        cols_to_chr(data[,names(data) %in% cols_long, drop = FALSE]),
        cols = !all_of(name_id_field)
      )

      nonvalid_factor <- dplyr::anti_join(
        data_long[!is.na(data_long$value), , drop = FALSE],
        m_fact,
        by = c("name" = "field_name", "value" = "value")
      )

      if (nrow(nonvalid_factor) > 0) {
        stop(
          "Non-valid levels of factor-type variables:\n",
          print_and_capture(nonvalid_factor),
          call. = FALSE
        )
      }
    }

    ## reclass -------------------------------------------------------------------
    cols_1dp <- m_dict$field_name[m_dict$validation %in% "number_1dp"]
    cols_1dp <- intersect(cols_1dp, names(data))
    data <- cols_reclass(data, cols_1dp, format_1dp)

    cols_2dp <- m_dict$field_name[m_dict$validation %in% "number_2dp"]
    cols_2dp <- intersect(cols_2dp, names(data))
    data <- cols_reclass(data, cols_2dp, format_2dp)
  }


  ## prepare request body ------------------------------------------------------
  body <- list(
    token = conn$token,
    content = "record",
    format = "csv",
    type = type,
    csvDelimiter = "",
    overwriteBehavior = overwrite,
    returnContent = return,
    returnFormat = "csv",
    dateFormat = "YMD",
    data = data_frame_to_string(data)
  )

  ## request -------------------------------------------------------------------
  x <- httr::POST(
    url = conn$url,
    body = body,
    config = conn$config
  )

  if (x$status_code != "200") {
    return(as.character(x))
  } else if (return == "count") {
    return(as.integer(as.character(x)))
  } else if (return == "ids") {
    return(strsplit(as.character(x), "\n")[[1]][-1])
  }
}


#' @noRd
#' @importFrom utils capture.output write.table
data_frame_to_string <- function(data) {
  paste0(
    utils::capture.output(
      utils::write.table(
        data,
        sep = ",",
        na = "",
        col.names = TRUE,
        qmethod = "double",
        row.names = FALSE
      )
    ),
    collapse = "\n"
  )
}


#' @noRd
cols_to_chr <- function(x) {
  for (j in seq_len(ncol(x))) x[[j]] <- as.character(x[[j]])
  x
}


#' @noRd
#' @importFrom utils capture.output
print_and_capture <- function(x) {
  # useful for adding a data.frame to a message() or warning()
  if ("tbl" %in% class(x)) x <- as.data.frame(x)
  paste(utils::capture.output(print(x)), collapse = "\n")
}

