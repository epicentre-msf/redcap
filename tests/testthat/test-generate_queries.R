context("generate_queries")

test_that("generate_queries works as expected", {
  skip_if(conn_test$token == "")

  q1 <- generate_queries(conn_test, query_types = "both")
  expect_is(q1, "tbl_df")
  expect_setequal(q1$form_name, meta_forms(conn_test)[[1]])

  # test arg forms
  q2 <- generate_queries(conn_test, forms = "eligibility")
  expect_setequal(q2$form_name, "eligibility")

  # test arg query_types
  q1_types <- vapply(q1$description, function(x) strsplit(x, "\\:")[[1]][1], "")
  expect_setequal(q1_types, c("Missing", "Not missing"))

  q3 <- generate_queries(conn_test, query_types = "missing")
  expect_true(all(grepl("^Missing", q3$description)))

  q4 <- generate_queries(conn_test, query_types = "not missing")
  expect_true(all(grepl("^Not missing", q4$description)))
})


test_that("generate_queries fails gracefully", {
  expect_error(generate_queries(conn_fake))
  skip_if(conn_test$token == "")
  expect_error(generate_queries(conn_test, forms = "blah_blah"))
})


test_that("range_queries = FALSE (default) produces no 'Out of range' descriptions", {
  skip_if(conn_test$token == "")

  q <- generate_queries(conn_test, query_types = "both")
  expect_false(any(grepl("^Out of range", q$description)))
})


test_that("range_queries = TRUE produces 'Out of range' descriptions when bounded fields exist", {
  skip_if(conn_test$token == "")

  dict <- meta_dictionary(conn_test, expand_checkbox = FALSE)

  has_bounds <- any(
    dict$field_type %in%
      "text" &
      dict$validation %in%
        c(
          "integer",
          "number",
          "number_1dp",
          "number_2dp",
          "number_3dp",
          "number_4dp",
          "date_dmy",
          "date_mdy",
          "date_ymd",
          "datetime_dmy",
          "datetime_mdy",
          "datetime_ymd",
          "datetime_seconds_dmy",
          "datetime_seconds_mdy",
          "datetime_seconds_ymd"
        ) &
      (!is.na(dict$validation_min) | !is.na(dict$validation_max))
  )

  q <- generate_queries(conn_test, range_queries = TRUE)

  if (has_bounds) {
    expect_true(any(grepl("^Out of range", q$description)))
    # All range query strings begin with !is.na(
    range_rows <- grepl("^Out of range", q$description)
    expect_true(all(grepl("^!is\\.na\\(", q$query[range_rows])))
  } else {
    expect_false(any(grepl("^Out of range", q$description)))
  }
})


test_that("range_queries = TRUE uses as.numeric() for numeric validation fields", {
  skip_if(conn_test$token == "")

  dict <- meta_dictionary(conn_test, expand_checkbox = FALSE)

  numeric_validations <- c(
    "integer",
    "number",
    "number_1dp",
    "number_2dp",
    "number_3dp",
    "number_4dp"
  )

  has_numeric_bounds <- any(
    dict$field_type %in%
      "text" &
      dict$validation %in% numeric_validations &
      (!is.na(dict$validation_min) | !is.na(dict$validation_max))
  )

  skip_if(!has_numeric_bounds)

  q <- generate_queries(conn_test, range_queries = TRUE)
  range_rows <- grepl("^Out of range", q$description)
  numeric_queries <- q$query[range_rows & grepl("as\\.numeric\\(", q$query)]
  expect_true(length(numeric_queries) > 0)
})


test_that("range_queries = TRUE with lang = 'fr' produces 'Hors limites' descriptions", {
  skip_if(conn_test$token == "")

  dict <- meta_dictionary(conn_test, expand_checkbox = FALSE)

  has_bounds <- any(
    dict$field_type %in% "text" & (!is.na(dict$validation_min) | !is.na(dict$validation_max))
  )

  skip_if(!has_bounds)

  q <- generate_queries(conn_test, range_queries = TRUE, lang = "fr")
  range_rows <- grepl("^Hors limites", q$description)
  expect_true(any(range_rows))
})
