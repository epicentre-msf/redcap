context("project_logging")

test_that("project_logging works as expected", {

  log_test <- project_logging(conn_test, time_end = "2021-04-01 00:00")
  dict_test <- meta_dictionary(conn_test, add_complete = TRUE)

  x1 <- parse_logging(log_test)
  expect_s3_class(x1, "tbl_df")

  expect_type(x1$rowid, "integer")
  expect_type(x1$redcap_repeat_instance, "integer")
  expect_true(all(grepl("(Create|Delete|Update)(d)? Record", x1$action, ignore.case = TRUE)))

  x2 <- parse_logging(log_test, format_long = TRUE, dict = dict_test)
  expect_true(all(c("form_name", "field_name", "value") %in% names(x2)))
  expect_true(all(x2$form_name %in% c(dict_test$form_name, NA)))
  # expect no filtering when format_long = TRUE
  expect_setequal(x2$rowid, x1$rowid)
  expect_setequal(x2$record_id, x1$record_id)

})


test_that("project_logging fails gracefully", {

  expect_error(parse_logging(log_test, format_long = TRUE))
})
