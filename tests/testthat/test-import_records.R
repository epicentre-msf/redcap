context("import_records and delete_records")

test_that("import_records and delete_records work as expected", {

  # skip_if(conn_test$token == "")

  # data to import
  dat_test <- data.frame(
    record_id = c("9901", "9902"),
    redcap_event_name = c("enrollment_arm_1", "enrollment_arm_1"),
    enr_form_dt = c("2021-01-05 14:55:10", "2021-01-06 09:13:52"),
    enr_last_name = c("Mitterrand", "Chirac"),
    enr_over_18 = c("1", "1"),
    enr_signed = c("0", "1"),
    enr_signed_date = as.Date(c("2021-01-05", "2021-01-06")),
    stringsAsFactors = FALSE
  )

  # return count
  import1 <- import_records(
    conn = conn_test,
    data = dat_test
  )

  delete1 <- delete_records(conn_test, c("9901", "9902"))

  expect_equal(import1, 2L)
  expect_equal(delete1, 2L)

  # return vector of IDs
  import2 <- import_records(
    conn = conn_test,
    data = dat_test,
    return = "ids"
  )

  expect_equal(import2, c("9901", "9902"))

  # clean-up
  delete_records(conn_test, c("9901", "9902"))
})



test_that("import_records fails gracefully", {

  # skip_if(conn_test$token == "")

  dat_nonvalid <- data.frame(
    record_id = c("9903", "9904"),
    redcap_event_name = c("enrollment_arm_1", "enrollment_arm_1"),
    enr_over_18 = c("3", "1"), # non-valid level "3"
    enr_signed = c("0", "1"),
    enr_signed_date = as.Date(c("2021-01-05", "2021-01-06")),
    stringsAsFactors = FALSE
  )

  expect_error(import_records(conn_test, data = dat_nonvalid))
})
