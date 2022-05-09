context("recode_labels")

test_that("recode_labels works as expected", {

  # skip_if(conn_test$token == "")

  df_factors <- meta_factors(conn_test, add_complete = TRUE)

  x1 <- fetch_records(conn_test, forms = "enrolment", value_labs = TRUE)
  x2 <- fetch_records(conn_test, forms = "enrolment", value_labs = FALSE)
  x3 <- fetch_records(conn_test, forms = "enrolment", value_labs = FALSE, header_labs = TRUE)

  # labels to labels
  x1a <- recode_labels(
    x1,
    conn_test,
    convert_to = "labels"
  )

  expect_equal(x1a, x1)

  # labels to values
  x1b <- recode_labels(
    x1,
    conn_test,
    convert_to = "values"
  )

  expect_setequal(x1b$redcap_event_name, "enrollment_arm_1")
  expect_true(all(x1b$enr_over_18 %in% df_factors$value[df_factors$field_name == "enr_over_18"]))
  expect_true(all(x1b$enrolment_complete %in% df_factors$value[df_factors$field_name == "enrolment_complete"]))

  # values to labels
  x2a <- recode_labels(
    x2,
    conn_test,
    convert_to = "labels"
  )

  expect_setequal(x2a$redcap_event_name, "Enrollment")
  expect_true(all(x2a$enr_over_18 %in% df_factors$label[df_factors$field_name == "enr_over_18"]))
  expect_true(all(x2a$enrolment_complete %in% df_factors$label[df_factors$field_name == "enrolment_complete"]))

  # values to values
  x2b <- recode_labels(
    x2,
    conn_test,
    convert_to = "values"
  )

  expect_equal(x2b, x2)

  # values to labels, with header_labs = TRUE
  x3a <- recode_labels(
    x3,
    conn_test,
    convert_to = "labels",
    header_labs = TRUE
  )

  expect_setequal(x3a[["Event Name"]], "Enrollment")
  expect_true(all(x3a[["Is the participant 18 years or older?"]] %in% df_factors$label[df_factors$field_name == "enr_over_18"]))
  expect_true(all(x3a[["Complete?"]] %in% df_factors$label[df_factors$field_name == "enrolment_complete"]))
})
