context("meta_factors")

test_that("meta_factors works as expected", {

  skip_if(conn_test$token == "")

  check_field <- "elig_medical"
  check_checkbox <- paste(check_field, c(1:7, 88), sep = "___")

  m1 <- meta_factors(conn_test)
  expect_is(m1, "tbl_df")
  expect_true(!check_field %in% m1$field_name)
  expect_true(all(check_checkbox %in% m1$field_name))

  m2 <- meta_factors(conn_test, forms = "enrolment")
  expect_setequal(m2$form_name, "enrolment")
})


test_that("meta_events fails gracefully", {

  expect_error(meta_dictionary(conn_fake))
})
