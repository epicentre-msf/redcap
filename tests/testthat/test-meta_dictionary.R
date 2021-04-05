context("meta_dictionary")

test_that("meta_dictionary works as expected", {

  skip_if(conn_test$token == "")

  check_field <- "elig_medical"
  check_checkbox <- paste(check_field, c(1:7, 88), sep = "___")

  m1 <- meta_dictionary(conn_test, expand_checkbox = TRUE)
  expect_is(m1, "tbl_df")
  expect_true(!check_field %in% m1$field_name)
  expect_true(all(check_checkbox %in% m1$field_name))

  m2 <- meta_dictionary(conn_test, expand_checkbox = FALSE)
  expect_true(check_field %in% m2$field_name)
  expect_true(!any(check_checkbox %in% m2$field_name))

  m3 <- meta_dictionary(conn_test, forms = "eligibility")
  expect_setequal(m3$form_name, "eligibility")

  # test arg add_complete
  m4 <- meta_dictionary(conn_test, forms = "eligibility", add_complete = TRUE)
  expect_equal(
    m4$choices[m4$field_name %in% "eligibility_complete"],
    "0, Incomplete | 1, Unverified | 2, Complete"
  )
})


test_that("meta_events fails gracefully", {

  expect_error(meta_dictionary(conn_fake))
})
