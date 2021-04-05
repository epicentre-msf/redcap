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

  m3 <- meta_factors(conn_test, add_complete = TRUE)
  vars_complete <- paste0(meta_forms(conn_test)[[1]], "_complete")
  expect_true(all(vars_complete %in% m3$field_name))
  expect_setequal(m3$value[m3$field_name == vars_complete[1]], c("0", "1", "2"))
  expect_setequal(m3$label[m3$field_name == vars_complete[1]], c("Incomplete", "Unverified", "Complete"))
})


test_that("meta_events fails gracefully", {

  expect_error(meta_dictionary(conn_fake))
})
