context("meta_repeating")

test_that("meta_repeating works as expected", {

  skip_if(conn_test$token == "")

  m1 <- meta_repeating(conn_test)
  expect_is(m1, "tbl_df")
  expect_true(all(c("form_name", "custom_form_label") %in% names(m1)))

})


test_that("meta_repeating fails gracefully", {

  expect_null(meta_repeating(conn_fake, on_error = "null"))
  expect_error(meta_repeating(conn_fake, on_error = "fail"))
})

