context("meta_mapping")

test_that("meta_mapping works as expected", {

  skip_if(conn_test$token == "")

  m1 <- meta_mapping(conn_test)
  expect_is(m1, "tbl_df")
  expect_equal(names(m1), c("arm_num", "unique_event_name", "form"))
})


test_that("meta_mapping fails gracefully", {

  expect_null(meta_mapping(conn_fake, on_error = "null"))
  expect_error(meta_mapping(conn_fake, on_error = "fail"))
})

