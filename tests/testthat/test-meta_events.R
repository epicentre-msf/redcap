context("meta_events")

test_that("meta_events works as expected", {

  skip_if(conn_test$token == "")

  m1 <- meta_events(conn_test)
  expect_is(m1, "tbl_df")
  expect_true(all(c("event_name", "unique_event_name") %in% names(m1)))

})


test_that("meta_events fails gracefully", {

  expect_null(meta_events(conn_fake, on_error = "null"))
  expect_error(meta_events(conn_fake, on_error = "fail"))
})
