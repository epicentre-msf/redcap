context("meta_forms")

test_that("meta_forms works as expected", {

  skip_if(conn_test$token == "")

  m1 <- meta_forms(conn_test)
  expect_is(m1, "tbl_df")
  expect_equal(names(m1), c("instrument_name", "instrument_label"))

})


test_that("meta_forms fails gracefully", {

  expect_error(meta_forms(conn_fake))
})

