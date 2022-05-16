context("meta_arms")

test_that("meta_arms works as expected", {

  skip_if(conn_test$token == "")

  m1 <- meta_arms(conn_test)
  expect_is(m1, "tbl_df")
  expect_equal(names(m1), c("arm_num", "name"))

})


test_that("meta_arms fails gracefully", {

  expect_error(meta_arms(conn_fake))
})

