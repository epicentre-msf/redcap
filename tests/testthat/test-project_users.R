context("project_users")

test_that("project_users works as expected", {

  skip_if(conn_test$token == "")

  m1 <- project_users(conn_test)
  expect_is(m1, "tbl_df")
  expect_true(all(c("username", "email", "expiration", "data_access_group") %in% names(m1)))

})


test_that("project_users fails gracefully", {

  expect_error(project_users(conn_fake))
})

