context("project_users_dags")

test_that("project_users_dags works as expected", {

  skip_if(conn_test$token == "")

  m1 <- project_users_dags(conn_test)
  expect_is(m1, "tbl_df")
  expect_equal(names(m1), c("username", "redcap_data_access_group"))

})


test_that("project_users_dags fails gracefully", {

  expect_error(project_users_dags(conn_fake))
})

